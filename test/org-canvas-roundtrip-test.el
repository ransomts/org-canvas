;;; org-canvas-roundtrip-test.el --- Pull idempotence / round-trip guards  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Round-trip idempotence guards: applying the same Canvas response to a
;; heading twice must leave the buffer byte-identical.  This directly guards
;; against the scariest failure mode — a pull silently corrupting an .org
;; file on re-sync (duplicated bodies, churned properties, drifting
;; timestamps).  A non-idempotent pull means each sync mutates the source of
;; truth a little more.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas)
(require 'org-canvas-assignments)
(require 'org-canvas-assignment-groups)
(require 'org-canvas-rubrics)
(require 'org-canvas-pages)

(defun org-canvas-roundtrip--pull-twice (initial pull-fn response)
  "Apply PULL-FN/RESPONSE to INITIAL's first heading twice.
Returns a cons (FIRST . SECOND) of the buffer string after each pull.
Paged list sub-fetches (e.g. assignment overrides) are mocked to nil
so no pull-item reaches the network guard."
  (let ((tmp (make-temp-file "roundtrip-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert initial))
          (with-current-buffer (find-file-noselect tmp)
            (unwind-protect
                (with-html-to-org-identity
                  (cl-letf (((symbol-function 'org-canvas-api-request-all-pages)
                             (lambda (&rest _) nil)))
                  (let ((org-canvas-course-id "99999")
                        (org-canvas-base-url "https://test.canvas.example.com")
                        (org-canvas-api-token "test-token")
                        first second)
                    (goto-char (point-min))
                    (re-search-forward "^\\* ")
                    (org-back-to-heading)
                    (funcall pull-fn response (point))
                    (setq first (buffer-string))
                    (goto-char (point-min))
                    (re-search-forward "^\\* ")
                    (org-back-to-heading)
                    (funcall pull-fn response (point))
                    (setq second (buffer-string))
                    (cons first second))))
              (kill-buffer))))
      (delete-file tmp))))

(describe "pull idempotence (round-trip guard)"
  (it "assignment pull is idempotent and populates the heading"
    (let* ((initial "* Essay\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n")
           (response '((id . 1) (name . "Essay") (points_possible . 50)
                       (due_at . "2026-09-01T10:00:00Z") (published . t)))
           (r (org-canvas-roundtrip--pull-twice
               initial #'org-canvas--assignment-pull-item response)))
      (expect (car r) :to-match "POINTS")        ; pull actually did something
      (expect (cdr r) :to-equal (car r))))       ; second pull changed nothing

  (it "assignment-group pull is idempotent"
    (let* ((initial "* Homework\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n")
           (response '((id . 1) (name . "Homework") (group_weight . 30)
                       (position . 2)
                       (rules . ((drop_lowest . 1)))))
           (r (org-canvas-roundtrip--pull-twice
               initial #'org-canvas--assignment-group-pull-item response)))
      (expect (car r) :to-match "WEIGHT")
      (expect (cdr r) :to-equal (car r))))

  (it "rubric pull is idempotent"
    (let* ((initial "* Essay Rubric\n:PROPERTIES:\n:CANVAS_ID: 1\n:END:\n")
           (response '((id . 1) (title . "Essay Rubric")
                       (data . (((description . "Thesis") (points . 20))
                                ((description . "Clarity") (points . 10))))))
           (r (org-canvas-roundtrip--pull-twice
               initial #'org-canvas--rubric-pull-item response)))
      (expect (cdr r) :to-equal (car r))))

  (it "page pull with body is idempotent (no body duplication on re-pull)"
    ;; Pages fetch detail and insert the body — the classic duplication risk.
    (with-org-canvas-test-config
      (with-mock-api
        (setq test-org-canvas-api-responses
              '(("pages/welcome" . ((page_id . 5)
                                     (body . "<p>Hello class.</p>")))))
        (let* ((initial (concat "* Welcome\n:PROPERTIES:\n"
                                ":CANVAS_URL: welcome\n:END:\n"))
               (response '((url . "welcome") (page_id . 5) (title . "Welcome")))
               (r (org-canvas-roundtrip--pull-twice
                   initial #'org-canvas--page-pull-item response)))
          (expect (car r) :to-match "Hello class\\.")
          (expect (cdr r) :to-equal (car r)))))))

(provide 'org-canvas-roundtrip-test)
;;; org-canvas-roundtrip-test.el ends here
