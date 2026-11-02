#!/usr/bin/env python3
"""Flag list-only functions applied to a whole Canvas API reply.

`org-canvas-api-request` decodes the reply with `json-read', so a JSON
array arrives as a *vector*: `car', `nth', `last', `append', `mapcar'
and `dolist' signal "wrong type argument" on it.  The mocked harness in
test/test-helper.el now hands a stub's quoted list back as the vector
Canvas would send (issue #625), which is what catches such code, but a
spec that mocks `org-canvas-api-request' itself with `cl-letf' still
bypasses the harness.  This check is the coarse net for those: it reads
the source, not the sexps, so it is a guard rail rather than a proof.

    scripts/api-reply-list-check.py            check lisp/, write nothing
    scripts/api-reply-list-check.py DIR        check DIR instead
    scripts/api-reply-list-check.py --self-test
                                               prove the check catches a
                                               violation and passes a clean
                                               file

`org-canvas-api-request-all-pages' returns a list by design (it appends
every page), so its calls are left alone; only the single-reply
function is checked.
"""

import pathlib
import re
import sys
import tempfile

# Functions that only accept a list.  `length', `seq-*', `aref',
# `mapconcat' and friends work on vectors too, so they are absent.
# `append' is absent because `(append (org-canvas-api-request ...) nil)'
# is the fix -- it copies the vector into a list -- not the bug.
LIST_ONLY = (
    "car", "cdr", "caar", "cadr", "cdar", "cddr",
    "nth", "nthcdr", "last", "butlast",
    "mapcar", "mapc", "dolist",
)

# `(fn (org-canvas-api-request` with whitespace after the name, which
# keeps `org-canvas-api-request-all-pages' out.  The window lets the
# opening call sit a line or two above, as it usually does.
CALL = re.compile(r"\(\s*org-canvas-api-request\s")
WRAPPER = re.compile(
    r"\(\s*(?:" + "|".join(LIST_ONLY) + r")\s+\Z",
    re.DOTALL,
)
WINDOW = 200


def strip_comments_and_strings(source):
    """Blank out comments and string literals, keeping offsets intact."""
    out = []
    i = 0
    while i < len(source):
        char = source[i]
        if char == ";":
            end = source.find("\n", i)
            end = len(source) if end < 0 else end
            out.append(" " * (end - i))
            i = end
        elif char == '"':
            end = i + 1
            while end < len(source):
                if source[end] == "\\":
                    end += 2
                elif source[end] == '"':
                    end += 1
                    break
                else:
                    end += 1
            out.append(" " * (end - i))
            i = end
        else:
            out.append(char)
            i += 1
    return "".join(out)


def findings(path):
    """Return (line, snippet) for each call reached through a list-only fn."""
    source = path.read_text(encoding="utf-8")
    clean = strip_comments_and_strings(source)
    found = []
    for match in CALL.finditer(clean):
        before = clean[max(0, match.start() - WINDOW):match.start()]
        if WRAPPER.search(before):
            line = clean.count("\n", 0, match.start()) + 1
            snippet = source.splitlines()[line - 1].strip()
            found.append((line, snippet))
    return found


def check(directory):
    """Check every lisp/ file under DIRECTORY; return the exit status."""
    files = sorted(directory.glob("*.el"))
    if not files:
        print(f"api-reply-list-check: no .el files under {directory}")
        return 1
    failures = 0
    for path in files:
        for line, snippet in findings(path):
            failures += 1
            print(f"{path}:{line}: list-only function on an API reply: {snippet}")
    if failures:
        print(
            "api-reply-list-check: a Canvas array is a vector; iterate it with "
            "`seq-doseq'/'aref' or bind it with `append' first."
        )
        return 1
    print(f"api-reply-list-check: {len(files)} files clean")
    return 0


def self_test():
    """Prove the check flags a violation and passes a clean file."""
    bad = """
(defun bad ()
  (car (org-canvas-api-request 'GET "items")))
"""
    good = """
(defun good ()
  (seq-elt (org-canvas-api-request 'GET "items") 0))
(defun fine ()
  (append (org-canvas-api-request-all-pages 'GET "items") nil))
(defun also-fine ()
  (append (org-canvas-api-request 'GET "items") nil))
(defun multi-line ()
  (car
   (org-canvas-hidden-call)))
"""
    with tempfile.TemporaryDirectory() as tmp:
        directory = pathlib.Path(tmp)
        (directory / "bad.el").write_text(bad, encoding="utf-8")
        (directory / "good.el").write_text(good, encoding="utf-8")
        bad_found = findings(directory / "bad.el")
        good_found = findings(directory / "good.el")
    if not bad_found:
        print("api-reply-list-check: self-test failed to flag `car' on a reply")
        return 1
    if good_found:
        print(f"api-reply-list-check: self-test flagged clean code: {good_found}")
        return 1
    print("api-reply-list-check: self-test passed")
    return 0


def main(argv):
    """Run the check, the self-test, or the usage error."""
    if "--self-test" in argv:
        return self_test()
    root = pathlib.Path(__file__).resolve().parent.parent
    directory = root / "lisp"
    if len(argv) > 1 and not argv[1].startswith("-"):
        directory = pathlib.Path(argv[1])
    return check(directory)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
