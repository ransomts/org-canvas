# Security Policy

org-canvas holds a Canvas API token and writes to a live course on your
behalf. A bug in how it handles either is a security bug. This page says
what counts, how to report it, and what to do if a token has already
leaked.

## Reporting a vulnerability

Report privately through GitHub's advisory form:

https://github.com/ransomts/org-canvas/security/advisories/new

Do not open a public issue for anything that could expose a token, a
session cookie, or student data. This is a single-maintainer project;
expect an acknowledgement within a week and a fix or a plan within a
month for anything confirmed.

Include what you would put in a bug report (see the bug template): the
org-canvas commit, the Emacs version, the command you ran, and the
smallest reproduction you have. **Never include your token, session
cookies, or a course's student data.** If a log line or a backtrace is
needed, redact it by hand before pasting; `Bearer 1215~...` style
values are the ones to look for.

## What is in scope

- A token, session cookie or CSRF value reaching anywhere a person can
  read it: the `*org-canvas-log*` buffer or log file, the echo area and
  `*Messages*`, batch stderr, a backtrace, a curl config or temp file,
  a report buffer, or the pull summary. The package redacts on every
  path it knows about (`org-canvas--log-redact`,
  `org-canvas--user-message`, the transport boundary of #178); a path
  it missed is a bug.
- A write reaching Canvas that should not have: while
  `org-canvas-read-only` is set, during a dry run, or a delete that was
  not confirmed. `org-canvas--check-writable` refuses non-GET requests
  at the transport, and every custom sync loop must honour
  `org-canvas--dry-run` itself; a request that slips past either is a
  bug.
- Content that org-canvas writes into a course which the Org file did
  not say: link rewriting, HTML export, or body handling that could
  inject markup or an external resource into a page students read.
- The build and test tooling as it runs in CI: anything in `Eldev`,
  `scripts/` or `.githooks/` that could execute untrusted content.

## What is out of scope

- Canvas itself, and any LTI tool it launches (Turnitin, Gradescope).
  Report those to the vendor or your institution.
- Bugs in dependencies (`plz`, `transient`, Org). Report upstream; open
  an issue here if org-canvas needs to work around one.
- Anything requiring a token you already hold: org-canvas can do
  whatever your Canvas role can do, by design.

## If a token has leaked

Treat it as burned even if the leak was local. Rotate it in Canvas
under **Account → Settings → Approved Integrations** (delete the token,
generate a new one), then replace it in your `org-canvas-credentials.el`.
That file is gitignored; check `git status` before every commit if you
have moved it.

## Reducing exposure

- Keep `org-canvas-log-request-bodies` off unless you are debugging a
  specific request; the log then contains only URLs and status lines.
- Mark a course you do not own read-only in its credentials file
  (`org-canvas-read-only`); every write is refused before it is built.
- Prefer `org-canvas-diff` and `org-canvas-validate` over a push when
  you are unsure what a sync would do; both are read-only.

## Supported versions

Only `main` is supported. There is no release line yet; a fix lands on
`main` and the changelog names it.
