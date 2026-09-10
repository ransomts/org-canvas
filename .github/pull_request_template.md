Closes #

## Summary

<!-- What changed and why. If it fixes a bug seen on a live course, say what the course showed. -->

## Checklist

- [ ] `eldev test` is green
- [ ] `eldev lint` has no warnings. CI lints on Emacs 30.1, whose checkdoc rejects a third-person verb ("holds") in a docstring's first line even when a newer local Emacs accepts it
- [ ] `eldev complexity` reports 0 functions above 15
- [ ] Coverage stays at 99% or above (`eldev test -u "on,codecov,dontsend" -U coverage/coverage.json`)
- [ ] `CHANGELOG.org` has an entry under *Unreleased* naming the issue
- [ ] If the property registry changed: the manual's Property Reference was regenerated with
      `eldev emacs --batch -l test/docgen/generate-property-reference.el --eval '(org-canvas-docgen-write "documentation/manual.org")'`
- [ ] If behaviour changed: `documentation/manual.org` and the relevant `documentation/architecture/` narrative say so
- [ ] If a sync command was added: it is in `org-canvas-dry-run--sync-commands` (Hard Rule 1)
- [ ] If a module was added: it is in `eldev-undercover-fileset` in `Eldev`, and its sync and delete functions are mocked in `test/org-canvas-test.el`

## Test plan

<!-- What you ran, and against what. A live course probe should say read-only or not. -->
