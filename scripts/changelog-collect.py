#!/usr/bin/env python3
"""Fold changelog.d/ fragments into CHANGELOG.org.

Each pull request adds one fragment, changelog.d/<issue>.org (or
<issue>-<slug>.org when an issue ships in more than one), instead of
editing CHANGELOG.org: every PR editing the top of the same section
made each merge conflict with the next.  A fragment is Org text:

    * Fixed
    - #298 What changed, in the words the changelog uses.  Continuation
      lines are indented two spaces.

One or more `* Section` headings (Added, Changed, Deprecated, Removed,
Fixed, Security), each followed by one or more `- #N ...` items.

    scripts/changelog-collect.py            fold every fragment in, delete them
    scripts/changelog-collect.py --check    validate them and the fold, write nothing
    scripts/changelog-collect.py --dry-run  print the CHANGELOG.org the fold would write
    scripts/changelog-collect.py --branch-check BRANCH
                                            does BRANCH bring the fragment its
                                            name promises?

`--branch-check` answers the question a pull request asks of its own
branch.  A branch named for an issue (`fix/601-complexity-budget`,
`docs/598-branch-check`) must bring `changelog.d/601.org` or
`601-<slug>.org`.  An `integration/` branch lands several issues at
once, one commit and one fragment each, and carries a date rather than
an issue in its name, so it must bring at least one *new* fragment
against `--base` (default `origin/main`); every fragment it brings is
validated like any other.  A branch with no issue number in its name
is only validated, exactly as `--check` does.

Entries land at the top of their section under `* Unreleased`, newest
(highest number) first, a missing section created in the order above.
"""

import argparse
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
FRAGMENTS = ROOT / "changelog.d"
CHANGELOG = ROOT / "CHANGELOG.org"
SECTIONS = ["Added", "Changed", "Deprecated", "Removed", "Fixed", "Security"]
NAME = re.compile(r"^(\d+)(?:-[A-Za-z0-9._-]+)?\.org$")
ITEM = re.compile(r"^- #\d+\b")
# A branch named for an issue: `fix/601-...`, `docs/598`, `chore/575_...`.
BRANCH_ISSUE = re.compile(r"^[A-Za-z][A-Za-z0-9._-]*/(\d+)(?:[-_.]|$)")
INTEGRATION = "integration/"


class FragmentError(Exception):
    pass


def branch_issue(branch):
    """Return the issue BRANCH is named for, or None.

    An `integration/` branch carries a date, not an issue: it lands
    several issues at once, and its name must not be read as one
    (issue #598 read `integration/2026-10-31` as issue 2026)."""
    if branch == INTEGRATION.rstrip("/") or branch.startswith(INTEGRATION):
        return None
    match = BRANCH_ISSUE.match(branch)
    return int(match.group(1)) if match else None


def branch_fragments(base):
    """Return the fragments BRANCH adds against BASE."""
    try:
        result = subprocess.run(
            ["git", "diff", "--name-only", "--diff-filter=A",
             f"{base}...HEAD", "--", "changelog.d"],
            cwd=ROOT, capture_output=True, text=True, check=True)
    except (OSError, subprocess.CalledProcessError) as err:
        detail = getattr(err, "stderr", "") or err
        raise FragmentError(f"cannot compare against {base}: {detail}")
    return [ROOT / line for line in result.stdout.splitlines() if line.strip()]


def fragment_files():
    """Return the fragment paths, newest (highest number) first."""
    if not FRAGMENTS.is_dir():
        return []
    paths = [p for p in FRAGMENTS.iterdir()
             if p.suffix == ".org" and p.name != "README.org"]
    for p in paths:
        if not NAME.match(p.name):
            raise FragmentError(f"{p.relative_to(ROOT)}: name it <issue>.org or <issue>-<slug>.org")
    return sorted(paths, key=lambda p: (-int(NAME.match(p.name).group(1)), p.name))


def parse_fragment(path):
    """Return {section: [item text, ...]} for the fragment at PATH."""
    rel = path.relative_to(ROOT)
    sections, section, items = {}, None, None
    for n, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not line.strip() or line.startswith("#"):
            continue
        heading = re.match(r"^\* +(\S+)\s*$", line)
        if heading:
            section = heading.group(1)
            if section not in SECTIONS:
                raise FragmentError(f"{rel}:{n}: unknown section '{section}' (use one of {', '.join(SECTIONS)})")
            items = sections.setdefault(section, [])
        elif section is None:
            raise FragmentError(f"{rel}:{n}: text before the first '* Section' heading")
        elif ITEM.match(line):
            items.append(line)
        elif line.startswith("  ") and items:
            items[-1] += "\n" + line
        else:
            raise FragmentError(f"{rel}:{n}: an item starts '- #<issue> ', continuation lines are indented two spaces")
    if not sections or not any(sections.values()):
        raise FragmentError(f"{rel}: no '- #<issue> ...' item under a '* Section' heading")
    for section, entries in sections.items():
        if not entries:
            raise FragmentError(f"{rel}: '* {section}' has no items")
    return sections


def fold(changelog, collected):
    """Return CHANGELOG text CHANGELOG with COLLECTED ({section: [items]}) added."""
    lines = changelog.split("\n")
    try:
        start = lines.index("* Unreleased")
    except ValueError:
        raise FragmentError("CHANGELOG.org has no '* Unreleased' heading")
    end = next((i for i in range(start + 1, len(lines)) if lines[i].startswith("* ")), len(lines))
    for section in SECTIONS:
        items = collected.get(section)
        if not items:
            continue
        text = "\n".join(items).split("\n")
        heading = f"** {section}"
        if heading in lines[start:end]:
            at = lines.index(heading, start) + 1
            while at < end and not lines[at].strip():
                at += 1
            lines[at:at] = text
        else:
            # Before the first existing section that comes after this one.
            later = [f"** {s}" for s in SECTIONS[SECTIONS.index(section) + 1:]]
            at = next((i for i in range(start + 1, end) if lines[i] in later), end)
            while at > start + 1 and not lines[at - 1].strip():
                at -= 1
            lines[at:at] = ["", heading, ""] + text
        end = next((i for i in range(start + 1, len(lines)) if lines[i].startswith("* ")), len(lines))
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--check", action="store_true", help="validate the fragments and the fold; write nothing")
    mode.add_argument("--dry-run", action="store_true", help="print the folded CHANGELOG.org; write nothing")
    parser.add_argument("--branch-check", metavar="BRANCH",
                        help="require the fragment BRANCH's issue promises; an "
                             "integration/ branch must add at least one fragment")
    parser.add_argument("--base", default="origin/main", metavar="REF",
                        help="what an integration/ branch is compared against (default: origin/main)")
    args = parser.parse_args()
    try:
        paths = fragment_files()
        collected = {}
        for path in paths:
            for section, items in parse_fragment(path).items():
                collected.setdefault(section, []).extend(items)
        result = fold(CHANGELOG.read_text(encoding="utf-8"), collected)
        if args.branch_check:
            issue = branch_issue(args.branch_check)
            if issue is None and args.branch_check.startswith(INTEGRATION):
                added = branch_fragments(args.base)
                if not added:
                    raise FragmentError(
                        f"{args.branch_check}: an integration/ branch adds one "
                        f"changelog.d/<issue>.org per issue; this one adds none "
                        f"against {args.base}")
                print(f"changelog-collect: {args.branch_check} adds "
                      f"{len(added)} fragment(s): " + ", ".join(p.name for p in added))
            elif issue is None:
                print(f"changelog-collect: {args.branch_check} carries no issue "
                      "number; the fragments are validated only")
            else:
                named = [p for p in paths if int(NAME.match(p.name).group(1)) == issue]
                if not named:
                    raise FragmentError(
                        f"{args.branch_check}: no changelog.d/{issue}.org (or "
                        f"{issue}-<slug>.org); see changelog.d/README.org")
                print(f"changelog-collect: {args.branch_check} brings "
                      + ", ".join(p.name for p in named))
            return 0
    except FragmentError as err:
        print(f"changelog-collect: {err}", file=sys.stderr)
        return 1
    if args.check:
        print(f"changelog-collect: {len(paths)} fragment(s) OK")
        return 0
    if args.dry_run:
        sys.stdout.write(result)
        return 0
    if not paths:
        print("changelog-collect: no fragments to fold")
        return 0
    CHANGELOG.write_text(result, encoding="utf-8")
    for path in paths:
        path.unlink()
    print(f"changelog-collect: folded {len(paths)} fragment(s) into CHANGELOG.org")
    return 0


if __name__ == "__main__":
    sys.exit(main())
