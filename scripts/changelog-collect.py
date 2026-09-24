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

Entries land at the top of their section under `* Unreleased`, newest
(highest number) first, a missing section created in the order above.
"""

import argparse
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
FRAGMENTS = ROOT / "changelog.d"
CHANGELOG = ROOT / "CHANGELOG.org"
SECTIONS = ["Added", "Changed", "Deprecated", "Removed", "Fixed", "Security"]
NAME = re.compile(r"^(\d+)(?:-[A-Za-z0-9._-]+)?\.org$")
ITEM = re.compile(r"^- #\d+\b")


class FragmentError(Exception):
    pass


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
    args = parser.parse_args()
    try:
        paths = fragment_files()
        collected = {}
        for path in paths:
            for section, items in parse_fragment(path).items():
                collected.setdefault(section, []).extend(items)
        result = fold(CHANGELOG.read_text(encoding="utf-8"), collected)
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
