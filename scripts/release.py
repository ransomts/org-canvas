#!/usr/bin/env python3
"""Cut a versioned release of org-canvas.

org-canvas follows semantic versioning from 0.x: while the package is
pre-1.0 a breaking change bumps the minor version and everything else
bumps the patch, and the maintainer cuts a release before each term
starts plus a patch release for field fixes (issue #627).

    scripts/release.py 0.2.0 --dry-run        print what a release would do
    scripts/release.py 0.2.0                  fold the changelog, bump Version
    scripts/release.py 0.2.0 --commit --tag   and commit and tag it
    scripts/release.py --check                is Version the newest release?

A release folds every `changelog.d/` fragment into a dated
`* VERSION (DATE)` section of `CHANGELOG.org` (leaving `* Unreleased`
empty for the next PR), and rewrites the `;; Version:` header in
`lisp/org-canvas.el`, which `org-canvas-version' reads at runtime so a
single source of truth survives byte-compilation.  Pushing the commit
and the tag, and creating the GitHub release, stay the maintainer's:
the script prints the commands rather than running them.
"""

import argparse
import datetime
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
COLLECT = ROOT / "scripts" / "changelog-collect.py"
CHANGELOG = ROOT / "CHANGELOG.org"
MAIN_SOURCE = ROOT / "lisp" / "org-canvas.el"
VERSION = re.compile(r"^\d+\.\d+\.\d+$")
HEADER = re.compile(r"^;; Version: (\S+)$", re.M)
RELEASE = re.compile(r"^\* (\d+\.\d+\.\d+) \(", re.M)


class ReleaseError(Exception):
    pass


def run(args, dry_run=False):
    """Run ARGS, returning its standard output.  Print instead if DRY-RUN."""
    if dry_run:
        print("would run: " + " ".join(args))
        return ""
    result = subprocess.run(args, cwd=ROOT, capture_output=True, text=True)
    if result.returncode != 0:
        raise ReleaseError(
            f"{' '.join(args)} failed:\n{result.stderr or result.stdout}")
    return result.stdout


def current_version():
    """Return the `;; Version:' header in the main source file."""
    match = HEADER.search(MAIN_SOURCE.read_text(encoding="utf-8"))
    if not match:
        raise ReleaseError("lisp/org-canvas.el has no ';; Version:' header")
    return match.group(1)


def released_versions():
    """Return the versions CHANGELOG.org already has a section for, oldest first."""
    return [tuple(int(part) for part in match.group(1).split("."))
            for match in RELEASE.finditer(CHANGELOG.read_text(encoding="utf-8"))]


def parse_version(text):
    """Return TEXT as a (major, minor, patch) tuple, refusing anything else."""
    if not VERSION.match(text):
        raise ReleaseError(f"'{text}' is not a version like 0.2.0")
    return tuple(int(part) for part in text.split("."))


def check():
    """Refuse a Version header that disagrees with the newest release section."""
    versions = released_versions()
    if not versions:
        print("release: no release section in CHANGELOG.org yet; "
              f"Version is {current_version()}")
        return 0
    newest = ".".join(str(part) for part in max(versions))
    header = current_version()
    if header != newest:
        raise ReleaseError(
            f"lisp/org-canvas.el says Version {header} but CHANGELOG.org's "
            f"newest release is {newest}; a release bumps both")
    print(f"release: Version {header} is CHANGELOG.org's newest release")
    return 0


def require_clean_tree():
    """Refuse to cut a release over uncommitted work."""
    status = run(["git", "status", "--porcelain", "--", "lisp", "test",
                  "scripts", "CHANGELOG.org", "changelog.d"])
    changed = [line for line in status.splitlines() if line.strip()]
    if changed:
        shown = "\n".join(changed[:5])
        more = (f"\n... and {len(changed) - 5} more" if len(changed) > 5 else "")
        raise ReleaseError(
            "the release commit must hold only the release's own changes:\n"
            + shown + more)


def require_main():
    """Refuse to cut a release from a branch."""
    branch = run(["git", "branch", "--show-current"]).strip()
    if branch != "main":
        raise ReleaseError(f"cut a release from main, not '{branch or 'HEAD'}'")


def bump_header(version, dry_run=False):
    """Rewrite the Version header in the main source file to VERSION."""
    text = MAIN_SOURCE.read_text(encoding="utf-8")
    if not HEADER.search(text):
        raise ReleaseError("lisp/org-canvas.el has no ';; Version:' header")
    if dry_run:
        print(f"would set ;; Version: {version} in lisp/org-canvas.el")
        return
    MAIN_SOURCE.write_text(HEADER.sub(f";; Version: {version}", text, count=1),
                           encoding="utf-8")


def main():
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("version", nargs="?", help="the version to cut, e.g. 0.2.0")
    parser.add_argument("--date", metavar="YYYY-MM-DD",
                        help="the release date (default: today)")
    parser.add_argument("--dry-run", action="store_true",
                        help="print what a release would do and change nothing")
    parser.add_argument("--commit", action="store_true",
                        help="commit the changelog and the version bump")
    parser.add_argument("--tag", action="store_true",
                        help="tag the release commit vVERSION (needs --commit)")
    parser.add_argument("--check", action="store_true",
                        help="check that Version is CHANGELOG.org's newest release")
    args = parser.parse_args()
    try:
        if args.check:
            return check()
        if not args.version:
            raise ReleaseError("name the version to cut, e.g. 0.2.0")
        version = parse_version(args.version)
        if args.tag and not args.commit:
            raise ReleaseError("--tag needs --commit")
        date = args.date or datetime.date.today().isoformat()
        try:
            datetime.date.fromisoformat(date)
        except ValueError:
            raise ReleaseError(f"'{date}' is not a date like 2026-11-02")
        versions = released_versions()
        if versions and version <= max(versions):
            raise ReleaseError(
                f"{args.version} is not newer than "
                f"{'.'.join(str(part) for part in max(versions))}")
        if not args.dry_run:
            require_main()
            require_clean_tree()
        run([sys.executable, str(COLLECT), "--release", args.version,
             "--date", date], dry_run=args.dry_run)
        bump_header(args.version, dry_run=args.dry_run)
        if args.commit:
            run(["git", "add", "CHANGELOG.org", "lisp/org-canvas.el",
                 "changelog.d"], dry_run=args.dry_run)
            run(["git", "commit", "-m", f"release: {args.version}"],
                dry_run=args.dry_run)
        if args.tag:
            run(["git", "tag", "-a", f"v{args.version}",
                 "-m", f"org-canvas {args.version}"], dry_run=args.dry_run)
    except ReleaseError as err:
        print(f"release: {err}", file=sys.stderr)
        return 1
    print()
    print(f"release {args.version}:")
    if not args.commit:
        print("  git add CHANGELOG.org lisp/org-canvas.el changelog.d")
        print(f'  git commit -m "release: {args.version}"')
    print("  git push origin main")
    if not args.tag:
        print(f"  git tag -a v{args.version} -m \"org-canvas {args.version}\"")
        print(f"  git push origin v{args.version}")
    else:
        print(f"  git push origin v{args.version}")
    print(f"  gh release create v{args.version} --title \"org-canvas {args.version}\" "
          "--notes-from-tag")
    return 0


if __name__ == "__main__":
    sys.exit(main())
