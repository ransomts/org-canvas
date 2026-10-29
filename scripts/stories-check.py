#!/usr/bin/env python3
"""Keep documentation/user-stories.org honest against the package.

A story catalogue rots the way any map does: a command is renamed and a
story keeps naming the old one, a capability ships with no manual
section for a story to rest on, or a planned story's issue closes
without the story moving. Three checks catch that, none needing the
network:

  symbols  every command a story names is documented in the manual
  manual   every autoloaded command in lisp/ is in the manual
  format   shipped stories read "As a ---, I can"; planned stories
           carry their issue in the heading and acceptance criteria;
           proposed ones carry no issue number

    scripts/stories-check.py            run the checks, write nothing
    scripts/stories-check.py --issues   also ask gh whether every
                                        planned issue is still open

CI runs the first three in the generated-artifacts job beside the
changelog fragment check. --issues belongs to the fold-time sweep
described in user-stories.org, "Keeping this file true": an issue
closed under a Planned heading is a story that owes its move to
"Shipped behaviour".
"""

import argparse
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
STORIES = ROOT / "documentation" / "user-stories.org"
MANUAL = ROOT / "documentation" / "manual.org"
LISP = ROOT / "lisp"

SHIPPED_STORY = re.compile(
    r"^- As (?:a course owner|an adopter|a collaborator|a user), I can ")
PLANNED_HEADING = re.compile(r"^\*\* +(.*?)\s+\(#(\d+)\)\s*$")


class StoryError(Exception):
    pass


def autoload_commands():
    """Return the autoloaded org-canvas commands: the user surface."""
    commands = set()
    for path in sorted(LISP.glob("*.el")):
        lines = path.read_text(encoding="utf-8").splitlines()
        for n, line in enumerate(lines):
            if not line.startswith(";;;###autoload"):
                continue
            for follow in lines[n + 1:n + 4]:
                if not follow.strip():
                    continue
                m = re.match(r"\(defun (org-canvas-[A-Za-z0-9-]+)", follow)
                if m:
                    commands.add(m.group(1))
                break
    return sorted(commands)


def check_symbols(problems):
    """Every command a story names must be in the manual."""
    stories = STORIES.read_text(encoding="utf-8")
    manual = MANUAL.read_text(encoding="utf-8")
    rel = STORIES.relative_to(ROOT)
    for sym in sorted(set(re.findall(r"org-canvas-[A-Za-z0-9-]+", stories))):
        if sym not in manual:
            problems.append(
                f"{rel}: a story names {sym}, which the manual does not "
                f"document: the command was renamed or removed, and the "
                f"story is lying")


def check_manual(problems):
    """Every autoloaded command must be in the manual."""
    manual = MANUAL.read_text(encoding="utf-8")
    for cmd in autoload_commands():
        if cmd not in manual:
            problems.append(
                f"lisp: {cmd} is autoloaded but not in the manual: "
                f"either document it or drop the cookie")


def check_format(problems):
    """Shipped stories are present-tense, planned ones carry an issue."""
    text = STORIES.read_text(encoding="utf-8")
    rel = STORIES.relative_to(ROOT)
    current = None
    heading, heading_line, body = None, None, []

    def close_story():
        if heading is None:
            return
        joined = " ".join(body)
        if ", I want " not in joined:
            problems.append(
                f'{rel}:{heading_line}: planned story "{heading}" is '
                f'not written "As a ---, I want"')
        if "Acceptance criteria:" not in joined:
            problems.append(
                f'{rel}:{heading_line}: planned story "{heading}" '
                f"carries no acceptance criteria")

    for n, line in enumerate(text.splitlines(), 1):
        if re.match(r"^\* ", line):
            close_story()
            current, heading, body = line[2:].strip(), None, []
            continue
        if current is None:
            continue
        if current == "Shipped behaviour":
            if line.startswith("- ") and not SHIPPED_STORY.match(line):
                problems.append(
                    f'{rel}:{n}: shipped story is not "As a ---, I can": '
                    f"{line[:70]}")
        elif current.startswith("Planned"):
            m = PLANNED_HEADING.match(line)
            if m:
                close_story()
                heading, heading_line, body = m.group(1), n, []
            elif re.match(r"^\*\* ", line):
                close_story()
                heading, body = None, []
            elif heading is not None:
                body.append(line)
        elif current.startswith("Proposed"):
            if line.startswith("- ") and (
                    ", I want " not in line or re.search(r"\(#\d+\)", line)):
                problems.append(
                    f"{rel}:{n}: proposed story is not a plain "
                    f'"As a ---, I want" bullet: {line[:70]}')
    close_story()


def repo_slug():
    """Return the origin remote's owner/repo, or None off a fork."""
    r = subprocess.run(["git", "remote", "get-url", "origin"],
                       capture_output=True, text=True, cwd=ROOT)
    m = re.search(r"[:/]([^/:]+)/([^/]+?)(?:\.git)?\s*$", r.stdout)
    return f"{m.group(1)}/{m.group(2)}" if m else None


def check_issues(problems):
    """Every planned story's issue must still be open (needs gh)."""
    text = STORIES.read_text(encoding="utf-8")
    rel = STORIES.relative_to(ROOT)
    slug = repo_slug()
    if slug is None:
        problems.append("error: could not read the origin remote for --issues")
        return
    in_planned = False
    for n, line in enumerate(text.splitlines(), 1):
        if re.match(r"^\* ", line):
            in_planned = line[2:].strip().startswith("Planned")
            continue
        if not in_planned:
            continue
        m = PLANNED_HEADING.match(line)
        if not m:
            continue
        title, number = m.groups()
        r = subprocess.run(
            ["gh", "api", f"repos/{slug}/issues/{number}",
             "--jq", "[.state, (.state_reason // \"unknown\")] | @tsv"],
            capture_output=True, text=True)
        if r.returncode != 0:
            problems.append(
                f"{rel}:{n}: gh could not read issue #{number}: "
                f"{r.stderr.strip()}")
            continue
        state, reason = (r.stdout.strip().split("\t") + ["unknown"])[:2]
        if state.lower() == "open":
            continue
        if reason == "completed":
            problems.append(
                f'{rel}:{n}: #{number} ("{title}") closed as completed: '
                f'the story owes its move to "Shipped behaviour" -- or the '
                f"issue closed without the work shipping, which is the "
                f"thing to look at")
        else:
            problems.append(
                f'{rel}:{n}: #{number} ("{title}") closed as {reason}: '
                f"the story dies, or returns to Proposed with the reason")


def main():
    parser = argparse.ArgumentParser(
        description=__doc__.splitlines()[0])
    parser.add_argument(
        "--issues", action="store_true",
        help="also check with gh that every planned issue is open")
    args = parser.parse_args()

    for path in (STORIES, MANUAL):
        if not path.is_file():
            sys.exit(f"error: {path.relative_to(ROOT)} not found")
    if not LISP.is_dir():
        sys.exit(f"error: {LISP.relative_to(ROOT)} not found")

    problems = []
    check_symbols(problems)
    check_manual(problems)
    check_format(problems)
    if args.issues:
        check_issues(problems)

    if problems:
        for p in problems:
            print(f"error: {p}")
        sys.exit(1)
    lines = STORIES.read_text(encoding="utf-8").splitlines()
    stories = sum(1 for l in lines if SHIPPED_STORY.match(l))
    planned = sum(1 for l in lines if PLANNED_HEADING.match(l))
    print(f"user-stories.org: {stories} stories, {planned} planned, "
          f"{len(autoload_commands())} autoloaded commands: all true")


if __name__ == "__main__":
    main()
