"""Local worktree coordination and validation for Rosetta maintainers.

Git is the worktree registry. Optional owner/scope notes are local, untracked
coordination hints; they do not grant permission or lock files. No command
pushes, removes worktrees, resets branches, or discards changes.
"""

from __future__ import annotations

import argparse
from contextlib import contextmanager
import fcntl
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile


class HarnessError(Exception):
    """An expected validation or Git failure."""


def git(repo: Path, *args: str) -> str:
    result = subprocess.run(["git", "-C", str(repo), *args], capture_output=True)
    output = os.fsdecode(result.stdout)
    if result.returncode:
        raise HarnessError(os.fsdecode(result.stderr).strip() or output.strip())
    return output


def checkout(repo: Path) -> Path:
    return Path(git(repo, "rev-parse", "--show-toplevel").removesuffix("\n")).resolve()


def root_checkout(repo: Path) -> Path:
    common_directory = git(repo, "rev-parse", "--path-format=absolute", "--git-common-dir")
    common = Path(common_directory.removesuffix("\n"))
    # Rosetta uses ordinary clones with linked worktrees, not bare repositories
    # or custom --separate-git-dir layouts.
    if common.name != ".git" or not (common.parent / ".git").is_dir():
        raise HarnessError("Expected an ordinary root checkout with a .git directory")
    return common.parent.resolve()


@contextmanager
def lock_worktrees(repo: Path):
    # The common Git directory gives every linked worktree the same lock. Closing
    # the descriptor releases it, including when the process exits early.
    descriptor = os.open(root_checkout(repo) / ".git", os.O_RDONLY)
    try:
        fcntl.flock(descriptor, fcntl.LOCK_EX)
        yield
    finally:
        os.close(descriptor)


def default_branch(repo: Path) -> str:
    try:
        value = json.loads((repo / "branch-policy.json").read_text(encoding="utf-8"))["default_dev_branch"]
    except (OSError, ValueError, KeyError, TypeError) as error:
        raise HarnessError(f"Cannot read branch-policy.json: {error}") from error
    if not isinstance(value, str):
        raise HarnessError("default_dev_branch must be a branch name")
    validate_branch(repo, value)
    return value


def validate_branch(repo: Path, branch: str) -> None:
    if git(repo, "check-ref-format", "--branch", branch).strip() != branch:
        raise HarnessError("Use a literal branch name, not a checkout shorthand")


def commit_revision(repo: Path, ref: str) -> str:
    return git(repo, "rev-parse", "--verify", "--end-of-options", f"{ref}^{{commit}}").strip()


def worktrees(repo: Path) -> list[dict[str, str]]:
    records = []
    record: dict[str, str] = {}
    # -z preserves spaces, newlines and other characters in existing paths.
    for field in git(repo, "worktree", "list", "--porcelain", "-z").split("\0"):
        if not field:
            if record:
                records.append(record)
                record = {}
            continue
        key, _, value = field.partition(" ")
        record[key] = value
    if record:
        records.append(record)
    return records


def require_worktree(repo: Path) -> str:
    if repo == root_checkout(repo):
        raise HarnessError("Implementation belongs in a linked worktree; keep the root checkout stable")
    if repo.parent != worktree_home(repo):
        raise HarnessError("Implementation worktrees belong directly under the root .worktrees directory")
    branch = git(repo, "branch", "--show-current").strip()
    if not branch or branch == default_branch(repo):
        raise HarnessError("Use a named feature/fix branch, not detached HEAD or the default branch")
    return branch


def require_clean(repo: Path) -> None:
    if git(repo, "status", "--porcelain", "--untracked-files=normal"):
        raise HarnessError("Checkout has uncommitted changes; commit or explicitly preserve them first")


def worktree_home(repo: Path) -> Path:
    root = root_checkout(repo)
    home = root / ".worktrees"
    if home.is_symlink() or (home.exists() and not home.is_dir()):
        raise HarnessError(".worktrees must be a real directory inside the root checkout")
    return home


def safe_name(name: str) -> str:
    if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", name):
        raise HarnessError("Worktree names must contain lowercase letters, digits and hyphens")
    return name


def notes_path(repo: Path, name: str) -> Path:
    directory = worktree_home(repo) / "_meta"
    if directory.is_symlink() or (directory.exists() and not directory.is_dir()):
        raise HarnessError(".worktrees/_meta must be a real local directory")
    target = directory / f"{safe_name(name)}.json"
    if target.is_symlink():
        raise HarnessError("Coordination notes must not be symlinks")
    return target


def coordination_notes(args: argparse.Namespace) -> dict[str, object]:
    if args.owner is not None and not args.owner.strip():
        raise HarnessError("Provide a nonempty owner for the claim")
    scopes = args.scope or []
    for scope in scopes:
        relative = Path(scope)
        if relative.is_absolute() or ".." in relative.parts:
            raise HarnessError("Write scopes must be relative paths inside the checkout")
    return {"owner": args.owner, "summary": args.summary, "scope": scopes}


def save_notes(target: Path, notes: dict[str, object]) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=".claim-", dir=target.parent) as directory:
        temporary = Path(directory) / target.name
        temporary.write_text(json.dumps(notes, indent=2) + "\n", encoding="utf-8")
        temporary.replace(target)


def create_worktree(repo: Path, args: argparse.Namespace) -> None:
    name = safe_name(args.name)
    home = worktree_home(repo)
    target = home / name
    if target.exists() or target.is_symlink():
        raise HarnessError(f"Worktree path already exists: {target}")
    branch = args.branch if args.branch is not None else f"wip/{name}"
    validate_branch(repo, branch)
    default = default_branch(repo)
    if branch == default:
        raise HarnessError("The default branch belongs to the stable root checkout")
    # Validate notes before changing Git or filesystem state.
    target_notes = notes_path(repo, name)
    if target_notes.exists():
        raise HarnessError("Coordination notes already exist; coordinate with their owner before reusing this name")
    notes = coordination_notes(args)
    if any(record.get("branch") == f"refs/heads/{branch}" for record in worktrees(repo)):
        raise HarnessError(f"Branch already has a worktree: {branch}")
    require_clean(repo)
    existing = subprocess.run(["git", "-C", str(repo), "show-ref", "--verify", "--quiet",
                               f"refs/heads/{branch}"]).returncode == 0
    if existing:
        if args.base is not None:
            raise HarnessError("--base is only meaningful when creating a new branch")
        git(repo, "worktree", "add", "--", str(target), branch)
    else:
        base = args.base if args.base is not None else f"refs/heads/{default}"
        revision = commit_revision(repo, base)
        git(repo, "worktree", "add", "-b", branch, "--", str(target), revision)
    if args.owner or args.summary or args.scope:
        try:
            save_notes(target_notes, notes)
        except OSError as error:
            raise HarnessError(f"Created worktree at {target} but could not save notes: {error}") from error
    print(target)


def worktree_status(repo: Path, current_only: bool = False) -> None:
    root = root_checkout(repo)
    records = []
    for item in worktrees(repo):
        path = Path(item["worktree"])
        if current_only and path.resolve() != repo:
            continue
        dirty = None
        if path.exists() and "prunable" not in item:
            dirty = bool(git(path, "status", "--porcelain", "--untracked-files=normal"))
        record: dict[str, object] = {
            "path": str(path), "branch": item.get("branch", "").removeprefix("refs/heads/") or None,
            "root": path.resolve() == root,
            "dirty": dirty,
            "locked": "locked" in item, "prunable": "prunable" in item,
        }
        if path.parent == root / ".worktrees" and re.fullmatch(r"[a-z0-9][a-z0-9-]*", path.name):
            notes = notes_path(repo, path.name)
            if notes.exists():
                try:
                    record["notes"] = json.loads(notes.read_text(encoding="utf-8"))
                except (OSError, ValueError) as error:
                    raise HarnessError(f"Cannot read {notes}: {error}") from error
        records.append(record)
    print(json.dumps(records, indent=2))


def claim_worktree(repo: Path, args: argparse.Namespace) -> None:
    require_worktree(repo)
    notes = coordination_notes(args)
    target = notes_path(repo, repo.name)
    if target.exists():
        previous = json.loads(target.read_text(encoding="utf-8"))
        if not isinstance(previous, dict):
            raise HarnessError(f"Expected an object in coordination notes: {target}")
        if previous.get("owner") and previous["owner"] != args.owner:
            raise HarnessError(f"Already claimed by {previous['owner']}; coordinate with that owner first")
    save_notes(target, notes)
    print(target)


def prepare_pr(repo: Path, args: argparse.Namespace) -> None:
    require_worktree(repo)
    require_clean(repo)
    base = args.base if args.base is not None else default_branch(repo)
    revision = commit_revision(repo, f"refs/heads/{base}" if args.base is None else base)
    if subprocess.run(["git", "-C", str(repo), "merge-base", "--is-ancestor", revision, "HEAD"]).returncode:
        raise HarnessError(f"PR base must be an ancestor of HEAD: {base}")
    count = int(git(repo, "rev-list", "--count", f"{revision}..HEAD").strip())
    if not count:
        raise HarnessError(f"No commits to propose against {base}")
    title = args.title if args.title is not None else git(repo, "log", "-1", "--format=%s").strip()
    if not re.fullmatch(r"(feat|fix|doc|style|refactor|test|chore|perf): [a-z0-9].*", title) or "\n" in title:
        raise HarnessError("Use a Lean-style title: type: summary (no scopes; lowercase summary)")
    if title.endswith("."):
        raise HarnessError("Public subjects should not end with a period")
    try:
        guidance = (repo / ".github/pull_request_template.md").read_text(encoding="utf-8")
    except FileNotFoundError:
        guidance = "[Explain the motivation and resulting behavior before opening the PR.]\n"
    print(f"Title: {title}\nBase: {base}\n\nThis PR …\n\n{guidance}")


def validate(repo: Path, args: argparse.Namespace) -> None:
    checks = args.checks or ["all"]
    selected = {"harness", "core", "verso", "packages"} if "all" in checks else set(checks)
    commands: list[tuple[Path, list[str]]] = []
    if "harness" in selected:
        commands.append((repo, [sys.executable, "-m", "unittest", "discover", "-s", "tests",
                                "-p", "test_harness.py"]))
    wrapper = str(repo / "scripts/lean-low-priority")
    for check, directory in [("core", repo), ("verso", repo / "verso")]:
        if check in selected:
            for command in ["build", "test"]:
                commands.append((directory, [wrapper, "lake", command]))
    if "packages" in selected:
        require_clean(repo)
        commands.append((repo, [wrapper, sys.executable, "tests/check_packages.py"]))
    for directory, command in commands:
        print(f"{directory}: {' '.join(command)}", flush=True)
        subprocess.run(command, cwd=directory, check=True)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=Path.cwd(), help="Checkout (default: current directory)")
    commands = parser.add_subparsers(dest="command", required=True)
    create = commands.add_parser("create-worktree", help="Create one local linked worktree/branch")
    create.add_argument("name")
    create.add_argument("--branch", help="Local branch (default: wip/NAME); reuse an unused existing branch")
    create.add_argument("--base", help="New branch start point (default: local default branch)")
    claim = commands.add_parser("worktree-claim", help="Record local owner, summary and write scope")
    for command in [create, claim]:
        command.add_argument("--owner", required=command is claim)
        command.add_argument("--summary")
        command.add_argument("--scope", action="append", help="Relative write scope; repeatable")
    commands.add_parser("worktree-list", help="Show all Git worktrees, including pre-harness ones")
    commands.add_parser("worktree-status", help="Show the current checkout and coordination notes")
    commands.add_parser("require-worktree", help="Reject implementation in the root/default checkout")
    prepare = commands.add_parser("prepare-pr", help="Print a local public title/body scaffold; no network writes")
    prepare.add_argument("--base", help="Ancestor branch or commit to target (default: local default branch)")
    prepare.add_argument("--title", help="Lean-style title (default: latest commit subject)")
    validation = commands.add_parser("validate", help="Run repository checks, with Lean commands at low priority")
    validation.add_argument("checks", nargs="*", choices=["harness", "core", "verso", "packages", "all"])
    args = parser.parse_args()
    try:
        repo = checkout(args.repo)
        if args.command == "create-worktree":
            with lock_worktrees(repo):
                create_worktree(repo, args)
        elif args.command == "worktree-claim":
            with lock_worktrees(repo):
                claim_worktree(repo, args)
        elif args.command in {"worktree-list", "worktree-status"}:
            worktree_status(repo, current_only=args.command == "worktree-status")
        elif args.command == "require-worktree":
            print(require_worktree(repo))
        elif args.command == "prepare-pr":
            prepare_pr(repo, args)
        else:
            validate(repo, args)
    except (HarnessError, OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"rosetta-harness: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
