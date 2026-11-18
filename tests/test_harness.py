"""Exercise the maintainer CLI against disposable local Git repositories."""

from concurrent.futures import ThreadPoolExecutor
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


HARNESS = Path(__file__).resolve().parents[1] / "scripts/rosetta_harness.py"
WRAPPER = HARNESS.with_name("lean-low-priority")


class WorktreeHarnessTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="rosetta-harness-")
        self.addCleanup(self.temporary.cleanup)
        # Whitespace ensures worktree parsing and command invocation are not
        # accidentally shell/whitespace based.
        self.repo = Path(self.temporary.name) / "root checkout "
        self.repo.mkdir()
        self.git("init", "-b", "main")
        self.git("config", "user.name", "Harness Test")
        self.git("config", "user.email", "harness@example.invalid")
        (self.repo / "branch-policy.json").write_text('{"default_dev_branch": "main"}\n')
        (self.repo / ".gitignore").write_text(".worktrees/\n")
        self.git("add", ".")
        self.git("commit", "-m", "chore: initialize test repository")

    def git(self, *args, repo=None):
        return subprocess.run(["git", "-C", str(repo or self.repo), *args],
                              check=True, capture_output=True, text=True).stdout

    def run_cli(self, *args, repo=None):
        return subprocess.run([sys.executable, str(HARNESS), "--repo",
                               str(repo or self.repo), *args], capture_output=True, text=True)

    def cli(self, *args, repo=None, ok=True):
        result = self.run_cli(*args, repo=repo)
        self.assertEqual(result.returncode, 0 if ok else 1, result.stdout + result.stderr)
        return result

    def create(self, name="example", *args):
        self.cli("create-worktree", name, "--branch", f"feat/{name}", *args)
        return self.repo / ".worktrees" / name

    def test_create_from_default_and_existing_branch_without_upstream(self):
        self.git("config", "branch.autoSetupMerge", "always")
        self.git("tag", "main")
        self.git("commit", "--allow-empty", "-m", "test: advance the default branch")
        branch_head = self.git("rev-parse", "refs/heads/main").strip()
        worktree = self.create("first", "--owner", "maintainer", "--scope", "scripts")
        self.assertEqual(self.git("rev-parse", "HEAD", repo=worktree).strip(), branch_head)
        self.assertEqual(self.git("branch", "--show-current").strip(), "main")
        upstream = subprocess.run(["git", "-C", str(worktree), "config", "--get-regexp", r"^branch\.feat/first\."],
                                  capture_output=True, text=True)
        self.assertEqual(upstream.stdout, "")
        self.git("branch", "--no-track", "fix/existing", "refs/heads/main")
        self.git("tag", "fix/existing", "refs/tags/main")
        self.cli("create-worktree", "second", "--branch", "fix/existing")
        reused = self.repo / ".worktrees/second"
        self.assertEqual(self.git("branch", "--show-current", repo=reused).strip(), "fix/existing")
        self.assertEqual(self.git("rev-parse", "HEAD", repo=reused).strip(), branch_head)
        records = json.loads(self.cli("worktree-list").stdout)
        self.assertEqual(len(records), 3)
        self.assertEqual(records[1]["notes"]["owner"], "maintainer")
        self.assertFalse(records[1]["dirty"])

    def test_require_worktree_rejects_root_default_and_detached(self):
        self.assertIn("root checkout", self.cli("require-worktree", ok=False).stderr)
        worktree = self.create()
        self.assertEqual(self.cli("require-worktree", repo=worktree).stdout.strip(), "feat/example")
        self.git("checkout", "--detach", repo=worktree)
        self.assertIn("detached", self.cli("require-worktree", repo=worktree, ok=False).stderr)
        self.git("checkout", "--detach")
        self.git("checkout", "main", repo=worktree)
        self.assertIn("default branch", self.cli("require-worktree", repo=worktree, ok=False).stderr)

    def test_dirty_source_refused_and_existing_dirty_worktree_preserved(self):
        (self.repo / "uncommitted").write_text("keep me")
        self.assertIn("uncommitted", self.cli("create-worktree", "blocked", ok=False).stderr)
        self.assertFalse((self.repo / ".worktrees").exists())
        (self.repo / "uncommitted").unlink()
        worktree = self.create()
        (worktree / "uncommitted").write_text("keep me too")
        self.assertIn("already has a worktree", self.cli("create-worktree", "other", "--branch",
                                                        "feat/example", ok=False).stderr)
        self.assertEqual((worktree / "uncommitted").read_text(), "keep me too")
        self.assertTrue(json.loads(self.cli("worktree-status", repo=worktree).stdout)[0]["dirty"])

    def test_unsafe_paths_and_branches_have_no_side_effects(self):
        for name in ["../escape", "/tmp/escape", "a/b", ".", "_meta", "-option"]:
            self.cli("create-worktree", "--", name, ok=False)
        self.cli("create-worktree", "wrong", "--branch", "main", ok=False)
        self.cli("create-worktree", "wrong", "--branch=--orphan", ok=False)
        self.cli("create-worktree", "wrong", "--scope", "../outside", ok=False)
        self.cli("create-worktree", "wrong", "--base=--help", ok=False)
        self.assertFalse((self.repo / ".worktrees").exists())
        self.assertEqual(len(self.git("branch", "--format=%(refname)").splitlines()), 1)

    def test_existing_destination_never_overwritten(self):
        target = self.repo / ".worktrees" / "example"
        target.mkdir(parents=True)
        (target / "keep").write_text("data")
        self.cli("create-worktree", "example", ok=False)
        self.assertEqual((target / "keep").read_text(), "data")

    def test_symlinked_storage_and_notes_rejected(self):
        outside = Path(self.temporary.name) / "outside"
        outside.mkdir()
        home = self.repo / ".worktrees"
        home.symlink_to(outside, target_is_directory=True)
        self.cli("create-worktree", "wrong", ok=False)
        self.assertEqual(list(outside.iterdir()), [])
        home.unlink()
        home.mkdir()
        (home / "_meta").symlink_to(outside, target_is_directory=True)
        self.cli("create-worktree", "wrong", ok=False)
        (home / "_meta").unlink()
        (home / "_meta").mkdir()
        (home / "_meta/wrong.json").symlink_to(outside / "missing")
        self.cli("create-worktree", "wrong", ok=False)
        self.assertFalse((outside / "missing").exists())

    def test_preharness_worktree_and_owner_coordination(self):
        worktree = self.repo / ".worktrees" / "manual"
        self.git("worktree", "add", "-b", "fix/manual", str(worktree))
        records = json.loads(self.cli("worktree-list").stdout)
        self.assertEqual(len(records), 2)
        self.assertNotIn("notes", records[1])
        self.cli("worktree-claim", "--owner", "alice", "--summary", "review lifecycle",
                 "--scope", "Rosetta", repo=worktree)
        self.assertIn("alice", self.cli("worktree-status", repo=worktree).stdout)
        self.assertIn("Already claimed", self.cli("worktree-claim", "--owner", "bob",
                                                 repo=worktree, ok=False).stderr)

    def test_stale_coordination_notes_are_not_overwritten_by_creation(self):
        notes = self.repo / ".worktrees/_meta/example.json"
        notes.parent.mkdir(parents=True)
        content = '{"owner": "alice"}\n'
        notes.write_text(content)
        self.assertIn("already exist", self.cli("create-worktree", "example", "--owner", "bob",
                                               ok=False).stderr)
        self.assertEqual(notes.read_text(), content)
        self.assertFalse(notes.parent.parent.joinpath("example").exists())

    def test_prunable_worktree_does_not_report_root_status(self):
        worktree = self.create()
        shutil.rmtree(worktree)
        worktree.mkdir()
        (self.repo / "dirty-root").touch()
        records = json.loads(self.cli("worktree-list").stdout)
        self.assertTrue(records[0]["dirty"])
        self.assertTrue(records[1]["prunable"])
        self.assertIsNone(records[1]["dirty"])

    def test_concurrent_first_claims_preserve_one_owner(self):
        worktrees = [self.create("unclaimed"), self.create("ownerless", "--summary", "notes")]
        for worktree in worktrees:
            with self.subTest(worktree=worktree.name):
                def claim(owner):
                    result = self.run_cli("worktree-claim", "--owner", owner, repo=worktree)
                    return owner, result

                with ThreadPoolExecutor(max_workers=2) as pool:
                    results = sorted(pool.map(claim, ["alice", "bob"]), key=lambda item: item[1].returncode)
                (owner, success), (_, rejected) = results
                self.assertEqual(success.returncode, 0, success.stderr)
                self.assertEqual(rejected.returncode, 1, rejected.stderr)
                self.assertIn(f"Already claimed by {owner}", rejected.stderr)
                records = json.loads(self.cli("worktree-status", repo=worktree).stdout)
                self.assertEqual(records[0]["notes"]["owner"], owner)

    def test_pr_scaffold_requires_clean_worktree_commits_and_public_title(self):
        self.cli("prepare-pr", ok=False)
        worktree = self.create()
        self.assertIn("No commits", self.cli("prepare-pr", repo=worktree, ok=False).stderr)
        (worktree / "change").write_text("new behavior")
        self.cli("prepare-pr", repo=worktree, ok=False)
        self.git("add", "change", repo=worktree)
        self.git("commit", "-m", "fix: improve fixture", repo=worktree)
        starter = "Title: fix: improve fixture\nBase: main\n\nThis PR …\n\n"
        fallback = "[Explain the motivation and resulting behavior before opening the PR.]\n\n"
        self.assertEqual(self.cli("prepare-pr", repo=worktree).stdout, starter + fallback)

        template = worktree / ".github/pull_request_template.md"
        template.parent.mkdir()
        guidance = "<!-- Describe the fixture behavior for reviewers. -->\n"
        template.write_text(guidance, encoding="utf-8")
        self.git("add", ".github/pull_request_template.md", repo=worktree)
        self.git("commit", "--amend", "--no-edit", repo=worktree)
        before = self.git("show-ref")
        self.assertEqual(self.cli("prepare-pr", repo=worktree).stdout, starter + guidance + "\n")
        for title in ["", "fix(scope): example", "docs: example", "[codex] fix: example",
                      "fix: Example", "fix: example."]:
            self.cli("prepare-pr", "--title", title, repo=worktree, ok=False)
        for kind in ["feat", "fix", "doc", "style", "refactor", "test", "chore", "perf"]:
            self.cli("prepare-pr", "--title", f"{kind}: describe behavior", repo=worktree)
        self.assertEqual(before, self.git("show-ref"))
        self.assertEqual(self.git("status", "--porcelain", repo=worktree), "")

    def test_base_and_existing_branch_constraints(self):
        self.git("branch", "fix/existing")
        self.cli("create-worktree", "example", "--branch", "fix/existing", "--base", "main", ok=False)
        self.cli("create-worktree", "example", "--base", "does-not-exist", ok=False)
        self.assertFalse((self.repo / ".worktrees").exists())
        self.create("from-head", "--base", "HEAD")

    def test_reuse_does_not_require_default_ref(self):
        self.git("branch", "fix/existing")
        self.git("branch", "-m", "renamed-main")
        self.cli("create-worktree", "example", "--branch", "fix/existing")
        worktree = self.repo / ".worktrees/example"
        self.assertEqual(self.git("branch", "--show-current", repo=worktree).strip(), "fix/existing")

    def test_pr_scaffold_uses_explicit_stack_base_without_mutating_refs(self):
        first = self.create("first")
        (first / "first").write_text("first layer")
        self.git("add", ".", repo=first)
        self.git("commit", "-m", "feat: add first layer", repo=first)
        second = self.create("second", "--base", "feat/first")
        self.assertIn("No commits", self.cli("prepare-pr", "--base", "feat/first",
                                            repo=second, ok=False).stderr)
        (second / "second").write_text("second layer")
        self.git("add", ".", repo=second)
        self.git("commit", "-m", "feat: add second layer", repo=second)
        before = self.git("show-ref")
        self.assertIn("Base: feat/first", self.cli("prepare-pr", "--base", "feat/first",
                                                  repo=second).stdout)
        revision = self.git("rev-parse", "feat/first").strip()
        self.assertIn(f"Base: {revision}", self.cli("prepare-pr", "--base", revision,
                                                   repo=second).stdout)
        for base in ["missing", "--help"]:
            self.cli("prepare-pr", f"--base={base}", repo=second, ok=False)
        self.assertEqual(before, self.git("show-ref"))
        (first / "other").write_text("divergent change")
        self.git("add", ".", repo=first)
        self.git("commit", "-m", "fix: diverge first layer", repo=first)
        before = self.git("show-ref")
        self.assertIn("ancestor", self.cli("prepare-pr", "--base", "feat/first",
                                          repo=second, ok=False).stderr)
        self.assertEqual(self.git("status", "--porcelain", repo=second), "")
        self.assertEqual(before, self.git("show-ref"))

    def test_branch_checkout_shorthand_is_not_a_literal_branch(self):
        self.git("checkout", "-b", "fix/temporary")
        self.git("checkout", "main")
        self.assertIn("literal branch", self.cli("create-worktree", "example", "--branch", "@{-1}",
                                               ok=False).stderr)
        self.assertFalse((self.repo / ".worktrees").exists())

    def test_validation_dispatch_stops_after_failed_build(self):
        scripts = self.repo / "scripts"
        scripts.mkdir()
        shutil.copy2(WRAPPER, scripts / WRAPPER.name)
        (self.repo / "verso").mkdir()
        binaries = Path(self.temporary.name) / "bin"
        binaries.mkdir()
        log = Path(self.temporary.name) / "commands.jsonl"
        lake = binaries / "lake"
        lake.write_text(f"#!{sys.executable}\n"
                        "import json, os, pathlib, sys\n"
                        f"with open({str(log)!r}, 'a') as output:\n"
                        "    output.write(json.dumps([os.getcwd(), sys.argv[1:]]) + '\\n')\n"
                        "sys.exit(8 if os.environ.get('FAIL_BUILD') and sys.argv[1] == 'build' else 0)\n")
        lake.chmod(0o755)
        env = dict(os.environ, PATH=str(binaries) + os.pathsep + os.environ["PATH"])
        command = [sys.executable, str(HARNESS), "--repo", str(self.repo), "validate", "core", "verso"]
        result = subprocess.run(command, env=env, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual([json.loads(line) for line in log.read_text().splitlines()],
                         [[str(self.repo), ["build"]], [str(self.repo), ["test"]],
                          [str(self.repo / "verso"), ["build"]], [str(self.repo / "verso"), ["test"]]])
        log.unlink()
        result = subprocess.run(command, env=dict(env, FAIL_BUILD="yes"), capture_output=True, text=True)
        self.assertEqual(result.returncode, 1)
        self.assertEqual(len(log.read_text().splitlines()), 1)
        self.assertIn("uncommitted", self.cli("validate", "packages", ok=False).stderr)


class LowPriorityTests(unittest.TestCase):
    def test_argument_boundaries_and_exit_status(self):
        result = subprocess.run([str(WRAPPER), sys.executable, "-c",
                                 "import sys; print(sys.argv[1]); sys.exit(7)", "spaces; $literal"],
                                capture_output=True, text=True)
        self.assertEqual(result.returncode, 7)
        self.assertEqual(result.stdout.strip(), "spaces; $literal")

    def test_usage_and_invalid_niceness(self):
        self.assertEqual(subprocess.run([str(WRAPPER)], capture_output=True).returncode, 2)
        for value in ["-1", "20", "1;exit", "99999999999999999999999999"]:
            env = dict(os.environ, ROSETTA_LEAN_NICENESS=value)
            self.assertEqual(subprocess.run([str(WRAPPER), "true"], env=env,
                                            capture_output=True).returncode, 2)


if __name__ == "__main__":
    unittest.main()
