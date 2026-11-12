"""Build fresh pinned Git consumers and verify the core/Verso boundary."""

import argparse
import json
from pathlib import Path
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    repo = Path(__file__).resolve().parents[1]
    if subprocess.check_output(["git", "rev-parse", "--is-shallow-repository"], cwd=repo, text=True).strip() == "true":
        raise SystemExit("Package consumer checks need a complete Git checkout; run git fetch --unshallow first.")
    subprocess.run(["git", "diff", "--exit-code", "HEAD"], cwd=repo, check=True)
    revision = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo, text=True).strip()
    bridge_manifest = json.loads((repo / "verso/lake-manifest.json").read_text())
    with tempfile.TemporaryDirectory(prefix="rosetta-consumers-") as temporary:
        output = args.output.resolve() if args.output else Path(temporary)
        for name, dependency, imports, subdir in [
            ("core", "rosetta", "import Rosetta\n#check Rosetta.analyze\n", None),
            ("bridge", "verso-rosetta", "import VersoRosetta\n#check VersoRosetta.collect\n", "verso"),
        ]:
            consumer = output / name
            consumer.mkdir(parents=True, exist_ok=True)
            (consumer / "lean-toolchain").write_bytes((repo / "lean-toolchain").read_bytes())
            config = (f'name = "{name}_consumer"\ndefaultTargets = ["Consumer"]\n'
                      f'[[require]]\nname = "{dependency}"\ngit = {json.dumps(repo.as_uri())}\n'
                      f'rev = "{revision}"\n')
            if subdir:
                config += 'subDir = "verso"\n'
                # Reuse exact warmed external checkouts, never Rosetta source.
                for entry in bridge_manifest["packages"]:
                    if entry["type"] != "git":
                        continue
                    package = entry["name"].strip("«»")
                    source = repo / "verso/.lake/packages" / package
                    if not source.exists():
                        continue
                    actual = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=source, text=True).strip()
                    assert actual == entry["rev"], f"dependency revision differs: {package}"
                    subprocess.run(["git", "diff", "--exit-code", "HEAD"], cwd=source, check=True)
                    destination = consumer / ".lake/packages" / package
                    destination.parent.mkdir(parents=True, exist_ok=True)
                    if not destination.exists():
                        destination.symlink_to(source.resolve(), target_is_directory=True)
            (consumer / "lakefile.toml").write_text(config + '[[lean_lib]]\nname = "Consumer"\n')
            (consumer / "Consumer.lean").write_text(imports)
            subprocess.run(["lake", "build"], cwd=consumer, check=True)
            manifest = json.loads((consumer / "lake-manifest.json").read_text())
            names = {entry["name"].strip("«»") for entry in manifest["packages"]}
            if subdir:
                assert {"rosetta", "verso-rosetta", "verso"} <= names
                assert not {"VersoBlueprint", "lean_vir", "verso-slides", "proofwidgets"} & names
                forbidden = "VersoBlueprint"
            else:
                assert names == {"rosetta"}
                forbidden = "Verso"
            unavailable = consumer / "Unavailable.lean"
            unavailable.write_text(f"import {forbidden}\n")
            result = subprocess.run(["lake", "lean", unavailable.name], cwd=consumer,
                                    capture_output=True, text=True)
            assert result.returncode != 0 and forbidden in result.stdout + result.stderr
            print(f"{name}: pinned Git consumer and import isolation passed", flush=True)


if __name__ == "__main__":
    main()
