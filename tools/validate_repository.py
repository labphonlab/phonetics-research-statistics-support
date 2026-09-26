#!/usr/bin/env python3
"""Validate the public release without third-party package installs."""

from __future__ import annotations

import json
import wave
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SKIP = {".git", "__pycache__", "results", "output"}
errors: list[str] = []
counts = {"notebooks": 0, "python": 0, "json": 0, "wav": 0}


def public_files(suffix: str):
    for path in ROOT.rglob(f"*{suffix}"):
        if any(part in SKIP for part in path.parts):
            continue
        yield path


for required in ("README.md", "LICENSE", "CITATION.cff", "CHANGELOG.md"):
    if not (ROOT / required).is_file():
        errors.append(f"missing required file: {required}")

for path in public_files(".ipynb"):
    counts["notebooks"] += 1
    try:
        notebook = json.loads(path.read_text(encoding="utf-8"))
        if notebook.get("nbformat") != 4:
            errors.append(f"{path.relative_to(ROOT)}: nbformat is not 4")
        if not isinstance(notebook.get("cells"), list) or not notebook["cells"]:
            errors.append(f"{path.relative_to(ROOT)}: no cells")
    except Exception as exc:
        errors.append(f"{path.relative_to(ROOT)}: invalid notebook: {exc}")

for path in public_files(".json"):
    counts["json"] += 1
    try:
        json.loads(path.read_text(encoding="utf-8"))
    except Exception as exc:
        errors.append(f"{path.relative_to(ROOT)}: invalid JSON: {exc}")

for path in public_files(".py"):
    counts["python"] += 1
    try:
        compile(path.read_text(encoding="utf-8"), str(path), "exec")
    except Exception as exc:
        errors.append(f"{path.relative_to(ROOT)}: Python syntax error: {exc}")

for path in public_files(".wav"):
    counts["wav"] += 1
    try:
        with wave.open(str(path), "rb") as audio:
            if audio.getnchannels() < 1 or audio.getframerate() < 1 or audio.getnframes() < 1:
                errors.append(f"{path.relative_to(ROOT)}: empty or invalid WAVE metadata")
    except Exception as exc:
        errors.append(f"{path.relative_to(ROOT)}: invalid WAVE file: {exc}")

if errors:
    print("Validation failed:")
    for error in errors:
        print(f"- {error}")
    raise SystemExit(1)

print("Validation passed: " + ", ".join(f"{key}={value}" for key, value in counts.items()))
