from __future__ import annotations

import re
import sys
import unicodedata
import urllib.parse
from pathlib import Path


URL_PATTERN = re.compile(
    r"(https://storage\.googleapis\.com/blogs_josa/sport/blog_assets/)"
    r"([^/\s\"')\]>}]+)"
)


def iter_qmd_files(paths: list[str]) -> list[Path]:
    files: list[Path] = []
    for raw_path in paths:
        path = Path(raw_path)
        if path.is_dir():
            files.extend(sorted(path.rglob("*.qmd")))
        elif path.is_file():
            files.append(path)
    return files


def normalize_match(match: re.Match[str]) -> str:
    prefix, post_segment = match.groups()
    decoded = urllib.parse.unquote(post_segment)
    normalized = unicodedata.normalize("NFD", decoded)
    encoded = urllib.parse.quote(normalized, safe="")
    return f"{prefix}{encoded}"


def main() -> int:
    roots = sys.argv[1:] or ["index.qmd", "posts"]

    for file_path in iter_qmd_files(roots):
        original = file_path.read_text(encoding="utf-8")
        updated = URL_PATTERN.sub(normalize_match, original)

        if updated != original:
            file_path.write_text(updated, encoding="utf-8")
            print(f"Updated {file_path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
