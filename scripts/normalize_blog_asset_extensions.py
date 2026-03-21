from __future__ import annotations

import re
import sys
from pathlib import Path


PATTERN = re.compile(
    r"(https://storage\.googleapis\.com/blogs_josa/sport/blog_assets/"
    r"[^\s\"')\]>}]+/img/thumbnail/[^\s\"')\]>}]+)\.(png|jpeg)"
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


def main() -> int:
    roots = sys.argv[1:] or ["index.qmd", "posts"]

    for file_path in iter_qmd_files(roots):
        original = file_path.read_text(encoding="utf-8")
        updated = PATTERN.sub(r"\1.jpg", original)

        if updated != original:
            file_path.write_text(updated, encoding="utf-8")
            print(f"Updated {file_path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
