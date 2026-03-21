from __future__ import annotations

import re
import sys
from pathlib import Path


LINE_PATTERN = re.compile(
    r'(?P<indent>\s*)!\[(?P<alt>[^\]]*)\]\((?P<thumb>https://storage\.googleapis\.com/blogs_josa/sport/blog_assets/'
    r'[^\s)]+/img/thumbnail/[^\s)]+)\)\{(?P<attrs>[^}]*(?:group="hi")[^}]*)\}(?P<trailing>\s*)$'
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


def rewrite_line(line: str) -> str:
    match = LINE_PATTERN.match(line)
    if not match:
        return line

    alt = match.group("alt")
    thumb = match.group("thumb")
    full = thumb.replace("/img/thumbnail/", "/img/full/")
    indent = match.group("indent")
    attrs = match.group("attrs")
    trailing = match.group("trailing")

    return f'{indent}[![{alt}]({thumb}){{{attrs}}}]({full}){trailing}'


def main() -> int:
    roots = sys.argv[1:] or ["posts", "index.qmd"]
    for file_path in iter_qmd_files(roots):
        original_lines = file_path.read_text(encoding="utf-8").splitlines(keepends=True)
        updated_lines = [rewrite_line(line) for line in original_lines]

        if updated_lines != original_lines:
            file_path.write_text("".join(updated_lines), encoding="utf-8")
            print(f"Updated {file_path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
