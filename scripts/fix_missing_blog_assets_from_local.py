from __future__ import annotations

import re
import sys
from collections import defaultdict
from pathlib import Path
from urllib.parse import unquote, urlparse


MISSING_TSV = Path("scripts/missing_blog_assets.tsv")
URL_PATTERN = re.compile(
    r"https://storage\.googleapis\.com/blogs_josa/sport/blog_assets/[^\s\"')\]>}]+"
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


def parse_missing_urls() -> dict[str, list[str]]:
    missing_by_post: dict[str, list[str]] = defaultdict(list)
    for line in MISSING_TSV.read_text(encoding="utf-8").splitlines():
        parts = line.split("\t", 1)
        if len(parts) != 2:
            continue
        _, url = parts
        parsed = urlparse(url)
        path_parts = parsed.path.split("/")
        try:
            post_idx = path_parts.index("blog_assets") + 1
        except ValueError:
            continue
        post_segment = unquote(path_parts[post_idx])
        missing_by_post[post_segment].append(url)
    return missing_by_post


def available_local_filenames(post_segment: str) -> list[str]:
    local_dir = Path("blog_assets") / post_segment / "img" / "thumbnail"
    if not local_dir.is_dir():
        return []
    return sorted(
        p.name
        for p in local_dir.iterdir()
        if p.is_file() and p.name != ".DS_Store"
    )


def build_replacement_map(
    missing_by_post: dict[str, list[str]], file_texts: dict[Path, str]
) -> tuple[dict[str, str], dict[str, str]]:
    replacement_map: dict[str, str] = {}
    skipped_posts: dict[str, str] = {}

    referenced_by_post: dict[str, list[str]] = defaultdict(list)
    for text in file_texts.values():
        for url in URL_PATTERN.findall(text):
            parsed = urlparse(url)
            path_parts = parsed.path.split("/")
            try:
                post_idx = path_parts.index("blog_assets") + 1
                file_idx = path_parts.index("thumbnail") + 1
            except ValueError:
                continue
            post_segment = unquote(path_parts[post_idx])
            referenced_by_post[post_segment].append(unquote(path_parts[file_idx]))

    for post_segment, missing_urls in missing_by_post.items():
        local_files = available_local_filenames(post_segment)
        if not local_files:
            skipped_posts[post_segment] = "no local thumbnails found"
            continue

        referenced = referenced_by_post.get(post_segment, [])
        unused = [name for name in local_files if name not in referenced]
        pool = unused if unused else local_files
        if not pool:
            skipped_posts[post_segment] = "no replacement candidates"
            continue

        idx = 0
        for missing_url in missing_urls:
            replacement_name = pool[idx % len(pool)]
            replacement_url = re.sub(r"[^/]+$", replacement_name, missing_url)
            replacement_map[missing_url] = replacement_url
            idx += 1

    return replacement_map, skipped_posts


def main() -> int:
    roots = sys.argv[1:] or ["index.qmd", "posts"]
    files = iter_qmd_files(roots)
    file_texts = {path: path.read_text(encoding="utf-8") for path in files}

    missing_by_post = parse_missing_urls()
    replacement_map, skipped_posts = build_replacement_map(missing_by_post, file_texts)

    for path, text in file_texts.items():
        updated = text
        for old_url, new_url in replacement_map.items():
            updated = updated.replace(old_url, new_url)
        if updated != text:
            path.write_text(updated, encoding="utf-8")
            print(f"Updated {path}")

    if skipped_posts:
        print("\nSkipped posts:", file=sys.stderr)
        for post_segment, reason in sorted(skipped_posts.items()):
            print(f"{post_segment}: {reason}", file=sys.stderr)

    print(f"\nReplaced {len(replacement_map)} missing URL(s).", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
