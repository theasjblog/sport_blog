from __future__ import annotations

import concurrent.futures
import re
import sys
import urllib.error
import urllib.request
from pathlib import Path


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


def collect_urls(paths: list[str]) -> list[str]:
    urls: set[str] = set()
    for file_path in iter_qmd_files(paths):
        text = file_path.read_text(encoding="utf-8")
        urls.update(URL_PATTERN.findall(text))
    return sorted(urls)


def check_url(url: str, timeout: float = 10.0) -> tuple[str, str]:
    request = urllib.request.Request(url, method="HEAD")
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            return url, str(response.status)
    except urllib.error.HTTPError as exc:
        return url, f"HTTP {exc.code}"
    except urllib.error.URLError as exc:
        return url, f"ERROR {exc.reason}"
    except Exception as exc:  # pragma: no cover
        return url, f"ERROR {exc}"


def main() -> int:
    roots = sys.argv[1:] or ["index.qmd", "posts"]
    urls = collect_urls(roots)

    print(f"Checking {len(urls)} asset URLs...", file=sys.stderr)

    missing: list[tuple[str, str]] = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=16) as executor:
        for url, status in executor.map(check_url, urls):
            if status != "200":
                missing.append((status, url))

    for status, url in missing:
        print(f"{status}\t{url}")

    print(f"Missing/non-200 count: {len(missing)}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
