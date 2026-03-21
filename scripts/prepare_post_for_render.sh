#!/bin/zsh

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: zsh scripts/prepare_post_for_render.sh posts/<category>/<post-slug>" >&2
  exit 1
fi

post_dir="${1:A}"

if [[ ! -d "$post_dir" ]]; then
  echo "Post directory not found: $post_dir" >&2
  exit 1
fi

if [[ ! -f "$post_dir/index.qmd" ]]; then
  echo "Missing index.qmd in: $post_dir" >&2
  exit 1
fi

img_dir="$post_dir/img"

if [[ -d "$img_dir" ]]; then
  if find "$img_dir" -maxdepth 1 -type f \( -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.png' \) | grep -q .; then
    echo "Generating image variants in $img_dir"
    zsh scripts/generate_image_variants.sh "$post_dir"
  else
    echo "No source images found in $img_dir"
  fi
else
  echo "No img/ directory in $post_dir"
fi

echo "Materializing summary table for $post_dir/index.qmd"
Rscript scripts/materialize_post_summary_tables.R "$post_dir/index.qmd"
