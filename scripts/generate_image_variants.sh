#!/bin/zsh

set -euo pipefail

ROOT_DIR="${1:-sport}"
FULL_LIMIT_BYTES=$((2500 * 1024))
THUMB_LIMIT_BYTES=$((400 * 1024))

if [[ ! -d "$ROOT_DIR" ]]; then
  echo "Root directory not found: $ROOT_DIR" >&2
  exit 1
fi

tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/sport_blog_img_resize.XXXXXX")"
trap 'rm -rf "$tmp_dir"' EXIT

is_supported_image() {
  local lower_name="${1:l}"
  [[ "$lower_name" == *.jpg || "$lower_name" == *.jpeg || "$lower_name" == *.png ]]
}

write_variant() {
  local source_file="$1"
  local dest_file="$2"
  local max_bytes="$3"
  local max_dimension="$4"

  local quality
  local resize_dimension
  local size_bytes
  local tmp_file="$tmp_dir/$(basename "$dest_file")"

  rm -f "$tmp_file"

  for quality in 85 75 65 55 45 35 25; do
    resize_dimension="$max_dimension"

    while (( resize_dimension >= 400 )); do
      sips \
        --resampleHeightWidthMax "$resize_dimension" \
        --setProperty format jpeg \
        --setProperty formatOptions "$quality" \
        "$source_file" \
        --out "$tmp_file" >/dev/null 2>/dev/null

      if [[ -f "$tmp_file" ]]; then
        size_bytes=$(stat -f%z "$tmp_file")

        if (( size_bytes <= max_bytes )); then
          mv "$tmp_file" "$dest_file"
          return 0
        fi
      fi

      resize_dimension=$(( resize_dimension * 85 / 100 ))
    done
  done

  echo "Failed to compress within limit: $source_file" >&2
  return 1
}

process_img_dir() {
  local img_dir="$1"
  local full_dir="${img_dir}/full"
  local thumb_dir="${img_dir}/thumbnail"
  local source_file

  mkdir -p "$full_dir" "$thumb_dir"

  while IFS= read -r -d '' source_file; do
    local file_name="${source_file:t}"
    local stem="${file_name%.*}"
    local full_dest="${full_dir}/${stem}.jpg"
    local thumb_dest="${thumb_dir}/${stem}.jpg"

    echo "Processing ${source_file}"
    write_variant "$source_file" "$full_dest" "$FULL_LIMIT_BYTES" 2400
    write_variant "$source_file" "$thumb_dest" "$THUMB_LIMIT_BYTES" 1200
  done < <(
    find "$img_dir" -maxdepth 1 -type f -print0 | while IFS= read -r -d '' candidate; do
      if is_supported_image "${candidate:t}"; then
        printf '%s\0' "$candidate"
      fi
    done
  )
}

while IFS= read -r -d '' img_dir; do
  process_img_dir "$img_dir"
done < <(find "$ROOT_DIR" -type d -name img -print0)
