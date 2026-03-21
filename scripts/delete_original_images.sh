#!/bin/zsh

set -euo pipefail

ROOT_DIR="${1:-sport}"

if [[ ! -d "$ROOT_DIR" ]]; then
  echo "Root directory not found: $ROOT_DIR" >&2
  exit 1
fi

is_supported_image() {
  local lower_name="${1:l}"
  [[ "$lower_name" == *.jpg || "$lower_name" == *.jpeg || "$lower_name" == *.png || "$lower_name" == *.on1 ]]
}

while IFS= read -r -d '' img_dir; do
  while IFS= read -r -d '' source_file; do
    if is_supported_image "${source_file:t}"; then
      echo "Deleting ${source_file}"
      rm "$source_file"
    fi
  done < <(find "$img_dir" -maxdepth 1 -type f -print0)
done < <(find "$ROOT_DIR" -type d -name img -print0)
