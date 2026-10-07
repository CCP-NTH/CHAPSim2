#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Set to 0/1 if you ever want to delete/preserve *outlet* files in 1_data
PRESERVE_OUTLET=${PRESERVE_OUTLET:-1}

clean_one_run_dir() {
  local d="$1"
  [[ -d "$d" ]] || return 0

  echo "== Cleaning: $d =="
  pushd "$d" > /dev/null

  # Delete specific file types in the current directory
  rm -f *.log *.dat fort* *.err *.out core 2>/dev/null || true

  # Delete directories starting with 2_, 3_, 4_ (keep .py and .sh)
  for prefix in 2_ 3_ 4_; do
    for dir in ${prefix}*/ ; do
      [[ -d "$dir" ]] || continue
      echo "  - Cleaning directory: $dir"
      find "$dir" -type f ! \( -name "*.py" -o -name "*.sh" \) -exec rm -f {} + || true
      find "$dir" -type d -empty -delete || true
    done
  done

  # Delete 0_src if present
  if [[ -d "0_src" ]]; then
    rm -rf 0_src
  fi

  # Clean up files in 1_data
  if [[ -d "1_data" ]]; then
    if [[ "$PRESERVE_OUTLET" -eq 1 ]]; then
      echo "  - Cleaning 1_data excluding '*outlet*'"
      find 1_data -type f ! -name "*outlet*" -exec rm -f {} \; || true
    else
      echo "  - Cleaning all files in 1_data"
      find 1_data -type f -exec rm -f {} \; || true
    fi
  fi

  popd > /dev/null
}

echo "Root: $ROOT"
echo "Searching for run directories (run_continuous/run_restart) under: $ROOT"
echo

# Find all run dirs in the restart functional-test tree
find "$ROOT" -type d \( -name "run_continuous" -o -name "run_restart" \) | sort | while read -r d; do
  clean_one_run_dir "$d"
done

echo
echo "Cleanup completed."
