#!/usr/bin/env bash
set -euo pipefail

echo "==============================================="
echo " CHAPSim Domain/Mesh Interpolation Step 2 Setup"
echo "==============================================="
echo

# Ask user for source path
read -r -p "Enter source case path (e.g. ../step1_case): " SRC_DIR

# Trim spaces
SRC_DIR="$(echo "$SRC_DIR" | xargs)"

# Check path exists
if [[ ! -d "$SRC_DIR" ]]; then
  echo "Error: Directory not found: $SRC_DIR" >&2
  exit 1
fi

# Check required files
if [[ ! -d "$SRC_DIR/1_data" ]]; then
  echo "Error: $SRC_DIR/1_data not found" >&2
  exit 1
fi

if [[ ! -f "$SRC_DIR/input_chapsim.ini" ]]; then
  echo "Error: input_chapsim.ini not found in $SRC_DIR" >&2
  exit 1
fi

if [[ ! -f "$SRC_DIR/input_chapsim_tgt.ini" ]]; then
  echo "Error: input_chapsim_tgt.ini not found in $SRC_DIR" >&2
  exit 1
fi

echo
echo "Using source directory: $SRC_DIR"
echo

# 1) Create 1_data
mkdir -p 1_data

# 2) Copy domain0_* and rename to domain1_*
echo "Copying and renaming domain files..."

shopt -s nullglob
files=("$SRC_DIR"/1_data/domain0_*)
if [[ ${#files[@]} -eq 0 ]]; then
  echo "Error: No domain0_* files found in $SRC_DIR/1_data" >&2
  exit 1
fi

for f in "${files[@]}"; do
  base=$(basename "$f")
  newname="${base/domain0_/domain1_}"
  cp "$f" "1_data/$newname"
done

echo "Copied ${#files[@]} files."

# 3) Copy ini files
cp "$SRC_DIR/input_chapsim.ini" .
cp "$SRC_DIR/input_chapsim_tgt.ini" .

# Helper: replace key in section
replace_key() {
  local file="$1"
  local section="$2"
  local key="$3"
  local newline="$4"

  awk -v sec="$section" -v key="$key" -v newline="$newline" '
    BEGIN { in_sec=0 }
    /^[[:space:]]*\[/ {
      in_sec = ($0 ~ "^[[:space:]]*\\[" sec "\\]")
    }
    {
      if (in_sec && $0 ~ "^[[:space:]]*" key "[[:space:]]*=") {
        print newline
      } else {
        print
      }
    }
  ' "$file" > tmp.ini && mv tmp.ini "$file"
}

# 4) Modify input_chapsim.ini
echo "Updating input_chapsim.ini..."

replace_key input_chapsim.ini process is_prerun "is_prerun= .false."
replace_key input_chapsim.ini flow irestartfrom "irestartfrom= 0"

# Extract values from tgt file
get_val() {
  awk -F'=' -v key="$1" '
    $1 ~ key {
      gsub(/[[:space:]]/, "", $2)
      print $2
    }
  ' input_chapsim_tgt.ini
}

# Domain values
lxx=$(get_val lxx)
lyt=$(get_val lyt)
lyb=$(get_val lyb)
lzz=$(get_val lzz)

# Mesh values
ncx=$(get_val ncx)
ncy=$(get_val ncy)
ncz=$(get_val ncz)
istret=$(get_val istret)
rstret=$(get_val rstret)

# Apply them to main ini
replace_key input_chapsim.ini domain lxx "lxx= $lxx"
replace_key input_chapsim.ini domain lyt "lyt= $lyt"
replace_key input_chapsim.ini domain lyb "lyb= $lyb"
replace_key input_chapsim.ini domain lzz "lzz= $lzz"

replace_key input_chapsim.ini mesh ncx "ncx= $ncx"
replace_key input_chapsim.ini mesh ncy "ncy= $ncy"
replace_key input_chapsim.ini mesh ncz "ncz= $ncz"
replace_key input_chapsim.ini mesh istret "istret= $istret"
replace_key input_chapsim.ini mesh rstret "rstret= $rstret"

echo
echo "=============================================="
echo " Setup complete"
echo "=============================================="
echo
echo "Files prepared:"
echo "  - 1_data/domain1_*"
echo "  - input_chapsim.ini (updated)"
echo "  - input_chapsim_tgt.ini"
echo
echo "Ready to run target case."