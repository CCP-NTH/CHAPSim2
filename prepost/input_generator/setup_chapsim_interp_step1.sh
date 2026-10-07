#!/usr/bin/env bash
set -euo pipefail

SRC_INI="input_chapsim.ini"
TGT_INI="input_chapsim_tgt.ini"

if [[ ! -f "$SRC_INI" ]]; then
  echo "Error: $SRC_INI not found in the current directory: $(pwd)" >&2
  exit 1
fi

trim() {
  local s="$1"
  s="${s#${s%%[![:space:]]*}}"
  s="${s%${s##*[![:space:]]}}"
  printf '%s' "$s"
}

get_ini_value() {
  local file="$1"
  local section="$2"
  local key="$3"
  awk -F'=' -v sec="$section" -v key="$key" '
    BEGIN { in_sec=0 }
    /^[[:space:]]*\[/ {
      in_sec = ($0 ~ "^[[:space:]]*\\[" sec "\\][[:space:]]*$")
      next
    }
    in_sec {
      line=$0
      sub(/[;#].*$/, "", line)
      if (line ~ "^[[:space:]]*" key "[[:space:]]*=") {
        sub("^[[:space:]]*" key "[[:space:]]*=[[:space:]]*", "", line)
        gsub(/[[:space:]]+$/, "", line)
        print line
        exit
      }
    }
  ' "$file"
}

prompt_with_default() {
  local prompt="$1"
  local def="$2"
  local ans
  read -r -p "$prompt [$def]: " ans
  ans="$(trim "$ans")"
  if [[ -z "$ans" ]]; then
    printf '%s' "$def"
  else
    printf '%s' "$ans"
  fi
}

prompt_integer_with_default() {
  local prompt="$1"
  local def="$2"
  local ans
  while true; do
    read -r -p "$prompt [$def]: " ans
    ans="$(trim "$ans")"
    if [[ -z "$ans" ]]; then
      printf '%s' "$def"
      return 0
    elif [[ "$ans" =~ ^[0-9]+$ ]]; then
      printf '%s' "$ans"
      return 0
    else
      echo "Invalid input. Please enter a non-negative integer."
    fi
  done
}

replace_key_in_section() {
  local file="$1"
  local section="$2"
  local key="$3"
  local new_line="$4"

  local tmpfile
  tmpfile=$(mktemp)

  awk -v sec="$section" -v key="$key" -v newline="$new_line" '
    BEGIN { in_sec=0; replaced=0 }
    /^[[:space:]]*\[/ {
      if (in_sec && !replaced) {
        print newline
        replaced=1
      }
      in_sec = ($0 ~ "^[[:space:]]*\\[" sec "\\][[:space:]]*$")
      print
      next
    }
    {
      if (in_sec && $0 ~ "^[[:space:]]*" key "[[:space:]]*=") {
        print newline
        replaced=1
      } else {
        print
      }
    }
    END {
      if (in_sec && !replaced) {
        print newline
      }
    }
  ' "$file" > "$tmpfile"

  mv "$tmpfile" "$file"
}

# Backup original input file once
cp "$SRC_INI" "${SRC_INI}.bak"

# Update [process] and [flow] in original input_chapsim.ini
replace_key_in_section "$SRC_INI" "process" "is_prerun"  "is_prerun= .true."
replace_key_in_section "$SRC_INI" "flow"    "initfl"      "initfl= 0"

# Read defaults from source file
current_irestart="$(get_ini_value "$SRC_INI" "flow" "irestartfrom")"
def_icase="$(get_ini_value "$SRC_INI" "domain" "icase")"
def_lxx="$(get_ini_value "$SRC_INI" "domain" "lxx")"
def_lyt="$(get_ini_value "$SRC_INI" "domain" "lyt")"
def_lyb="$(get_ini_value "$SRC_INI" "domain" "lyb")"
def_lzz="$(get_ini_value "$SRC_INI" "domain" "lzz")"
def_ncx="$(get_ini_value "$SRC_INI" "mesh" "ncx")"
def_ncy="$(get_ini_value "$SRC_INI" "mesh" "ncy")"
def_ncz="$(get_ini_value "$SRC_INI" "mesh" "ncz")"
def_istret="$(get_ini_value "$SRC_INI" "mesh" "istret")"
def_rstret="$(get_ini_value "$SRC_INI" "mesh" "rstret")"

# Basic checks
for var_name in current_irestart def_icase def_lxx def_lyt def_lyb def_lzz def_ncx def_ncy def_ncz def_istret def_rstret; do
  if [[ -z "${!var_name}" ]]; then
    echo "Error: failed to read ${var_name} from $SRC_INI" >&2
    exit 1
  fi
done

# Ask user for restart index
new_irestart="$(prompt_integer_with_default "Enter irestartfrom" "$current_irestart")"
replace_key_in_section "$SRC_INI" "flow" "irestartfrom" "irestartfrom= $new_irestart"

echo
echo "Updated $SRC_INI:"
echo "  - is_prerun= .true."
echo "  - initfl= 0"
echo "  - irestartfrom= $new_irestart"
echo "  - backup saved as ${SRC_INI}.bak"
echo
echo "Create $TGT_INI from current [domain] and [mesh] values."
echo "Press Enter to keep the current value shown in brackets."
echo
echo "icase will be copied directly from $SRC_INI and kept unchanged."
echo

new_lxx="$(prompt_with_default "Enter lxx" "$def_lxx")"
new_lyt="$(prompt_with_default "Enter lyt" "$def_lyt")"
new_lyb="$(prompt_with_default "Enter lyb" "$def_lyb")"
new_lzz="$(prompt_with_default "Enter lzz" "$def_lzz")"

echo
new_ncx="$(prompt_with_default "Enter ncx" "$def_ncx")"
new_ncy="$(prompt_with_default "Enter ncy" "$def_ncy")"
new_ncz="$(prompt_with_default "Enter ncz" "$def_ncz")"
new_istret="$(prompt_with_default "Enter istret" "$def_istret")"
new_rstret="$(prompt_with_default "Enter rstret" "$def_rstret")"

cat > "$TGT_INI" <<EOT
[domain]
icase= $def_icase
lxx= $new_lxx
lyt= $new_lyt
lyb= $new_lyb
lzz= $new_lzz

[mesh]
ncx= $new_ncx
ncy= $new_ncy
ncz= $new_ncz
istret= $new_istret
rstret= $new_rstret
EOT

echo
echo "Created $TGT_INI with the following content:"
echo "----------------------------------------"
cat "$TGT_INI"
echo "----------------------------------------"

cat <<'EOM'

==============================================
 Domain & Mesh Interpolation Workflow Guide
==============================================

Step 1: Run source case
  - Run your ORIGINAL case with the intended MPI configuration
  - Ensure the simulation finishes successfully

Step 2: Prepare interpolation data
  - In the source case, go to folder: 1_data/
  - Copy files: domain0_*

Step 3: Setup target case
  - In your NEW case, paste these files into: 1_data/
  - Rename files:
        domain0_*  ->  domain1_*

Step 4: Run target case
  - Run your NEW case
  - Make sure the runtime mesh matches the target mesh in input_chapsim_tgt.ini

==============================================

EOM
