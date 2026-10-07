#!/bin/bash
set -e

cd "$(dirname "$0")"

if ! command -v mkdocs >/dev/null 2>&1; then
  echo "mkdocs is not installed. Opening the static guidance index instead."
  if command -v google-chrome >/dev/null 2>&1; then
    google-chrome index.html
  elif command -v google-chrome-stable >/dev/null 2>&1; then
    google-chrome-stable index.html
  elif command -v chromium >/dev/null 2>&1; then
    chromium index.html
  else
    echo "Open docs/guidance/index.html in your browser."
  fi
  exit 0
fi

mkdocs serve &
MKDOCS_PID=$!

sleep 2

if command -v google-chrome >/dev/null 2>&1; then
  google-chrome http://127.0.0.1:8000
elif command -v google-chrome-stable >/dev/null 2>&1; then
  google-chrome-stable http://127.0.0.1:8000
elif command -v chromium >/dev/null 2>&1; then
  chromium http://127.0.0.1:8000
else
  echo "Open http://127.0.0.1:8000 in your browser."
fi

trap "kill $MKDOCS_PID" EXIT
wait "$MKDOCS_PID"
