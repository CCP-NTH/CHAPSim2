#!/bin/bash

# Attempt to start mkdocs serve in the background and capture the output
mkdocs serve &
MKDOCS_PID=$!

# Wait for a short moment to allow the server to start and potentially log the address
sleep 2

# Try to find the MkDocs server address in the logs (might vary slightly)
SERVER_ADDRESS=$(ps aux | grep "mkdocs serve" | grep -v grep | awk '{print $11}')

# If we found an address, open it in Chrome
if [ -n "$SERVER_ADDRESS" ]; then
  open -a "Google Chrome" "$SERVER_ADDRESS"
else
  echo "Warning: Could not reliably determine MkDocs server address. Please open your browser manually to http://127.0.0.1:8000 (or the address shown by mkdocs serve)."
fi

# Optional: Clean up the background process when the script exits
trap "kill $MKDOCS_PID" EXIT
