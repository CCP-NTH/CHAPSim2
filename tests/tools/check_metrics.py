#!/usr/bin/env python3

import json
import sys
import math

SAFE_REL_THRESHOLD = 1e-10


def die(msg):
    raise SystemExit(f"\nFAILED: {msg}\n")


# -------------------------------------------------
# Load JSON files safely
# -------------------------------------------------
try:
    with open(sys.argv[1]) as f:
        new = json.load(f)
    with open(sys.argv[2]) as f:
        ref = json.load(f)
    with open(sys.argv[3]) as f:
        tol = json.load(f)
except Exception as e:
    die(f"Failed to load JSON files: {e}")


FAILED = False


# -------------------------------------------------
# Helper: check one metric
# -------------------------------------------------
def check_metric(key, new_val, ref_val, tol_entry):
    try:
        new_val = float(new_val)
        ref_val = float(ref_val)
    except Exception:
        print(f"[FAIL ] {key}: non-numeric value")
        return True

    if not math.isfinite(new_val) or not math.isfinite(ref_val):
        print(f"[FAIL ] {key}: NaN or Inf detected")
        return True

    err = abs(new_val - ref_val)

    abs_tol = tol_entry.get("abs")
    rel_tol = tol_entry.get("rel")

    # -------------------------
    # Absolute tolerance
    # -------------------------
    if abs_tol is not None:
        print(
            f"[CHECK] {key:35s} "
            f"new={new_val:.6e} ref={ref_val:.6e} abs={err:.2e}"
        )
        if err > abs_tol:
            print(f"[FAIL ] {key}: abs {err:.2e} > {abs_tol:.2e}")
            return True

    # -------------------------
    # Relative tolerance
    # -------------------------
    if rel_tol is not None:
        if abs(ref_val) < SAFE_REL_THRESHOLD:
            print(
                f"[SKIP ] {key:35s} "
                f"relative check skipped (ref ≈ 0)"
            )
        else:
            rel_err = err / abs(ref_val)
            print(
                f"[CHECK] {key:35s} "
                f"new={new_val:.6e} ref={ref_val:.6e} rel={rel_err:.2e}"
            )
            if rel_err > rel_tol:
                print(f"[FAIL ] {key}: rel {rel_err:.2e} > {rel_tol:.2e}")
                return True

    return False


# -------------------------------------------------
# Loop over metrics
# -------------------------------------------------
for key, ref_val in ref.items():
    if key not in new:
        die(f"Missing metric in new results: '{key}'")

    if key not in tol:
        print(f"[SKIP ] {key:35s} (no tolerance defined)")
        continue

    if check_metric(key, new[key], ref_val, tol[key]):
        FAILED = True


# -------------------------------------------------
# Final result
# -------------------------------------------------
if FAILED:
    die("One or more metrics exceeded tolerance")

print("\nMetrics OK")
sys.exit(0)