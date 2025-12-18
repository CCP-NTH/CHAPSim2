#!/usr/bin/env python3

import json
import sys

SAFE_REL_THRESHOLD = 1e-12  # below this, relative tolerance is ignored


def die(msg):
    raise SystemExit(f"\nFAILED: {msg}\n")


# -------------------------------------------------
# Load JSON files
# -------------------------------------------------
try:
    new = json.load(open(sys.argv[1]))
    ref = json.load(open(sys.argv[2]))
    tol = json.load(open(sys.argv[3]))
except Exception as e:
    die(f"Failed to load JSON files: {e}")

FAILED = False


# -------------------------------------------------
# Helper: check one metric
# -------------------------------------------------
def check_metric(key, new_val, ref_val, tol_entry):
    """
    Returns True if metric fails tolerance, False otherwise
    """
    err = abs(new_val - ref_val)

    # -------------------------
    # Absolute tolerance (if defined)
    # -------------------------
    abs_tol = tol_entry.get("abs")
    if abs_tol is not None:
        print(f"[CHECK] {key:35s} new={new_val:.6e} ref={ref_val:.6e} abs={err:.2e}")
        if err > abs_tol:
            print(f"[FAIL ] {key}: abs {err:.2e} > {abs_tol:.2e}")
            return True

    # -------------------------
    # Relative tolerance (with safe fallback)
    # -------------------------
    rel_tol = tol_entry.get("rel")
    if rel_tol is not None:
        if abs(ref_val) >= SAFE_REL_THRESHOLD:
            rel_err = err / abs(ref_val)
            print(
                f"[CHECK] {key:35s} new={new_val:.6e} ref={ref_val:.6e} rel={rel_err:.2e}"
            )
            if rel_err > rel_tol:
                print(f"[FAIL ] {key}: rel {rel_err:.2e} > {rel_tol:.2e}")
                return True
        else:
            # ref ≈ 0 → fall back to absolute comparison using rel_tol
            abs_err = err
            print(
                f"[CHECK] {key:35s} new={new_val:.6e} ref={ref_val:.6e} "
                f"abs={abs_err:.2e} (ref ~ 0, abs fallback)"
            )
            if abs_err > rel_tol:
                print(f"[FAIL ] {key}: abs {abs_err:.2e} > {rel_tol:.2e}")
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

    failed = check_metric(
        key=key,
        new_val=new[key],
        ref_val=ref_val,
        tol_entry=tol[key],
    )

    if failed:
        FAILED = True


# -------------------------------------------------
# Final result
# -------------------------------------------------
if FAILED:
    die("One or more metrics exceeded tolerance")

print("\nMetrics OK")
sys.exit(0)
