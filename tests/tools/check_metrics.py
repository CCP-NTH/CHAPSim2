#!/usr/bin/env python3

import json
import os
import sys
import math

SAFE_REL_THRESHOLD = 1e-10

# Key in tolerances.json holding per-case overrides, keyed by case directory name.
# Needed because a few metrics are not expected to be near zero in every
# configuration, so one global tolerance cannot be right for all of them.
CASE_OVERRIDE_KEY = "cases"


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


# -------------------------------------------------
# Per-case tolerance overrides
# -------------------------------------------------
# The case name is the directory holding reference.json. Overrides replace the
# global entry for that metric outright rather than merging field by field, so a
# case can drop the absolute check (by supplying "rel" only) for a quantity that
# has no reason to be zero. Every override applied is printed, so a loosened
# tolerance can never pass silently.
case_overrides = tol.pop(CASE_OVERRIDE_KEY, {})
case_name = os.path.basename(os.path.dirname(os.path.abspath(sys.argv[2])))

for key, entry in case_overrides.get(case_name, {}).items():
    tol[key] = entry
    print(f"[TOLER] {key:35s} case override for '{case_name}': {entry}")


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
    checks = []
    failures = []

    # -------------------------
    # Absolute tolerance
    # -------------------------
    if abs_tol is not None:
        checks.append(f"abs={err:.2e}/{abs_tol:.2e}")
        if err > abs_tol:
            failures.append(f"abs {err:.2e} > {abs_tol:.2e}")

    # -------------------------
    # Relative tolerance
    # -------------------------
    if rel_tol is not None:
        if abs(ref_val) < SAFE_REL_THRESHOLD:
            checks.append("rel=skip(ref≈0)")
        else:
            rel_err = err / abs(ref_val)
            checks.append(f"rel={rel_err:.2e}/{rel_tol:.2e}")
            if rel_err > rel_tol:
                failures.append(f"rel {rel_err:.2e} > {rel_tol:.2e}")

    applicable_checks = [c for c in checks if not c.startswith("rel=skip")]

    if checks:
        print(
            f"[CHECK] {key:35s} "
            f"new={new_val:.6e} ref={ref_val:.6e} {' '.join(checks)}"
        )
    else:
        print(f"[SKIP ] {key:35s} (no applicable tolerance)")

    if not applicable_checks:
        return False

    if failures and len(failures) == len(applicable_checks):
        print(f"[FAIL ] {key}: " + "; ".join(failures))
        return True

    return False


# -------------------------------------------------
# Loop over metrics
# -------------------------------------------------
# A tolerance entry may carry "known_limitation": "<reason>". The reference value
# of such a metric is the documented output of the algorithm, not a physical
# expectation (e.g. mass in a sealed isobaric domain). It stays gated: a change
# beyond tolerance is still an unexpected solver regression, reported as such.
N_LIMIT = 0
for key, ref_val in ref.items():
    if key not in new:
        die(f"Missing metric in new results: '{key}'")

    if key not in tol:
        print(f"[SKIP ] {key:35s} (no tolerance defined)")
        continue

    reason = tol[key].get("known_limitation") if isinstance(tol[key], dict) else None
    if reason:
        N_LIMIT += 1
        print(f"[LIMIT] {key:35s} documented modelling limitation (reference = algorithm output, "
              f"not a physical expectation): {reason}")
    if check_metric(key, new[key], ref_val, tol[key]):
        if reason:
            print(f"[FAIL ] {key}: changed beyond tolerance - unexpected regression of a documented-limitation metric")
        FAILED = True

if N_LIMIT:
    print(f"\n{N_LIMIT} metric(s) carry a documented modelling limitation (see [LIMIT] lines)")


# -------------------------------------------------
# Final result
# -------------------------------------------------
if FAILED:
    die("One or more metrics exceeded tolerance")

print("\nMetrics OK")
sys.exit(0)
