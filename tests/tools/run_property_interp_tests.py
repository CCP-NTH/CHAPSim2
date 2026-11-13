#!/usr/bin/env python3
"""Run the property-interpolation unit tests (tests/unit_property_interp).

The driver test_property_interp.f90 is linked against the solver's own objects
(obj/, build/ modules) and loads each shipped NIST table through the production
path (Buildup_thermo_mapping_relations), in both property_interp modes:

  * dense  : node values, 64 points per interval and points either side of every
             node, plus round trips h -> T -> h, T -> h -> T and the Bc path;
             checked by check_property_interp.py (node reproduction, agreement
             with an independent scipy PCHIP, C1 continuity in pchip mode and C0 in
             linear mode, monotonicity/positivity/boundedness, round trips, cp
             consistency, conditioning near the pseudo-critical point);
  * oor_h  : an enthalpy above the table must stop through the collective
             property-range check, in both modes;
  * oor_t  : a temperature below the table must stop in pchip mode (linear-mode
             from_T keeps its pre-existing extrapolation and is not gated here).

Run after ./build_chapsim.sh from the repository root. Needs mpif90/mpirun and
numpy + scipy. Exit status 0 only if every check passes.

A table that is not present in validation/thermal_properties/ is reported as
SKIP and not tested: the public distribution does not include NIST_CO2_8MP.DAT
(see validation/thermal_properties/README.md). The water table is required.
Usage: python3 tests/tools/run_property_interp_tests.py
"""
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
UNIT = ROOT / "tests" / "unit_property_interp"
TABLES = {"co2": ("NIST_CO2_8MP.DAT", 301.15), "water": ("NIST_WATER_23.5MP.DAT", 645.15)}


def run(cmd, cwd, expect_ok=True):
    p = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True)
    return p.returncode, p.stdout + p.stderr


def main() -> int:
    if not (ROOT / "bin" / "CHAPSim").is_file():
        print("FAIL: bin/CHAPSim not found - build the solver first")
        return 1
    rc, out = run(["bash", str(UNIT / "build_test.sh")], ROOT)
    driver = UNIT / "test_property_interp"
    if rc != 0 or not driver.is_file():
        print("FAIL: could not build the unit driver\n" + out[-2000:])
        return 1
    ok = True
    with tempfile.TemporaryDirectory(prefix="property_interp_") as tmp:
        tmp = Path(tmp)
        (tmp / "4_check").mkdir()          # the table loader writes its diagnostic tables here
        for fluid, (table, t0) in TABLES.items():
            src = ROOT / "validation" / "thermal_properties" / table
            if not src.is_file():
                if fluid == "water":
                    print(f"FAIL: {src} not found")
                    ok = False
                else:
                    print(f"SKIP {fluid}: {table} is not in validation/thermal_properties/ "
                          "(not distributed; see the README there)")
                continue
            shutil.copy(src, tmp / table)
            for mode in ("linear", "pchip"):
                rc, out = run(["mpirun", "-np", "1", str(driver), mode, fluid, "dense"], tmp)
                if rc != 0:
                    ok = False
                    print(f"FAIL dense {fluid} {mode}: driver exit {rc}\n{out[-1500:]}")
                for task in ("oor_h", "oor_t"):
                    if task == "oor_t" and mode == "linear":
                        continue
                    rc, out = run(["mpirun", "-np", "1", str(driver), mode, fluid, task], tmp)
                    stopped = (rc != 0 and "NOT STOPPED" not in out
                               and "thermodynamic state out of the property range" in out)
                    ok &= stopped
                    print(f"{'PASS' if stopped else 'FAIL'} {task} {fluid} {mode}: "
                          f"{'stopped with the range diagnostic' if stopped else 'did not stop'}")
            rc, out = run([sys.executable, str(UNIT / "check_property_interp.py"), fluid, str(t0)], tmp)
            print(out.rstrip())
            ok &= (rc == 0 and "ALL PASS" in out)
    driver.unlink(missing_ok=True)
    print("ALL PROPERTY-INTERPOLATION UNIT TESTS PASSED" if ok else "PROPERTY-INTERPOLATION UNIT TESTS FAILED")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
