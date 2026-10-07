#!/usr/bin/env python3
"""Check the SGS metric gates cannot be published without having sampled.

Four of the five subgrid gates are referenced at exactly 0.0, because a correct
wall closure gives zero and a periodic case has no wall or inlet/outlet at all.
A zero therefore cannot distinguish "sampled and found zero" from "never
sampled", so losing the instrumentation would read green. `io_monitor.f90`
counts recorder invocations and aborts when a gate was never reached; this
checks that wiring is still in place.
"""

from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "src" / "io_monitor.f90"


def require(condition, message):
    if not condition:
        raise SystemExit(f"SGS gate coverage: {message}")


text = SRC.read_text()

COUNTERS = {
    "n_sgs_coef_face_calls": "record_sgs_coef_face",
    "n_sgs_visc_face_calls": "record_sgs_visc_face",
    "n_sgs_coef_bc_calls": "record_sgs_coef_bc",
    "n_sgs_enth_flux_calls": "record_sgs_enthalpy_flux",
}

for counter, recorder in COUNTERS.items():
    require(
        f"integer,  save :: {counter}" in text or f"integer, save :: {counter}" in text,
        f"counter {counter} is not declared",
    )
    require(
        f"{counter} = {counter} + 1" in text,
        f"{recorder} does not increment {counter}",
    )

require(
    "MPI_INTEGER, MPI_MAX" in text,
    "coverage counters are not reduced with MPI_MAX across ranks",
)
require(
    "crbuf" in text and text.count("call Print_error_msg") >= 4,
    "missing coverage counts do not raise an error",
)
for n in range(1, 4):
    require(
        f"if(crbuf({n}) == 0) call Print_error_msg" in text,
        f"coverage counter {n} is reduced but never checked",
    )
require(
    "if(is_thermo .and. crbuf(4) == 0) call Print_error_msg" in text,
    "the enthalpy-flux gate is not required to have sampled in a thermal run",
)
require(
    "call reduce_sgs_diagnostics(metrics, dm%is_thermo)" in text,
    "reduce_sgs_diagnostics is not called with the thermal flag",
)

print("SGS gate coverage checks OK")
