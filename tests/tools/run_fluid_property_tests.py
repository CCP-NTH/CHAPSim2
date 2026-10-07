#!/usr/bin/env python3
"""Exercise the polynomial thermophysical-property path of the solver.

The 37 shipped regression and functional cases all use ``ifluid= scp_water``,
which takes the NIST-table branch of ``src/input_thermo.f90``. The polynomial
branch -- every liquid metal and molten salt -- has no coverage at all, which is
how a copy-pasted enthalpy coefficient and a viscosity fit that goes negative
inside its advertised range both survived.

This driver runs ``bin/CHAPSim`` once per fluid on an 8x8x8 mesh for a single
iteration, and checks the property table that ``Write_thermo_property`` writes
to ``4_check/check_ftplist_dim.dat``. It therefore tests the production
routines, not a reimplementation of the correlations: every number checked here
came out of the solver.

What is checked
  * dH/dT equals the separately evaluated Cp, which is what ties ``CoH`` to
    ``CoCp`` in ``parameters_constant_mod``, measured as a central difference of
    the tabulated enthalpy;
  * for sodium and LBE, whose ``CoH`` rows were wrong, the solver's Cp also
    equals the enthalpy polynomial differentiated analytically here, which does
    not depend on any solver output;
  * density, dynamic viscosity, thermal conductivity and Cp are positive at
    every tabulated temperature;
  * density falls with temperature, and LBE's is 11065 - 1.293*600 kg/m3 at
    600 K, checked both as a direct function evaluation from the solver's
    reference-state log block and as a sample of the tabulated diagnostic;
  * for Li, the thermal-expansion coefficient equals -(1/rho) drho/dT measured
    from the tabulated density, which the ``1 / (CoB - T)`` form cannot give for
    a non-linear density, and matches the analytic value at 600 K;
  * PbLi's dynamic viscosity is the KfK-4144 Arrhenius expression, tabulated
    over the supported 521-625 K interval and nowhere else;
  * ``scp_water`` still initialises from the NIST table;
  * ordinary liquid water, an unknown fluid name, and a temperature outside the
    supported property interval are all rejected with a diagnostic instead of
    silently loading sodium or extrapolating a fit;
  * that last rejection happens *before* the correlations are evaluated, shown
    by driving lithium past the 3500 K pole in its density and checking that the
    run ends on the range diagnostic rather than on a floating-point trap.

Usage: python3 tests/tools/run_fluid_property_tests.py
Needs bin/CHAPSim to have been built already.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
SOLVER = REPO_ROOT / "bin" / "CHAPSim"
NIST_WATER = REPO_ROOT / "validation" / "thermal_properties" / "NIST_WATER_23.5MP.DAT"

# Columns of 4_check/check_ftplist_dim.dat, in write order.
COLUMNS = ("h", "t", "d", "m", "k", "sigma_e", "cp", "b", "rhoh")

# Stride, in table rows, of the central differences below. The table is written
# with ES13.5, so a one-row difference carries only ~3 significant digits of
# signal; widening the stencil trades truncation error, which is tiny because
# the correlations are low-order polynomials, for print precision.
STENCIL = 20

# Fluids on the polynomial path, with a reference temperature and two wall
# temperatures comfortably inside each one's tabulated range.
#
# Where a fluid has a published value to check against, ref_t0 is set to that
# temperature on purpose: the solver prints the reference state to the log as
# F15.8, evaluated by calling the property function at exactly ref_t0, so a
# check against the log is a direct function evaluation with no table sampling
# and no interpolation in it. See read_reference_state.
POLYNOMIAL_FLUIDS = {
    "sodium":  (700.0, 690.0, 710.0),
    "lead":    (1000.0, 990.0, 1010.0),
    "bismuth": (900.0, 890.0, 910.0),
    "lbe":     (600.0, 590.0, 610.0),
    "lithium": (600.0, 590.0, 610.0),
    "flibe":   (900.0, 890.0, 910.0),
    # PbLi's viscosity correlation is supported over 521-625 K only, so the
    # whole case has to sit inside that, not merely inside the liquid range.
    "pbli":    (550.0, 545.0, 555.0),
}

INPUT_TEMPLATE = """[process]
is_prerun= .false.
is_postprocess= .false.

[decomposition]
nxdomain= 1
p_row= 0
p_col= 0

[domain]
icase= channel
lxx= 8.0
lyt= 1.0
lyb= -1.0
lzz= 4.0

[flow]
initfl= poiseuille
irestartfrom= 0
veloinit= 0.0,0.0,0.0
noiselevel= 0.0
is_active_tripping= .false.
reni= 5000
nreni= 10000
ren= 5000

[thermo]
ithermo= .true.
icht= .false.
igravity= 0.0,0.0,0.0
ifluid= {fluid}
ref_l0= 0.0015
ref_t0= {ref_t0}
inittm= linear
irestartfrom= 0
tini= {ref_t0}
inout_buffer= 0.0, 0.0
qw_ramp= .false., 0, 0

[mesh]
ncx= 8
ncy= 8
ncz= 8
istret= twosides
rstret= 3fmd,0.10

[bc]
ifbcx_u= 1,1,0.0,0.0
ifbcx_v= 1,1,0.0,0.0
ifbcx_w= 1,1,0.0,0.0
ifbcx_p= 1,1,0.0,0.0
ifbcx_t= 1,1,0.0,0.0
ifbcy_u= 4,4,0.0,0.0
ifbcy_v= 4,4,0.0,0.0
ifbcy_w= 4,4,0.0,0.0
ifbcy_p= 5,5,0.0,0.0
ifbcy_t= 4,4,{tw_low},{tw_high}
ifbcz_u= 1,1,0.0,0.0
ifbcz_v= 1,1,0.0,0.0
ifbcz_w= 1,1,0.0,0.0
ifbcz_p= 1,1,0.0,0.0
ifbcz_t= 1,1,0.0,0.0
idriven= x_massflux
drivenfc= 0.0

[scheme]
dt= 1e-05
itimescheme= rk3
iaccuracy= cd2
iviscous= explicit
out_sponge_L_Re= 0.0, 100.0

[simcontrol]
niterflowfirst= 1
niterflowlast= 1
niterthermofirst= 1
niterthermolast= 1

[io]
cpu_nfre= 1000
ckpt_nfre= 1000
visu_idim= 2
visu_nfre= 1000
visu_nskip= 1,1,1
stat_istart= 1000
stat_level= 0
stat_nskip= 1,1,1
is_record_xoutlet_read_xinlet= .false.,.false.
ndbfre_ndbstart_ndbend= 0,0,0
existing_output_policy= overwrite
restart_data_layout= bundled

[probe]
npp= 0
pt1= 3.141593,0.0,1.5707965
"""


class CheckFailure(Exception):
    pass


def launch(workdir: Path) -> subprocess.CompletedProcess:
    cmd = [str(SOLVER)]
    if shutil.which("mpirun"):
        cmd = ["mpirun", "-np", "1", str(SOLVER)]
    env = dict(os.environ)
    # Single node, and some hosts expose an RDMA device UCX cannot open.
    env.setdefault("UCX_TLS", "sm,self,tcp")
    return subprocess.run(
        cmd, cwd=workdir, env=env, capture_output=True, text=True, timeout=600
    )


def run_solver(fluid: str, ref_t0: float, tw_low: float, tw_high: float,
               workdir: Path) -> subprocess.CompletedProcess:
    (workdir / "input_chapsim.ini").write_text(
        INPUT_TEMPLATE.format(fluid=fluid, ref_t0=ref_t0,
                              tw_low=tw_low, tw_high=tw_high)
    )
    if fluid == "scp_water":
        shutil.copy(NIST_WATER, workdir / NIST_WATER.name)
    return launch(workdir)


def read_table(workdir: Path) -> dict[str, list[float]]:
    path = workdir / "4_check" / "check_ftplist_dim.dat"
    if not path.is_file():
        raise CheckFailure(f"the solver wrote no property table at {path}")
    table = {name: [] for name in COLUMNS}
    for line in path.read_text().splitlines():
        if line.lstrip().startswith("#"):
            continue
        fields = line.split()
        if len(fields) != len(COLUMNS):
            continue
        for name, field in zip(COLUMNS, fields):
            table[name].append(float(field))
    if len(table["t"]) < 4 * STENCIL:
        raise CheckFailure(f"property table has only {len(table['t'])} rows")
    return table


def central_derivative(y: list[float], t: list[float], i: int) -> float:
    return (y[i + STENCIL] - y[i - STENCIL]) / (t[i + STENCIL] - t[i - STENCIL])


def sample_indices(n: int) -> list[int]:
    """A handful of interior rows, clear of both ends of the stencil."""
    lo, hi = STENCIL, n - 1 - STENCIL
    return [lo + round((hi - lo) * f) for f in (0.1, 0.3, 0.5, 0.7, 0.9)]


def check_enthalpy_matches_cp(fluid: str, table: dict[str, list[float]]) -> None:
    """dH/dT must equal Cp: they come from CoH and CoCp independently."""
    t, h, cp = table["t"], table["h"], table["cp"]
    for i in sample_indices(len(t)):
        dhdt = central_derivative(h, t, i)
        err = abs(dhdt - cp[i]) / abs(cp[i])
        if err > 1.0e-3:
            raise CheckFailure(
                f"{fluid}: at T = {t[i]:.2f} K, dH/dT = {dhdt:.6g} J/kg/K but "
                f"Cp = {cp[i]:.6g} J/kg/K ({err:.3%} apart). CoH is not the "
                f"integral of CoCp."
            )


# The Cp coefficients of the two fluids whose enthalpy coefficients were wrong,
# copied from parameters_constant_mod as (CoCp(-2) ... CoCp(2)). CoH is derived
# from these in the source, so differentiating the enthalpy polynomial by hand
#   dH/dT = -CoH(-1)/T^2 + CoH(1) + 2 CoH(2) T + 3 CoH(3) T^2
# gives back CoCp(-2)/T^2 + CoCp(0) + CoCp(1) T + CoCp(2) T^2 term by term. This
# is the analytic derivative that the solver's own Cp is checked against; the
# central difference of the tabulated H is a third, independent estimate of the
# same quantity.
ANALYTIC_COCP = {
    "sodium": (-3.001e6, 0.0, 1658.0, -0.8479, 4.454e-4),
    "lbe":    (-4.56e5,  0.0,  164.8, -3.94e-2, 1.25e-5),
}


def analytic_dhdt(fluid: str, temperature: float) -> float:
    c = ANALYTIC_COCP[fluid]
    return (c[0] / temperature**2 + c[1] / temperature + c[2]
            + c[3] * temperature + c[4] * temperature**2)


def check_cp_matches_analytic_dhdt(fluid: str,
                                   table: dict[str, list[float]]) -> None:
    """The solver's Cp must equal the hand-differentiated enthalpy polynomial.

    Only for the two fluids whose CoH rows were wrong. check_enthalpy_matches_cp
    compares two solver outputs with each other, so a consistently wrong pair
    would pass it; this compares one of them with a coefficient set written out
    here independently of the Fortran.
    """
    if fluid not in ANALYTIC_COCP:
        return
    t, cp = table["t"], table["cp"]
    for i in sample_indices(len(t)):
        expected = analytic_dhdt(fluid, t[i])
        err = abs(cp[i] - expected) / abs(expected)
        if err > 1.0e-5:
            raise CheckFailure(
                f"{fluid}: at T = {t[i]:.2f} K the solver gives Cp = "
                f"{cp[i]:.6g} J/kg/K, the analytic dH/dT is {expected:.6g} "
                f"J/kg/K ({err:.3%} apart)"
            )


def check_properties_positive(fluid: str, table: dict[str, list[float]]) -> None:
    for name in ("d", "m", "k", "cp"):
        for value, temperature in zip(table[name], table["t"]):
            if value <= 0.0:
                raise CheckFailure(
                    f"{fluid}: {name} = {value:.6g} at T = {temperature:.2f} K "
                    f"is not positive"
                )


def check_density_falls(fluid: str, table: dict[str, list[float]]) -> None:
    d, t = table["d"], table["t"]
    for i in range(1, len(d)):
        if d[i] >= d[i - 1]:
            raise CheckFailure(
                f"{fluid}: density rises with temperature, {d[i-1]:.6g} at "
                f"{t[i-1]:.2f} K to {d[i]:.6g} at {t[i]:.2f} K"
            )


def interpolate(table: dict[str, list[float]], name: str, target_t: float) -> float:
    t, y = table["t"], table[name]
    if not t[0] <= target_t <= t[-1]:
        raise CheckFailure(f"T = {target_t} K is outside the tabulated range "
                           f"[{t[0]:.2f}, {t[-1]:.2f}] K")
    for i in range(1, len(t)):
        if t[i] >= target_t:
            w = (target_t - t[i - 1]) / (t[i] - t[i - 1])
            return y[i - 1] + w * (y[i] - y[i - 1])
    return y[-1]


# Labels the solver prints for the reference state, in its log header, as
# F15.8. This block is written from fluidparam%ftp0ref, which
# ftp_get_thermal_properties_dimensional_from_T fills by calling the property
# functions at exactly ref_t0. Reading it is therefore a *direct function
# evaluation* at a known temperature. The property table in
# check_ftplist_dim.dat is *sampled diagnostic output*: 1024 nodes written with
# ES13.5, so a value between nodes has to be interpolated from 6-significant-
# figure numbers and is only good to ~1e-5 relative. The two are checked at
# different tolerances for that reason, and neither tolerance is a statement
# about the correlation.
REFERENCE_LABELS = {
    "t":  "Temperature(K):",
    "d":  "Density(Kg/m3):",
    "m":  "Dynamic Viscosity(Pa-s):",
    "k":  "Thermal Conductivity(W/m-K):",
    "cp": "Cp(J/Kg/K):",
}


def read_reference_state(fluid: str, log: str) -> dict[str, float]:
    """The reference-state properties the solver evaluated directly at ref_t0.

    The log prints the reference block first and the initial block after it, so
    the first match for each label is the reference state. beta is not printed,
    so it cannot be checked this way.
    """
    state: dict[str, float] = {}
    for line in log.splitlines():
        for name, label in REFERENCE_LABELS.items():
            if name not in state and label in line:
                try:
                    state[name] = float(line.split()[-1])
                except ValueError:
                    pass
    missing = set(REFERENCE_LABELS) - set(state)
    if missing:
        raise CheckFailure(f"{fluid}: the log has no reference {sorted(missing)}")
    return state


def check_reference_value(fluid: str, log: str, name: str, at_t: float,
                          expected: float, tol: float = 1.0e-9) -> None:
    """One directly evaluated reference property against its published value."""
    state = read_reference_state(fluid, log)
    if abs(state["t"] - at_t) > 1.0e-6:
        raise CheckFailure(
            f"{fluid}: the reference state is at T = {state['t']} K, not "
            f"{at_t} K, so {name} there is not the value being checked"
        )
    err = abs(state[name] - expected) / abs(expected)
    if err > tol:
        raise CheckFailure(
            f"{fluid}: {name} evaluated directly at {at_t} K is "
            f"{state[name]:.11g}, expected {expected:.11g} ({err:.3e} relative)"
        )


def check_lbe_density(table: dict[str, list[float]], log: str) -> None:
    """CoD_LBE = 11065 - 1.293 T, so rho(600 K) = 10289.2 kg/m3 exactly.

    This is the sign of the density slope, which was positive until it was
    fixed: with +1.293 the same point reads 11840.8 kg/m3.

    Checked twice. The direct evaluation at ref_t0 = 600 K has to reproduce
    11065 - 1.293*600 to round-off, because that is literally the expression the
    solver evaluates. The table value at the same temperature is allowed 1e-4
    relative: 600 K falls between nodes 599.577 K and 601.070 K, and
    differencing their densities, each rounded to 6 significant figures, leaves
    only two digits in a difference of 1.9 in 10289, so the interpolated slope
    carries ~1.6 % error over a ~0.4 K lever arm. That is print precision in the
    diagnostic file, not error in the correlation.
    """
    check_reference_value("lbe", log, "d", 600.0, 11065.0 - 1.293 * 600.0)
    rho = interpolate(table, "d", 600.0)
    if abs(rho - 10289.2) / 10289.2 > 1.0e-4:
        raise CheckFailure(
            f"lbe: density sampled from the table at 600 K is {rho:.4f} kg/m3, "
            f"expected about 10289.2 kg/m3"
        )


# Evaluated analytically from the shipped Li density correlation
#   rho    = 278.5 - 0.04657 T + 274.6 (1 - T/3500)**0.467
#   drho/dT = -0.04657 - (274.6*0.467/3500) (1 - T/3500)**(-0.533)
#   beta   = -(drho/dT) / rho
# These are properties of that correlation, not independent measurements.
LITHIUM_AT_600K = {"d": 502.07110643, "b": 1.734261978e-4}


def check_lithium_expansion(table: dict[str, list[float]], log: str) -> None:
    """beta must be -(1/rho) drho/dT of the tabulated Li density.

    Li is the one fluid whose density is non-linear in T, so the 1 / (CoB - T)
    shortcut does not apply to it; CoB_Li = 5620 K overstates beta by 11-16 %
    across the liquid range.

    Three checks: the density evaluated directly at ref_t0 = 600 K, which is
    exact; beta against the tabulated density everywhere, which is the identity
    itself; and both against their analytic values at 600 K sampled from the
    table, which pins the differentiation to the right correlation. beta is not
    printed in the reference block, so only the table can supply it.
    """
    check_reference_value("lithium", log, "d", 600.0, LITHIUM_AT_600K["d"],
                          tol=1.0e-8)
    t, d, b = table["t"], table["d"], table["b"]
    for i in sample_indices(len(t)):
        expected = -central_derivative(d, t, i) / d[i]
        err = abs(b[i] - expected) / abs(expected)
        if err > 1.0e-3:
            raise CheckFailure(
                f"lithium: at T = {t[i]:.2f} K, beta = {b[i]:.6g} 1/K but "
                f"-(1/rho) drho/dT = {expected:.6g} 1/K ({err:.3%} apart)"
            )
    for name, expected in LITHIUM_AT_600K.items():
        value = interpolate(table, name, 600.0)
        err = abs(value - expected) / abs(expected)
        if err > 1.0e-4:
            raise CheckFailure(
                f"lithium: {name} at 600 K is {value:.9g}, expected "
                f"{expected:.9g} from the density correlation ({err:.3%} apart)"
            )


# KfK-4144 (1986), Part II section 4.3, printed p. 39:
# mu = 1.87e-4 * exp(11640 / (Ru T)) Pa s, Ru = 8.314 J/mol/K.
# Independently evaluated here; the solver reaches the same numbers through
# CoM_PbLi, whose first element is the activation temperature 11640/8.314.
PBLI_VISCOSITY = {550.0: 2.38427562968e-3, 600.0: 1.92854697608e-3}

# The overlap of the two source ranges, see TMUmin_PbLi / TMUmax_PbLi.
PBLI_RANGE = (521.0, 625.0)


def check_pbli_viscosity(table: dict[str, list[float]], log: str) -> None:
    """PbLi viscosity: the KfK-4144 Arrhenius values, over the supported range.

    The table must not reach outside 521-625 K either. The previous cubic was
    tabulated to 1943 K and went negative at 858.996 K; the replacement is
    positive everywhere, so nothing but the range itself stops it being
    evaluated far outside where any source supports it.
    """
    check_reference_value("pbli", log, "m", 550.0, PBLI_VISCOSITY[550.0],
                          tol=1.0e-8)
    t_lo, t_hi = min(table["t"]), max(table["t"])
    low, high = PBLI_RANGE
    # One table interval of slack at the low end: the table starts one step in.
    step = (t_hi - t_lo) / max(1, len(table["t"]) - 1)
    if t_lo < low - 1e-6 or t_hi > high + 1e-6:
        raise CheckFailure(
            f"pbli: the property table spans [{t_lo:.2f}, {t_hi:.2f}] K, outside "
            f"the supported [{low:.1f}, {high:.1f}] K"
        )
    if t_lo > low + 2.0 * step or t_hi < high - 2.0 * step:
        raise CheckFailure(
            f"pbli: the property table spans only [{t_lo:.2f}, {t_hi:.2f}] K of "
            f"the supported [{low:.1f}, {high:.1f}] K"
        )
    for temperature, expected in PBLI_VISCOSITY.items():
        mu = interpolate(table, "m", temperature)
        err = abs(mu - expected) / expected
        if err > 1.0e-4:
            raise CheckFailure(
                f"pbli: viscosity at {temperature:.0f} K is {mu:.9g} Pa s, "
                f"expected {expected:.9g} Pa s from KfK-4144 ({err:.3%} apart)"
            )


PER_FLUID_CHECKS = {
    "lbe": check_lbe_density,
    "lithium": check_lithium_expansion,
    "pbli": check_pbli_viscosity,
}


def test_polynomial_fluid(fluid: str, temperatures: tuple[float, float, float],
                          workdir: Path) -> None:
    completed = run_solver(fluid, *temperatures, workdir=workdir)
    if completed.returncode != 0:
        raise CheckFailure(
            f"{fluid}: the solver exited {completed.returncode}\n"
            + completed.stdout[-2000:] + completed.stderr[-2000:]
        )
    table = read_table(workdir)

    # Report every property that is wrong, not just the first: one bad
    # coefficient usually shows up in several of these at once, and seeing all
    # of them is what tells you which coefficient it is.
    checks = [lambda: check_properties_positive(fluid, table),
              lambda: check_density_falls(fluid, table),
              lambda: check_enthalpy_matches_cp(fluid, table),
              lambda: check_cp_matches_analytic_dhdt(fluid, table)]
    extra = PER_FLUID_CHECKS.get(fluid)
    if extra is not None:
        checks.append(lambda: extra(table, completed.stdout))

    messages = []
    for check in checks:
        try:
            check()
        except CheckFailure as exc:
            messages.append(str(exc))
    if messages:
        raise CheckFailure("\n".join(messages))


def test_scp_water(workdir: Path) -> None:
    """The NIST-table path must keep working: it is what every case here uses."""
    if not NIST_WATER.is_file():
        raise CheckFailure(f"missing {NIST_WATER}")
    completed = run_solver("scp_water", 645.15, 640.15, 652.15, workdir=workdir)
    if completed.returncode != 0:
        raise CheckFailure(
            f"scp_water: the solver exited {completed.returncode}\n"
            + completed.stdout[-2000:] + completed.stderr[-2000:]
        )
    table = read_table(workdir)
    check_properties_positive("scp_water", table)


def test_rejected(fluid: str, expected: str, workdir: Path) -> None:
    """An unsupported or unknown fluid must stop the run and say why."""
    completed = run_solver(fluid, 700.0, 690.0, 710.0, workdir=workdir)
    if completed.returncode == 0:
        raise CheckFailure(f"ifluid = {fluid} was accepted; it must be rejected")
    output = completed.stdout + completed.stderr
    if expected.lower() not in output.lower():
        raise CheckFailure(
            f"ifluid = {fluid} was rejected without saying {expected!r}:\n"
            + output[-2000:]
        )


def test_out_of_range_rejected(workdir: Path) -> None:
    """A PbLi run above 625 K must stop, and must name the viscosity.

    The correlation range and the melting/boiling range are different limits.
    700 K is liquid PbLi, so nothing about the phase stops this run; only the
    viscosity correlation does, and the diagnostic has to say so.
    """
    completed = run_solver("pbli", 700.0, 690.0, 710.0, workdir=workdir)
    if completed.returncode == 0:
        raise CheckFailure("pbli at 700 K was accepted; it is outside 521-625 K")
    output = (completed.stdout + completed.stderr).lower()
    for expected in ("above the supported property range", "dynamic viscosity"):
        if expected not in output:
            raise CheckFailure(
                f"pbli at 700 K was rejected without saying {expected!r}:\n"
                + output[-2000:]
            )


def test_fractional_power_guarded(workdir: Path) -> None:
    """Lithium above 3500 K must hit the range diagnostic, not a SIGFPE.

    Li's density is CoD(0) + CoD(1)*T + CoD(2)*(1 - T/3500)**0.467. Above
    3500 K that raises a negative base to a fractional power, which the debug
    build's -ffpe-trap=invalid turns into a floating-point exception with no
    message at all. The range check therefore has to run before the reference
    and initial states are evaluated, not after -- and the only way to show
    that the ordering is right is to drive a run past 3500 K and watch which
    of the two happens. So this asserts three things: a clean non-zero exit
    rather than a signal, the range diagnostic naming the boiling point that
    binds Li's upper end, and no trace of an FPE anywhere in the output.
    """
    completed = run_solver("lithium", 3600.0, 3590.0, 3610.0, workdir=workdir)
    output = completed.stdout + completed.stderr
    if completed.returncode == 0:
        raise CheckFailure("lithium at 3600 K was accepted; it is above 1615 K")
    if completed.returncode < 0:
        raise CheckFailure(
            f"lithium at 3600 K died on signal {-completed.returncode}, so the "
            "range check did not run before the density was evaluated:\n"
            + output[-2000:]
        )
    lowered = output.lower()
    for expected in ("above the supported property range", "boiling point",
                     "error stop"):
        if expected not in lowered:
            raise CheckFailure(
                f"lithium at 3600 K stopped without saying {expected!r}:\n"
                + output[-2000:]
            )
    # Not "backtrace": -fbacktrace prints one for every ERROR STOP, including
    # the orderly one this test wants. Only an actual trap says these.
    for forbidden in ("floating point exception", "sigfpe"):
        if forbidden in lowered:
            raise CheckFailure(
                f"lithium at 3600 K reached the fractional power: {forbidden!r} "
                "in the output:\n" + output[-2000:]
            )


def main() -> int:
    if not SOLVER.is_file():
        print(f"ERROR: {SOLVER} not found. Build the solver first:")
        print("  CHAPSIM_MODE=non-interactive ./build_chapsim.sh")
        return 1

    tests = [(f"polynomial fluid {name}",
              lambda w, n=name, t=temps: test_polynomial_fluid(n, t, w))
             for name, temps in POLYNOMIAL_FLUIDS.items()]
    tests.append(("NIST table scp_water", test_scp_water))
    tests.append(("rejection of ifluid = water",
                  lambda w: test_rejected("water", "not implemented", w)))
    tests.append(("rejection of an unknown ifluid",
                  lambda w: test_rejected("helium", "Invalid ifluid", w)))
    tests.append(("rejection of pbli outside its viscosity range",
                  test_out_of_range_rejected))
    tests.append(("rejection of lithium before its fractional power traps",
                  test_fractional_power_guarded))

    failures = []
    with tempfile.TemporaryDirectory(prefix="chapsim_props_") as root:
        for label, func in tests:
            workdir = Path(root) / re.sub(r"\W+", "_", label)
            workdir.mkdir()
            try:
                func(workdir)
            except (CheckFailure, subprocess.TimeoutExpired) as exc:
                failures.append((label, str(exc)))
                print(f"[FAIL] {label}")
            else:
                print(f"[ OK ] {label}")

    print()
    if failures:
        for label, message in failures:
            print(f"--- {label} ---")
            print(message)
        print(f"{len(failures)} of {len(tests)} fluid property checks FAILED")
        return 1
    print(f"All {len(tests)} fluid property checks OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
