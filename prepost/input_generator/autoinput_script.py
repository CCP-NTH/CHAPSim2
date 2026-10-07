#!/usr/bin/env python3
"""
CHAPSim2 Auto Input Generator - Streamlines creation of CFD configuration files.

This utility generates structured input files for the CHAPSim2 DNS solver,
eliminating manual file creation errors and enhancing simulation reproducibility.
"""

import configparser
import math
from enum import Enum
from pathlib import Path

# Constants
MESSAGE_SEP = "=" * 22
DEFAULT_FILENAME = "input_chapsim_auto.ini"
SCRIPT_DIR = Path(__file__).resolve().parent
TEMPLATE_PATH = SCRIPT_DIR / "input_chapsim_complete.ini"
PI = round(math.pi, 6)
TWO_PI = 2.0 * PI
DEFAULT_PROBE_COUNT = 5
DEFAULT_VISU_SKIP = "1,1,1"
DEFAULT_STAT_SKIP = "1,1,1"
YES_NO_CHOICES = [0, 1]
SIGN_CHOICES = [1, -1]
INLET_BC_CHOICES = [4, 9, 10]
STAT_LEVEL_CHOICES = [0, 1, 2, 3]
OUTPUT_POLICY_CHOICES = ["overwrite", "skip", "rename_existing"]
RESTART_LAYOUT_CHOICES = ["per_field", "bundled"]
WALL_BC_CASES = {1, 3, 5}
PIPELIKE_CASES = {2, 3}
WALL_FLOW_CASES = {1, 2, 3, 5}

# Global state
icase = ithermo = iinlet = imhd = 0
has_convective_outlet = False


class Case(Enum):
    CHANNEL = 1
    PIPE = 2
    ANNULAR = 3
    TGV3D = 4
    DUCT = 5


class Drvfc(Enum):
    NONE = 0
    XMFLUX = 1
    XTAUW = 2
    XDPDX = 3
    ZMFLUX = 4
    ZTAUW = 5
    ZDPDZ = 6


class Init(Enum):
    RESTART = 0
    RANDOM = 2
    INLET = 3
    GIVEN = 4
    POISEUILLE = 5
    FUNCTION = 6
    GVBCLN = 7
    GVBCSMOOTH = 8


class Stretching(Enum):
    NONE = 0
    CENTRE = 1
    SIDE2 = 2
    BOTTOM = 3
    TOP = 4


class BC(Enum):
    INTERIOR = 0
    PERIODIC = 1
    SYMM = 2
    ASYMM = 3
    DIRICHLET = 4
    NEUMANN = 5
    INTRPL = 6
    CONVOL = 7
    TURGEN = 8
    PROFL = 9
    DATABS = 10
    PARABOLIC = 11
    OTHERS = 12


def reset_runtime_state():
    """Reset module-level state before collecting a new configuration."""
    global icase, ithermo, iinlet, imhd, has_convective_outlet
    icase = ithermo = iinlet = imhd = 0
    has_convective_outlet = False


def ensure_template_available():
    """Return the moved complete input template path, failing clearly if absent."""
    if not TEMPLATE_PATH.exists():
        raise FileNotFoundError(f"Template input file not found: {TEMPLATE_PATH}")
    return TEMPLATE_PATH


# Input Functions
def get_input(prompt, default=None, dtype=str, valid_choices=None):
    """Prompts user for input with validation and type conversion."""
    while True:
        user_input = input(f"{prompt} [{default}]: ").strip()

        if not user_input:
            return default

        try:
            converted_value = dtype(user_input)

            if valid_choices and converted_value not in valid_choices:
                print(f"❌ Invalid input. Choose from: {valid_choices}")
                continue

            return converted_value

        except ValueError:
            print(f"❌ Invalid input. Please enter a valid {dtype.__name__}.")


def get_yes_no(prompt, default=1):
    """Prompts for yes/no response (1=yes, 0=no)."""
    return get_input(prompt, default=default, dtype=int, valid_choices=YES_NO_CHOICES)


def get_sign(prompt, default=1):
    """Prompts for sign value (1 or -1)."""
    return get_input(prompt, default=default, dtype=int, valid_choices=SIGN_CHOICES)


def bool_to_string(value):
    """Converts 0/1 to Fortran boolean strings."""
    return ".true." if value else ".false."


def format_csv(*values):
    """Return values as a comma-separated string for the ini output."""
    return ",".join(str(value) for value in values)


def new_bc_entry():
    """Create a fresh default boundary-condition entry."""
    return [BC.PERIODIC.value, BC.PERIODIC.value, 0.0, 0.0]


def format_bc_entry(entry):
    """Format a boundary-condition entry for the ini output."""
    return format_csv(*entry)


def copy_velocity_bc_to_thermal(bc_dict, thermal_keys):
    """Copy velocity BCs to thermal BCs for non-thermal runs."""
    for key in thermal_keys:
        u_key = key.replace("_T", "_u")
        bc_dict[key] = bc_dict[u_key].copy()


def build_probe_point(x_coord, y_coord, z_coord):
    """Format a probe point triplet."""
    return format_csv(x_coord, y_coord, z_coord)


# Settings Functions
def get_process_settings():
    """Process configuration settings."""
    print(f"{MESSAGE_SEP} PROCESS {MESSAGE_SEP}")
    is_prerun = get_yes_no("Enable prerun only? (0:No, 1:Yes)", default=0)
    is_postprocess = get_yes_no("Enable postprocess? (0:No, 1:Yes)", default=0)

    return {
        "is_prerun": bool_to_string(is_prerun),
        "is_postprocess": bool_to_string(is_postprocess),
    }


def get_decomp_settings():
    """Domain decomposition settings."""
    print(f"{MESSAGE_SEP} DECOMPOSITION {MESSAGE_SEP}")
    is_decomp = get_yes_no(
        "Using automatic domain decomposition? (0:No, 1:Yes)", default=1
    )
    if is_decomp == 0:
        p_row = get_input("Subdomain division along Y direction", 0, int)
        p_col = get_input("Subdomain division along Z direction", 0, int)
    else:
        p_row, p_col = 0, 0

    return {"nxdomain": 1, "p_row": p_row, "p_col": p_col}


def get_domain_settings():
    """Domain geometry settings."""
    global icase
    print(f"{MESSAGE_SEP} DOMAIN {MESSAGE_SEP}")

    icase = get_input(
        "Simulation case (1:Channel, 2:Pipe, 3:Annular, 4:TGV3D, 5:DUCT)", 1, int
    )

    # Streamwise length
    if icase == Case.TGV3D.value:
        lxx = TWO_PI
    elif icase == Case.DUCT.value:
        lxx = get_input("Spanwise length (Lx/h)", 2.0, float)
    else:
        lxx = get_input("Streamwise length (Lx/h)", TWO_PI, float)

    # Spanwise length
    if icase == Case.TGV3D.value:
        lzz = TWO_PI
    elif icase in PIPELIKE_CASES:
        lzz = TWO_PI
    elif icase == Case.DUCT.value:
        lzz = get_input("Streamwise length (Lz/h)", 12.0, float)
    else:
        lzz = get_input("Spanwise length (Lz/h)", PI, float)

    # Vertical/radial bounds
    if icase in {Case.CHANNEL.value, Case.DUCT.value}:
        lyt, lyb = 1.0, -1.0
    elif icase == Case.PIPE.value:
        lyt, lyb = 1.0, 0.0
    elif icase == Case.TGV3D.value:
        lyt, lyb = PI, -PI
    else:
        lyb = get_input("Vertical/radial bottom boundary", -1.0, float)
        lyt = (
            1.0
            if icase == Case.ANNULAR.value
            else get_input("Vertical/radial top boundary", 1.0, float)
        )

    return {"icase": icase, "lxx": lxx, "lyt": lyt, "lyb": lyb, "lzz": lzz}


def get_flow_settings():
    """Flow initialization and Reynolds number settings."""
    global icase
    print(f"{MESSAGE_SEP} FLOW {MESSAGE_SEP}")

    is_restart = get_yes_no("Flow restart? (0:No, 1:Yes)", default=0)
    initfl = irestartfrom = 0
    noiselevel = 0.0
    velo1, velo2, velo3 = 0.0, 0.0, 0.0

    if is_restart == 1:
        initfl = Init.RESTART.value
        irestartfrom = get_input("From which iteration to restart", 2000, int)
    else:
        if icase in WALL_FLOW_CASES:
            initfl = get_input(
                "Flow initialization (2:Random, 3:Inlet, 4:Given, 5:Poiseuille, 6:Function)",
                Init.POISEUILLE.value,
                int,
                valid_choices=[
                    Init.RANDOM.value,
                    Init.INLET.value,
                    Init.GIVEN.value,
                    Init.POISEUILLE.value,
                    Init.FUNCTION.value,
                ],
            )
        elif icase == Case.TGV3D.value:
            initfl = Init.FUNCTION.value
        else:
            initfl = get_input(
                "Flow initialization (0:Restart, 2:Random, 3:Inlet, 4:Given, 5:Poiseuille, 6:Function)",
                5,
                int,
            )
        if initfl == Init.GIVEN.value:
            velo1 = get_input("Initial velocity in x", 1.0, float)
            velo2 = get_input("Initial velocity in y", 0.0, float)
            velo3 = get_input("Initial velocity in z", 0.0, float)

        if icase != Case.TGV3D.value:
            noiselevel = get_input(
                "Random fluctuation intensity (0.0-1.0)", 0.25, float
            )

    ren = get_input(
        "Reynolds number (bulk, half channel height/radius based)", 2800, int
    )

    if icase == Case.TGV3D.value:
        reni, nreni = ren, 0
    else:
        reni = get_input("Initial Reynolds number", 20000, int)
        nreni = get_input("Iterations for initial Re", 10000, int)

    return {
        "initfl": initfl,
        "irestartfrom": irestartfrom,
        "veloinit": format_csv(velo1, velo2, velo3),
        "noiselevel": noiselevel,
        "reni": reni,
        "nreni": nreni,
        "ren": ren,
    }


def get_thermo_settings():
    """Thermal field settings."""
    global ithermo
    print(f"{MESSAGE_SEP} THERMO {MESSAGE_SEP}")

    ithermo = get_yes_no("Enable thermal field? (0:No, 1:Yes)", default=0)
    if ithermo != 1:
        return None

    icht = get_yes_no("Enable conjugate heat transfer? (0:No, 1:Yes)", default=0)
    igravity = get_input(
        "Gravity direction (0:None, 1:+X, -1:-X, 2:+Y, -2:-Y, 3:+Z, -3:-Z)", 0, int
    )
    ifluid = get_input(
        "Fluid type (1:scp-H2O, 2:scp-CO2, 3:sodium, 4:lead, 5:bismuth, 6:LBE)", 1, int
    )
    refl0 = get_input("Reference length (meter)", 0.001, float)
    refT0 = get_input("Reference Temperature (Kelvin)", 645.15, float)
    inittm = get_input(
        "Thermal initialization (0:Restart, 4:Const, 6:Function, 7:Linear, 8:Smooth)",
        4,
        int,
    )
    Tini = get_input("Initial temperature (Kelvin)", 645.15, float)
    irestartfrom = (
        get_input("Iteration to restart", 2000, int)
        if inittm == Init.RESTART.value
        else 0
    )

    buffer_inlet = get_input("Inlet thermal buffer length (lx/L0)", 0.0, float)
    buffer_outlet = get_input("Outlet thermal buffer length (lx/L0)", 0.0, float)
    use_qw_ramp = get_yes_no("Enable wall heat-flux ramp? (0:No, 1:Yes)", default=0)
    if use_qw_ramp == 1:
        qw_ramp_start = get_input("Heat-flux ramp start iteration", 1, int)
        qw_ramp_end = get_input("Heat-flux ramp end iteration", 1000, int)
    else:
        qw_ramp_start = 0
        qw_ramp_end = 0

    return {
        "ithermo": bool_to_string(ithermo),
        "icht": bool_to_string(icht),
        "igravity": igravity,
        "ifluid": ifluid,
        "ref_l0": refl0,
        "ref_T0": refT0,
        "inittm": inittm,
        "irestartfrom": irestartfrom,
        "Tini": Tini,
        "inout_buffer": format_csv(buffer_inlet, buffer_outlet),
        "qw_ramp": format_csv(
            bool_to_string(use_qw_ramp), qw_ramp_start, qw_ramp_end
        ),
    }


def get_mhd_settings():
    """MHD field settings."""
    global imhd
    print(f"{MESSAGE_SEP} MHD {MESSAGE_SEP}")

    imhd = get_yes_no("Enable MHD? (0:No, 1:Yes)", default=0)
    if imhd != 1:
        return None

    ss = get_input("Stuart (1) or Hartmann (2) number based?", 2, int)
    if ss == 1:
        NS = get_input("Stuart Number", 10.0, float)
        NH = 0.0
        iStuart, iHartmn = 1, 0
    else:
        NH = get_input("Hartmann Number", 10.0, float)
        NS = 0.0
        iStuart, iHartmn = 0, 1

    b1 = get_input("Static magnetic field in X", 0.0, float)
    b2 = get_input("Static magnetic field in Y", 1.0, float)
    b3 = get_input("Static magnetic field in Z", 0.0, float)

    return {
        "imhd_xdom": bool_to_string(imhd),
        "NStuart": format_csv(bool_to_string(iStuart), NS),
        "NHartmn": format_csv(bool_to_string(iHartmn), NH),
        "B_static": format_csv(b1, b2, b3),
    }


def get_mesh_settings():
    """Mesh grid and stretching settings."""
    global icase
    print(f"{MESSAGE_SEP} MESH {MESSAGE_SEP}")

    ncx = get_input("Cell number in x", 64, int)
    ncy = get_input("Cell number in y", 64, int)
    ncz = get_input("Cell number in z", 64, int)

    # Default stretching by case
    if icase in {Case.CHANNEL.value, Case.DUCT.value, Case.ANNULAR.value}:
        istret = Stretching.SIDE2.value
    elif icase == Case.PIPE.value:
        istret = Stretching.TOP.value
    elif icase == Case.TGV3D.value:
        istret = Stretching.NONE.value
    else:
        istret = get_input(
            "Grid clustering type (0:None, 1:Centre, 2:2-sides, 3:Bottom, 4:Top)",
            0,
            int,
        )

    # Stretching parameters
    if istret != Stretching.NONE.value:
        if icase in [Case.CHANNEL.value, Case.DUCT.value]:
            rstret1, rstret2 = (
                "3fmd",
                get_input(
                    "Stretching factor (0.1-0.3, smaller=more clustered)", 0.12, float
                ),
            )
        elif icase in [Case.PIPE.value, Case.ANNULAR.value]:
            rstret1, rstret2 = (
                "tanh",
                get_input(
                    "Stretching factor (0.1-0.3, greater=more clustered)", 0.15, float
                ),
            )
        else:
            rstret1 = get_input(
                "Stretching method (uniform, 3fmd, tanh, powl)", "3fmd", str
            )
            rstret2 = get_input("Stretching factor (0.1-0.3)", 0.15, float)
    else:
        rstret1, rstret2 = 0, 0.0

    return {
        "ncx": ncx,
        "ncy": ncy,
        "ncz": ncz,
        "istret": istret,
        "rstret": format_csv(rstret1, rstret2),
        "poisson_y_method": "auto",
    }


def get_bc_settings():
    """Boundary condition settings."""
    global icase, ithermo, iinlet, has_convective_outlet
    print(f"{MESSAGE_SEP} BC {MESSAGE_SEP}")

    # Initialize all BC values
    bc_dict = {
        "ifbcy_u": new_bc_entry(),
        "ifbcx_u": new_bc_entry(),
        "ifbcz_u": new_bc_entry(),
        "ifbcy_p": new_bc_entry(),
        "ifbcx_p": new_bc_entry(),
        "ifbcz_p": new_bc_entry(),
        "ifbcy_T": new_bc_entry(),
        "ifbcx_T": new_bc_entry(),
        "ifbcz_T": new_bc_entry(),
    }

    # Y-direction BC
    if icase in WALL_BC_CASES:
        bc_dict["ifbcy_u"][:2] = [BC.DIRICHLET.value, BC.DIRICHLET.value]
        bc_dict["ifbcy_p"][:2] = [BC.NEUMANN.value, BC.NEUMANN.value]
    elif icase == Case.PIPE.value:
        bc_dict["ifbcy_u"][:2] = [BC.INTERIOR.value, BC.DIRICHLET.value]
        bc_dict["ifbcy_p"][:2] = [BC.INTERIOR.value, BC.NEUMANN.value]
        bc_dict["ifbcy_T"][:2] = [BC.INTERIOR.value, BC.PERIODIC.value]

    # X-direction BC (DUCT specific)
    if icase == Case.DUCT.value:
        bc_dict["ifbcx_u"][:2] = [BC.DIRICHLET.value, BC.DIRICHLET.value]
        bc_dict["ifbcx_p"][:2] = [BC.NEUMANN.value, BC.NEUMANN.value]
        bc_dict["ifbcx_T"][:2] = [BC.NEUMANN.value, BC.NEUMANN.value]

    # Inlet/periodic BC for the streamwise direction.
    # For duct, x/y are wall-normal directions and z is streamwise.
    if icase == Case.TGV3D.value:
        iinlet = 0
    else:
        iinlet = get_yes_no("Is streamwise periodic? (1:Yes, 0:No)", default=1)
        if iinlet != 1:
            iinlet = get_input(
                "Inlet boundary condition (4:Dirichlet, 9:Profile, 10:Database)",
                10,
                int,
                valid_choices=INLET_BC_CHOICES,
            )

    streamwise_prefix = "ifbcz" if icase == Case.DUCT.value else "ifbcx"
    if iinlet != 1 and icase != Case.TGV3D.value:
        bc_dict[f"{streamwise_prefix}_u"][:2] = [iinlet, BC.CONVOL.value]
        bc_dict[f"{streamwise_prefix}_p"][:2] = [BC.NEUMANN.value, BC.NEUMANN.value]
        bc_dict[f"{streamwise_prefix}_T"][:2] = [BC.DIRICHLET.value, BC.NEUMANN.value]
        if ithermo == 1:
            bc_dict[f"{streamwise_prefix}_T"][2] = get_input(
                "Temperature (K) at thermal inlet", 645.15, float
            )

    # Thermal BC
    if ithermo == 1:
        if icase in WALL_FLOW_CASES:
            is_T = get_input(
                "Thermal BC in y (1:constant T, 2:constant flux)",
                1 if icase != Case.PIPE.value else 2,
                int,
            )

            if is_T == 1:
                if icase != Case.PIPE.value:
                    bc_dict["ifbcy_T"][0] = BC.DIRICHLET.value
                    bc_dict["ifbcy_T"][2] = get_input(
                        "Temperature (K) on BC-y bottom", 645.15, float
                    )
                bc_dict["ifbcy_T"][1] = BC.DIRICHLET.value
                bc_dict["ifbcy_T"][3] = get_input(
                    "Temperature (K) on BC-y top", 650.15, float
                )
            else:  # Constant flux
                if icase != Case.PIPE.value:
                    bc_dict["ifbcy_T"][0] = BC.NEUMANN.value
                    sign_bottom = get_sign(
                        "BC-y bottom heat flux (-1=heating, +1=cooling)", default=-1
                    )
                    flux_bottom = abs(
                        get_input("Heat flux magnitude (W/m²)", 0.0, float)
                    )
                    bc_dict["ifbcy_T"][2] = flux_bottom * sign_bottom

                bc_dict["ifbcy_T"][1] = BC.NEUMANN.value
                sign_top = get_sign(
                    "BC-y top heat flux (1=heating, -1=cooling)", default=1
                )
                flux_top = abs(get_input("Heat flux magnitude (W/m²)", 0.0, float))
                bc_dict["ifbcy_T"][3] = flux_top * sign_top

            # Duct specific thermal BC
            if icase == Case.DUCT.value:
                is_T = get_input(
                    "Thermal BC in x (1:constant T, 2:constant flux)", 2, int
                )
                if is_T == 1:
                    bc_dict["ifbcx_T"][1] = BC.DIRICHLET.value
                    bc_dict["ifbcx_T"][3] = get_input(
                        "Temperature (K) on BC-x top", 650.15, float
                    )
                else:
                    bc_dict["ifbcx_T"][1] = BC.NEUMANN.value
                    bc_dict["ifbcx_T"][3] = get_input(
                        "Heat flux (W/m²) on BC-x top", 0.0, float
                    )
    else:
        # Thermal BCs are still parsed in isothermal runs. Keep wall/periodic
        # defaults consistent with the regression inputs, but preserve a
        # Dirichlet/Neumann thermal inlet/outlet marker for open streamwise runs.
        copy_velocity_bc_to_thermal(bc_dict, ["ifbcy_T", "ifbcx_T", "ifbcz_T"])
        if iinlet != 1 and icase != Case.TGV3D.value:
            bc_dict[f"{streamwise_prefix}_T"][:2] = [
                BC.DIRICHLET.value,
                BC.NEUMANN.value,
            ]
            bc_dict[f"{streamwise_prefix}_T"][2:] = [0.0, 0.0]

    # Flow driving method
    idriven = 0
    drivenCf = 0.0
    if (
        bc_dict["ifbcx_u"][0] == BC.PERIODIC.value
        and bc_dict["ifbcz_u"][0] == BC.PERIODIC.value
    ):
        if icase != Case.TGV3D.value:
            idriven = get_input(
                "Flow driven method (0:none, 1:mass flux, 2:skin friction, 3:pressure gradient)",
                1,
                int,
            )
    elif (
        bc_dict["ifbcx_u"][0] != BC.PERIODIC.value
        and bc_dict["ifbcz_u"][0] == BC.PERIODIC.value
    ):
        if icase == Case.DUCT.value:
            idriven = get_input(
                "Flow driven method (0:none, 4:mass flux, 5:skin friction, 6:pressure gradient)",
                4,
                int,
            )

    if idriven in [
        Drvfc.XTAUW.value,
        Drvfc.XDPDX.value,
        Drvfc.ZTAUW.value,
        Drvfc.ZDPDZ.value,
    ]:
        drivenCf = get_input("Magnitude of driving force", 0.0, float)

    # Format BC output
    result = {
        "ifbcx_u": format_bc_entry(bc_dict["ifbcx_u"]),
        "ifbcx_v": format_bc_entry(bc_dict["ifbcx_u"]),
        "ifbcx_w": format_bc_entry(bc_dict["ifbcx_u"]),
        "ifbcx_p": format_bc_entry(bc_dict["ifbcx_p"]),
        "ifbcx_t": format_bc_entry(bc_dict["ifbcx_T"]),
        "ifbcy_u": format_bc_entry(bc_dict["ifbcy_u"]),
        "ifbcy_v": format_bc_entry(bc_dict["ifbcy_u"]),
        "ifbcy_w": format_bc_entry(bc_dict["ifbcy_u"]),
        "ifbcy_p": format_bc_entry(bc_dict["ifbcy_p"]),
        "ifbcy_t": format_bc_entry(bc_dict["ifbcy_T"]),
        "ifbcz_u": format_bc_entry(bc_dict["ifbcz_u"]),
        "ifbcz_v": format_bc_entry(bc_dict["ifbcz_u"]),
        "ifbcz_w": format_bc_entry(bc_dict["ifbcz_u"]),
        "ifbcz_p": format_bc_entry(bc_dict["ifbcz_p"]),
        "ifbcz_t": format_bc_entry(bc_dict["ifbcz_T"]),
        "idriven": idriven,
        "drivenfc": drivenCf,
    }

    has_convective_outlet = any(
        bc[1] == BC.CONVOL.value
        for bc in (bc_dict["ifbcx_u"], bc_dict["ifbcy_u"], bc_dict["ifbcz_u"])
    )

    return result


def get_scheme_settings():
    """Numerical scheme settings."""
    global has_convective_outlet
    print(f"{MESSAGE_SEP} SCHEME {MESSAGE_SEP}")

    dt = get_input("Time step size", 0.00001, float)
    iAccuracy = get_input(
        "Spatial accuracy", "cd2", str, valid_choices=["cd2", "cd4", "cp4", "cp6"]
    ).lower()
    if has_convective_outlet:
        sponge_length = get_input("Outlet sponge layer length", 0.0, float)
        sponge_re = get_input("Reynolds number in sponge layer", 100.0, float)
    else:
        sponge_length = 0.0
        sponge_re = get_input("Reynolds number in sponge layer", 0.0, float)

    return {
        "dt": dt,
        "itimescheme": "rk3",
        "iaccuracy": iAccuracy,
        "iviscous": "explicit",
        "out_sponge_L_Re": format_csv(sponge_length, sponge_re),
    }


def get_simcontrol_settings():
    """Simulation control settings."""
    global ithermo
    print(f"{MESSAGE_SEP} SIMULATION CONTROL {MESSAGE_SEP}")

    nIterFlowFirst = get_input("First iteration for flow field", 1, int)
    nIterFlowLast = get_input("Last iteration for flow field", 1000000, int)

    if ithermo == 1:
        nIterThermoFirst = get_input("First iteration for thermal field", 1, int)
        nIterThermoLast = get_input("Last iteration for thermal field", 1000000, int)
    else:
        nIterThermoFirst = nIterThermoLast = 0

    return {
        "niterflowfirst": nIterFlowFirst,
        "niterflowlast": nIterFlowLast,
        "niterthermofirst": nIterThermoFirst,
        "niterthermolast": nIterThermoLast,
    }


def get_io_settings():
    """Input/Output settings."""
    global iinlet
    print(f"{MESSAGE_SEP} I/O {MESSAGE_SEP}")

    cpu_nfre = get_input("CPU info print frequency", 1, int)
    ckpt_nfre = get_input("Checkpoint save frequency", 1000, int)
    visu_idim = get_input(
        "Visualization mode (0:3-D only, 1:2-D planes only, 2:both 3-D and 2-D)",
        0,
        int,
        valid_choices=[0, 1, 2],
    )
    visu_nfre = get_input("Visualization frequency", 500, int)
    stat_istart = get_input("Start statistics from iteration", 1000, int)
    stat_level = get_input(
        "Statistics level (1: mean flow, 2: +Reynolds stresses, 3: +turbulent budget dynamics)",
        3,
        int,
        valid_choices=[0, 1, 2, 3],
    )
    stat_visu_nfre = get_input("Visualized statistics frequency", visu_nfre, int)
    stat_visu_mode = get_input(
        "Visualized statistics mode (all, tsp_only)",
        "all",
        str,
        valid_choices=["all", "tsp_only"],
    )
    existing_output_policy = get_input(
        "Existing output policy (overwrite, skip, rename_existing)",
        "overwrite",
        str,
        valid_choices=OUTPUT_POLICY_CHOICES,
    )
    restart_data_layout = get_input(
        "Restart/output data layout (per_field, bundled)",
        "bundled",
        str,
        valid_choices=RESTART_LAYOUT_CHOICES,
    )
    reset_unit_massflux = get_yes_no(
        "Reset restored streamwise bulk velocity to 1.0? (0:No, 1:Yes)", default=0
    )
    visu_precision = get_input(
        "Visualisation precision bytes (4:single, 8:double)", 4, int, valid_choices=[4, 8]
    )

    is_write = get_yes_no("Write outlet plane data? (0:No, 1:Yes)", default=0)
    is_read = 1 if iinlet == BC.DATABS.value else 0

    if is_write == 0 and is_read == 0:
        wrt_read_nfre1 = wrt_read_nfre2 = wrt_read_nfre3 = 0
        ndb_file_offset = 0
    else:
        wrt_read_nfre1 = get_input("Plane data save frequency (iterations)", 1000, int)
        wrt_read_nfre2 = get_input("Start saving from iteration", 2001, int)
        wrt_read_nfre3 = get_input("Stop saving at iteration", 10000, int)
        ndb_file_offset = get_input("Plane database file iteration offset", 0, int)

        total_steps = wrt_read_nfre3 - wrt_read_nfre2 + 1
        if total_steps % wrt_read_nfre1 != 0:
            suggested = (
                math.ceil(total_steps / wrt_read_nfre1) * wrt_read_nfre1
                + wrt_read_nfre2
                - 1
            )
            print(f"⚠️  Warning: (Stop - Start + 1) not divisible by frequency.")
            print(f"   Suggested Stop iteration: {suggested}")

    return {
        "cpu_nfre": cpu_nfre,
        "ckpt_nfre": ckpt_nfre,
        "visu_idim": visu_idim,
        "visu_nfre": visu_nfre,
        "visu_nskip": DEFAULT_VISU_SKIP,
        "stat_istart": stat_istart,
        "stat_level": stat_level,
        "stat_nskip": DEFAULT_STAT_SKIP,
        "stat_visu_nfre": stat_visu_nfre,
        "stat_visu_mode": stat_visu_mode,
        "is_record_xoutlet_read_xinlet": format_csv(bool_to_string(is_write), bool_to_string(is_read)),
        "ndbfre_ndbstart_ndbend": format_csv(wrt_read_nfre1, wrt_read_nfre2, wrt_read_nfre3),
        "ndb_file_offset": ndb_file_offset,
        "existing_output_policy": existing_output_policy,
        "restart_data_layout": restart_data_layout,
        "restart_data_layout_read": restart_data_layout,
        "restart_data_layout_write": restart_data_layout,
        "reset_unit_massflux": bool_to_string(reset_unit_massflux),
        "visu_precision": visu_precision,
    }


def get_probe_settings(lxx, lzz, lyt, lyb):
    """Probe point settings."""
    print(f"{MESSAGE_SEP} PROBE {MESSAGE_SEP}")

    is_auto = get_yes_no("Auto-generate 5 probe points? (0:No, 1:Yes)", default=1)
    if is_auto == 1:
        npp = DEFAULT_PROBE_COUNT
        lxp = [lxx / 2.0] * npp
        lzp = [lzz / 2.0] * npp
        lyp = [lyb + (lyt - lyb) * (i + 1) / (npp + 1) for i in range(npp)]
    else:
        npp = get_input("Number of probe points", 3, int)
        lxp, lyp, lzp = [], [], []

        for i in range(npp):
            x = get_input(f"Point {i} coord.x", 0.5, float)
            y = get_input(f"Point {i} coord.y", 0.5, float)
            z = get_input(f"Point {i} coord.z", 0.5, float)
            lxp.append(x)
            lyp.append(y)
            lzp.append(z)

    result = {"npp": npp}
    for i in range(npp):
        result[f"pt{i + 1}"] = build_probe_point(lxp[i], lyp[i], lzp[i])

    return result


class CustomConfigParser(configparser.ConfigParser):
    """Custom ConfigParser that formats output with space after '='."""

    def __init__(self):
        super().__init__(interpolation=None)
        self.optionxform = str

    def write(self, fp):
        for section in self.sections():
            fp.write(f"[{section}]\n")
            for key, value in self.items(section):
                fp.write(f"{key}= {value}\n")
            fp.write("\n")


def generate_ini(filename=DEFAULT_FILENAME):
    """Generate CHAPSim2 input file by collecting user inputs."""
    reset_runtime_state()
    ensure_template_available()
    config = CustomConfigParser()

    # Collect all settings
    settings = [
        ("process", get_process_settings),
        ("decomposition", get_decomp_settings),
        ("domain", get_domain_settings),
        ("flow", get_flow_settings),
        ("thermo", get_thermo_settings),
        ("mhd", get_mhd_settings),
        ("les", lambda: {"LESmode": "DNS"}),
        ("mesh", get_mesh_settings),
        ("bc", get_bc_settings),
        ("scheme", get_scheme_settings),
        ("simcontrol", get_simcontrol_settings),
        ("io", get_io_settings),
    ]

    for section_name, setter_func in settings:
        result = setter_func()
        if result:
            config[section_name] = result

    # Add probe settings with domain parameters
    domain_section = config["domain"] if "domain" in config else {}
    probe_settings = get_probe_settings(
        float(domain_section.get("lxx", TWO_PI)),
        float(domain_section.get("lzz", PI)),
        float(domain_section.get("lyt", 1.0)),
        float(domain_section.get("lyb", -1.0)),
    )
    if probe_settings:
        config["probe"] = probe_settings

    # Write to file
    with open(filename, "w") as configfile:
        config.write(configfile)

    print(f"\n✓ Configuration saved to {filename}\n")


if __name__ == "__main__":
    generate_ini()
