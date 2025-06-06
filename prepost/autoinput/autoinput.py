#!/usr/bin/env python3
"""
[![Python Version](https://img.shields.io/badge/Python-%3E=3.6-blue.svg)](https://www.python.org/)
[![License](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Project Repository](https://img.shields.io/badge/Repository-GitHub-lightgrey?logo=github)](https://github.com/CHAPSim/CHAPSim2)

## Overview

The `autoinput.py` script streamlines the creation of structured input files required by the CFD solver [CHAPSim2](https://github.com/weiwangstfc/CHAPSim2). Generating these configuration files manually can be error-prone and time-consuming. This utility provides an efficient and user-friendly way to define all necessary simulation parameters, ensuring accuracy and enhancing the reproducibility of your simulations.

## Key Features

* **Interactive Input:** Guides users through a series of prompts to define simulation parameters step-by-step.
* **Comprehensive DNS Configuration:** Supports a wide range of cases' settings, including:
    * Grid resolution and stretching
    * Physical properties of the fluid
    * Time-stepping schemes and parameters
    * Boundary conditions for various fields (velocity, pressure, temperature, MHD ...)
    * Solver options and numerical schemes
    * Input/Output and probe configurations
    * ...
* **Extensibility:** The modular design allows for easy expansion to accommodate new features and functionalities within CHAPSim2.
* **Standard Output:** Generates input files in a format directly compatible with CHAPSim2.

## System Requirements

* **Python:** Version 3.6 or higher is required to run the script.
* **Dependencies:** This script relies solely on standard Python libraries, eliminating the need for external package installations.

## Installation (No installation required)

This script is self-contained. Simply download or clone the repository containing `autoinput.py`.

## Usage

### Interactive Input File Generation

1.  Open your terminal or command prompt.
2.  Navigate to the directory where `autoinput.py` is located.
3.  Execute the script using the Python interpreter:

    ```bash
    python autoinput.py
    ```

4.  The script will then guide you through a series of questions to configure your CHAPSim2 input file. Provide the requested parameters as prompted.

### Output File

Upon completion, the script will generate a configuration file named `input_chamsim_auto.ini` in the same directory where you ran the script.

### Integrating with CHAPSim2

1.  Locate the generated `input_chamsim_auto.ini` file.
2.  Move this file to the directory where you intend to run your CHAPSim2 simulation case.
3.  Rename the file to `input_chamsim.ini`.
4.  CHAPSim2 will now read the simulation parameters from this file when you execute your case.

## License

This script is released under the **BSD 3-Clause License**. For the full license text, please refer to the [LICENSE](link_to_license_file_if_available) file in the repository or visit [https://opensource.org/licenses/BSD-3-Clause](https://opensource.org/licenses/BSD-3-Clause).

## Author Information

**Wei Wang**
Senior Computational Scientist
Scientific Computing Department
UKRI-STFC
[![Email](https://img.shields.io/badge/Email-wei.wang%40stfc.ac.uk-lightgrey?logo=mail)](mailto:wei.wang@stfc.ac.uk)
[![GitHub](https://img.shields.io/badge/GitHub-weiwangstfc-lightgrey?logo=github)](https://github.com/weiwangstfc)
[![Website](https://img.shields.io/badge/Website-STFC-lightgrey?logo=internet-explorer)](your_stfc_website_link_if_available)
"""

#
import configparser
from enum import Enum
import math

# Constants
message = "======================"
DEFAULT_FILENAME = "input_chapsim_auto.ini"
onepi = round(math.pi, 6)
twopi = 2.0 * onepi

# Simulation state flags
icase = ithermo = iinlet = imhd = 0

# Utility Functions
class Case(Enum):
    CHANNEL = 1
    PIPE = 2
    ANNULAR = 3
    TGV3D = 4

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
    INTRPL = 1
    RANDOM = 2
    INLET = 3
    GIVEN = 4
    POISEUILLE = 5
    FUNCTION = 6

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
    OTHERS = 11


# a general input 
def get_input(prompt, default=None, dtype=str):
    """
    Prompts the user for input with a default value and converts it to the specified type.
    
    Args:
        prompt (str): The question to ask the user.
        default (any): The default value if the user provides no input.
        dtype (type): The type to which the input should be converted (e.g., str, int, float).
    
    Returns:
        any: The user input converted to the specified type, or the default if no input is given.
    """
    user_input = input(f"{prompt} [{default}]: ").strip()
    
    if not user_input:  # If user does not enter anything, return the default
        return default
    try:
        return dtype(user_input)  # Convert the user input to the specified type
    except ValueError:
        print(f"Invalid input. Please enter a valid {dtype.__name__}. Using default value: {default}")
        return default


def bool_to_string(value):
    if value == 0:
        return ".false."
    elif value != 0:
        return ".true."
    else:
        raise ValueError("Input must be 0 or 1")



# Process Settings
def get_process_settings():
    
    is_prerun = get_input("Enable prerun only? (0:No, 1:Yes)", 0, int)
    is_postprocess = get_input("Enable postprocess? (0:No, 1:Yes)", 0, int)
    
    return {
        "is_prerun":  bool_to_string(is_prerun),
        "is_postprocess": bool_to_string(is_postprocess)
    }

# Decomposition Settings
def get_decomp_settings():
    
    nxdomain = 1
    is_decomp = get_input("Using automatic domain decomposition? (0:No, 1:Yes)", 1, int)
    if is_decomp == 0:
        p_row = get_input("Subdomain division along Y direction", 0, int)
        p_col = get_input("Subdomain division along Z direction", 0, int)
    else:
        p_row = 0
        p_col = 0
    return {
        "nxdomain": nxdomain,
        "p_row": p_row,
        "p_col": p_col
    }

# Domain Settings
def get_domain_settings():
    
    global icase
    icase = get_input("Simulation case (1:Channel, 2:Pipe, 3:Annular, 4:TGV3D, etc.)", 1, int)
    if icase == Case.TGV3D.value:
       lxx = twopi
       lzz = twopi
    else:
     lxx = get_input("Streamwise length (Lx/h)", twopi, float)
     lzz = get_input("Spanwise length (Lz/h)", onepi, float) if icase not in [Case.PIPE.value, Case.ANNULAR.value] else twopi

    if icase == Case.CHANNEL.value:
        lyt, lyb = 1.0, -1.0
    elif icase == Case.PIPE.value:
        lyt, lyb = 1.0, 0.0
    elif icase == Case.TGV3D.value:
        lyt, lyb = onepi, -onepi
    else:
        lyb = get_input("Vertical/radial bottom boundary", -1.0, float)
        if icase == Case.ANNULAR.value:
            lyt = 1.0
        else:
            lyt = get_input("Vertical/radial top boundary", 1.0, float)

    return {
        "icase": icase,
        "lxx": lxx,
        "lyt": lyt,
        "lyb": lyb,
        "lzz": lzz
    }
        
# Flow Initialization
def get_flow_settings():
    
    is_restart = get_input("Flow restart?(0:No, 1:Yes)", 0, int)
    is_extract = get_input("Flow field extracted from another field? (0:No, 1:Yes)", 0, int)
    
    initfl = 0
    irestartfrom = 0
    noiselevel = 0.0
    velo1, velo2, velo3 = 0.0, 0.0, 0.0
    if is_restart == 1:
        initfl = Init.RESTART.value
        irestartfrom = get_input("From which iteration to restart", 2000, int)

    if is_extract == 1:
        initfl = Init.INTRPL.value
    
    if is_restart != 1 and is_extract != 1:
      if icase in [Case.CHANNEL.value, Case.PIPE.value, Case.ANNULAR.value]:
        initfl = Init.POISEUILLE.value
      elif icase == Case.TGV3D.value:
        initfl = Init.FUNCTION.value
      else:
        initfl = get_input("Flow initialization (0:Restart, 1:Interpolation, 2:Random, 3:Inlet. 4:Given, 5: Poiseuille, 6: function)", 5, int)
        if initfl == Init.GIVEN.value:
            velo1 = get_input("Initial velocity in x", 1.0, float)
            velo2 = get_input("Initial velocity in y", 0.0, float)
            velo3 = get_input("Initial velocity in z", 0.0, float)
      
      if icase == Case.TGV3D.value:
        noiselevel = 0.0
      else:
       noiselevel = get_input("Random fluctuation intensity (0.0-1.0)", 0.25, float)

    ren = get_input("Reynolds number (bulk, half channel height/radius based)", 2800, int)

    if icase == Case.TGV3D.value:
      reni = ren
      nreni = 0
    else:
      reni = get_input("Initial Reynolds number", 20000, int)
      nreni = get_input("Iterations for the initial Re.", 10000, int)
    
    
    return {
        "initfl": initfl,
        "irestartfrom": irestartfrom,
        "veloinit": f"{velo1},{velo2},{velo3}",
        "noiselevel": noiselevel,
        "reni": reni,
        "nreni": nreni,
        "ren": ren,
    }

# thermal conditions
def get_thermo_settings():
    
    global ithermo
    ithermo = get_input("Enable thermal field? (0:No, 1:Yes)", 0, int)
    if ithermo != 1:
        return None

    icht = get_input("Enable conjugate heat transfer? (0:No, 1:Yes)", 0, int)
    igravity = get_input("Direction of gravity (0: None, 1:+X, -1:-X, 2:+Y, -2:-Y, 3:+Z, -3:-Z)", "0", int)
    ifluid = get_input("Which fluid flow (1: scp-H20, 2:scp-CO2, 3:sodium, 4:lead, 5:bismuth, 6:LBE)", "1", int)
    refl0 = get_input("Reference length (meter)", 0.001, float)
    refT0 = get_input("Reference Temperature (Kelvin)", 645.15, float)
    inittm = get_input("Thermal field initialization (0:Restart, 1:Interpolation, 2:Random, 3:Inlet. 4:Given, 5:Poiseuille, 6:function)", 4, int)
    Tini = get_input("Initial temperature (Kelvin)", 645.15, float)
    if inittm == Init.RESTART.value:
      irestartfrom = get_input("Iteration to restart", 2000, int)
    else:
      irestartfrom = 0

    return{
        "ithermo":  bool_to_string(ithermo),
        "icht":  bool_to_string(icht),
        "igravity": igravity,
        "ifluid": ifluid,
        "ref_l0": refl0,
        "ref_T0": refT0,
        "inittm": inittm,
        "irestartfrom": irestartfrom,
        "Tini": Tini
    }


# mhd conditions
def get_mhd_settings():
    
    global imhd
    imhd = get_input("Enable MHD? (0:No, 1:Yes)", 0, int)
    if imhd != 1:
        return None

    ss = get_input("Stuart (1) or Hartmann (2) number based?", 2, int)
    if ss == 1:
        iStuart, iHartmn = 1, 0
        NS = get_input("Stuart Number", 10.0, float)
        NH = 0.0
    else:
        iStuart, iHartmn = 0, 1
        NH = get_input("Hartmann Number", 10.0, float)
        NS = 0.0

    b1 = get_input("Static magnetic field in X", 0.0, float)
    b2 = get_input("Static magnetic field in Y", 1.0, float)
    b3 = get_input("Static magnetic field in Z", 0.0, float)

    return {
        "imhd": bool_to_string(imhd),
        "NStuart": f"{bool_to_string(iStuart)},{NS}",
        "NHartmn": f"{bool_to_string(iHartmn)},{NH}",
        "B_static": f"{b1},{b2},{b3}"
    }

# Mesh Settings
def get_mesh_settings():
    
    ncx = get_input("Cell number in x", 64, int)
    ncy = get_input("Cell number in y", 64, int)
    ncz = get_input("Cell number in z", 64, int)

    if icase == Case.CHANNEL.value:
        istret = Stretching.SIDE2.value
    elif icase == Case.PIPE.value:
        istret = Stretching.TOP.value
    elif icase == Case.ANNULAR.value:
        istret = Stretching.SIDE2.value
    elif icase == Case.TGV3D.value:
        istret = Stretching.NONE.value
    else:
        istret = get(icase, get_input("Grid clustering type (0:None, 1:Centre, 2:2-sides, 3:Bottom, 4:Top)", 0, int))

    if istret != Stretching.NONE.value:
        if icase == Case.CHANNEL.value:
            rstret1, rstret2, ifftlib = 1, get_input("Stretching factor (recommended 0.1-0.3, smaller means more clustered)", 0.12, float), 3
        elif icase in [Case.PIPE.value, Case.ANNULAR.value]:
            rstret1, rstret2, ifftlib = 2, get_input("Stretching factor (recommended 0.1-0.3, greater means more clustered)", 0.15, float), 2
        else:
            rstret1 = get_input("Stretching method (1:Laizet2009, 2:tanh function)", 1, int)
            rstret2 = get_input("Stretching factor (recommended 0.1-0.3)", 0.15, float)
            ifftlib = get_input("FFT solver (2:2D-FFT, 3:3D-FFT)", 3, int)
    else:
        rstret1, rstret2, ifftlib = 0, 0.0, 3

    

    return {
        "ncx": ncx,
        "ncy": ncy,
        "ncz": ncz,
        "istret": istret,
        "rstret": f"{rstret1},{rstret2}",
        "ifftlib": ifftlib,
    }


# Boundary Conditions
def get_bc_settings():
    
    global iinlet

    ifbcy_u1 = BC.PERIODIC.value
    ifbcy_p1 = BC.PERIODIC.value
    ifbcy_T1 = BC.PERIODIC.value
    ifbcy_u2 = BC.PERIODIC.value
    ifbcy_p2 = BC.PERIODIC.value
    ifbcy_T2 = BC.PERIODIC.value
    ffbcy_u1 = 0.0
    ffbcy_p1 = 0.0
    ffbcy_T1 = 0.0
    ffbcy_u2 = 0.0
    ffbcy_p2 = 0.0
    ffbcy_T2 = 0.0

    ifbcx_u1 = BC.PERIODIC.value
    ifbcx_p1 = BC.PERIODIC.value
    ifbcx_T1 = BC.PERIODIC.value
    ifbcx_u2 = BC.PERIODIC.value
    ifbcx_p2 = BC.PERIODIC.value
    ifbcx_T2 = BC.PERIODIC.value
    ffbcx_u1 = 0.0
    ffbcx_p1 = 0.0
    ffbcx_T1 = 0.0
    ffbcx_u2 = 0.0
    ffbcx_p2 = 0.0
    ffbcx_T2 = 0.0

    ifbcz_u1 = BC.PERIODIC.value
    ifbcz_p1 = BC.PERIODIC.value
    ifbcz_T1 = BC.PERIODIC.value
    ifbcz_u2 = BC.PERIODIC.value
    ifbcz_p2 = BC.PERIODIC.value
    ifbcz_T2 = BC.PERIODIC.value
    ffbcz_u1 = 0.0
    ffbcz_p1 = 0.0
    ffbcz_T1 = 0.0
    ffbcz_u2 = 0.0
    ffbcz_p2 = 0.0
    ffbcz_T2 = 0.0


    if icase in [Case.CHANNEL.value, Case.ANNULAR.value]:
        ifbcy_u1 = BC.DIRICHLET.value
        ifbcy_u2 = BC.DIRICHLET.value
        ifbcy_p1 = BC.NEUMANN.value
        ifbcy_p2 = BC.NEUMANN.value
    elif icase == Case.PIPE.value:
        ifbcy_u1 = BC.INTERIOR.value
        ifbcy_p1 = BC.INTERIOR.value
        ifbcy_T1 = BC.INTERIOR.value
        ifbcy_u2 = BC.DIRICHLET.value
        ifbcy_p2 = BC.NEUMANN.value

    if icase == Case.TGV3D.value:
      iinlet = 0
    else:
      iinlet = get_input("Use database for the streamwise inlet? (1:Yes, 0:No) ", 0, int)


    if(iinlet == 1):
        ifbcx_u1 = BC.DATABS.value
        ifbcx_p1 = BC.NEUMANN.value
        ifbcx_T1 = BC.DIRICHLET.value
        ifbcx_u2 = BC.CONVOL.value
        ifbcx_p2 = BC.NEUMANN.value
        ifbcx_T2 = BC.NEUMANN.value

    if ithermo == 1:
      if icase in [Case.CHANNEL.value, Case.PIPE.value, Case.ANNULAR.value]:
          if icase == Case.PIPE.value:
            is_T = get_input("Thermal boundary in y (1:constant temperature, 2:constant heat flux)", 2, int)
          else:
            is_T = get_input("Thermal boundary in y (1:constant temperature, 2:constant heat flux)", 1, int)
            
          if is_T == 1:
            if icase != Case.PIPE.value:
              ifbcy_T1 = BC.DIRICHLET.value
              ffbcy_T1 = get_input("Temperature (Kelvin) on BC-y bottom", 645.15, float)
            ifbcy_T2 = BC.DIRICHLET.value
            ffbcy_T2 = get_input("Temperature (Kelvin) on BC-y top", 650.15, float)
          elif is_T == 2:
            if icase != Case.PIPE.value:
              ifbcy_T1 = BC.NEUMANN.value
              ffbcy_T1 = get_input("Heat flux (W/m2) on BC-y bottom", 0.0, float)
            ifbcy_T2 = BC.NEUMANN.value
            ffbcy_T2 = get_input("Heat flux (W/m2) on BC-y top",    0.0, float)
    else:
      ifbcx_T1 = ifbcx_u1
      ifbcy_T1 = ifbcy_u1
      ifbcz_T1 = ifbcz_u1
      ifbcx_T2 = ifbcx_u2
      ifbcy_T2 = ifbcy_u2
      ifbcz_T2 = ifbcz_u2

    idriven = 0
    drivenCf = 0.0
    if ifbcx_u1 == BC.PERIODIC.value:
      if icase != Case.TGV3D.value:
        idriven = get_input("Flow driven method (periodic flow only, none (0), constant mass flux (1), skin friction (2 for x, 5 for z), 3:pressure gradient (3 for x, 6 for z)", 1, int)
        if idriven in [Drvfc.XTAUW.value, Drvfc.XDPDX.value, Drvfc.ZTAUW.value, Drvfc.ZDPDZ.value]:
          drivenCf = get_input("Magnitude of driven force", 0.0, float)

    return{
        "ifbcx_u": f"{ifbcx_u1},{ifbcx_u2},{ffbcx_u1},{ffbcx_u2}",
        "ifbcx_v": f"{ifbcx_u1},{ifbcx_u2},{ffbcx_u1},{ffbcx_u2}",
        "ifbcx_w": f"{ifbcx_u1},{ifbcx_u2},{ffbcx_u1},{ffbcx_u2}",
        "ifbcx_p": f"{ifbcx_p1},{ifbcx_p2},{ffbcx_p1},{ffbcx_p2}",
        "ifbcx_T": f"{ifbcx_T1},{ifbcx_T2},{ffbcx_T1},{ffbcx_T2}",
        "ifbcy_u": f"{ifbcy_u1},{ifbcy_u2},{ffbcy_u1},{ffbcy_u2}",
        "ifbcy_v": f"{ifbcy_u1},{ifbcy_u2},{ffbcy_u1},{ffbcy_u2}",
        "ifbcy_w": f"{ifbcy_u1},{ifbcy_u2},{ffbcy_u1},{ffbcy_u2}",
        "ifbcy_p": f"{ifbcy_p1},{ifbcy_p2},{ffbcy_p1},{ffbcy_p2}",
        "ifbcy_T": f"{ifbcy_T1},{ifbcy_T2},{ffbcy_T1},{ffbcy_T2}",
        "ifbcz_u": f"{ifbcz_u1},{ifbcz_u2},{ffbcz_u1},{ffbcz_u2}",
        "ifbcz_v": f"{ifbcz_u1},{ifbcz_u2},{ffbcz_u1},{ffbcz_u2}",
        "ifbcz_w": f"{ifbcz_u1},{ifbcz_u2},{ffbcz_u1},{ffbcz_u2}",
        "ifbcz_p": f"{ifbcz_p1},{ifbcz_p2},{ffbcz_p1},{ffbcz_p2}",
        "ifbcz_T": f"{ifbcz_T1},{ifbcz_T2},{ffbcz_T1},{ffbcz_T2}",
        "idriven": idriven,
        "drivenfc": drivenCf
    }

# Schemes 
def get_scheme_settings():
    
    dt = get_input("Time step size", 0.00001, float)
    iTimeScheme = 3
    iAccuracy = get_input("Spacial accuracy (1:2nd CD, 2:4th CD, 3:4th CP, 4:6th CP)", 1, int)
    iviscous = 1 
    
    return {
        "dt": dt,
        "iTimeScheme": iTimeScheme,
        "iAccuracy": iAccuracy,
        "iviscous": iviscous
    }

# simcontrol
def get_simcontrol_settings():
    
    nIterFlowFirst   = get_input("The first iteration for flow field", 1, int)
    nIterFlowLast    = get_input("The last  iteration for flow field", 1000000, int)
    if ithermo == 1:
      nIterThermoFirst = get_input("The first iteration for thermal field", 1, int)
      nIterThermoLast  = get_input("The last  iteration for thermal field", 1000000, int)
    else:
      nIterThermoFirst = 0
      nIterThermoLast = 0

    return {
        "nIterFlowFirst": nIterFlowFirst,
        "nIterFlowLast": nIterFlowLast,
        "nIterThermoFirst": nIterThermoFirst,
        "nIterThermoLast": nIterThermoLast
    }

# Output Settings
def get_io_settings():
    
    iskip1 = 1
    iskip2 = 1
    iskip3 = 1
    visu_idim = 0
    cpu_nfre = get_input("Frequency to print out CPU info", 1, int)
    ckpt_nfre = get_input("Frequency to save Checkpoint data", 1000, int)
    visu_nfre = get_input("Frequency for data visualization", 500, int)
    stat_istart =  get_input("From which iteration to start statistics", 1000, int)
    is_write = get_input("Writing out outlet plane data? (0:No, 1:Yes)", 0, int)
    if iinlet == 1:
      is_read = 1
    else:
      is_read = 0

    if is_write ==0 and is_read == 0:
      wrt_read_nfre1 = 0
      wrt_read_nfre2 = 0
      wrt_read_nfre3 = 0
    else:
      wrt_read_nfre1 = get_input("Plane data saving frequency (iterations)", 1000, int)
      wrt_read_nfre2 = get_input("Start saving data from iteration", 2001, int)
      wrt_read_nfre3 = get_input("Stop saving data at iteration", 10000, int)
      
      total_steps = wrt_read_nfre3 - wrt_read_nfre2 + 1
      if total_steps % wrt_read_nfre1 != 0:
        suggested_nfre3 = math.ceil(total_steps / wrt_read_nfre1) * wrt_read_nfre1 + wrt_read_nfre2 - 1
        print(f"Warning: (Stop - Start + 1) is not divisible by the frequency.")
        print(f"Suggested Stop iteration: {suggested_nfre3} to align with frequency (check: Stop < nIterFlowLast).")

    return {
        "cpu_nfre": cpu_nfre,
        "ckpt_nfre": ckpt_nfre,
        "visu_idim": visu_idim,
        "visu_nfre": visu_nfre,
        "visu_nskip": f"{iskip1},{iskip2},{iskip3}",
        "stat_istart": stat_istart,
        "stat_nskip": f"{iskip1},{iskip2},{iskip3}",
        "is_wrt_read_bc": f"{bool_to_string(is_write)},{bool_to_string(is_read)}",
        "wrt_read_nfre": f"{wrt_read_nfre1},{wrt_read_nfre2},{wrt_read_nfre3}"
    }

# probe Settings
def get_probe_settings(lxx, lzz, lyt, lyb):
    is_auto = get_input("Automatic generated 5 probe points? (0:No, 1:Yes)", 1, int)
    if is_auto == 1:
        npp = 5
        lxp = [lxx / 2.0] * npp  # Make sure it's a list
        lzp = [lzz / 2.0] * npp  # Make sure it's a list
        lyp = [(lyb + (lyt - lyb) * (i+1) / (npp + 1)) for i in range(npp)] 
    else:
        npp = get_input("Number of probe points", 3, int)
        lxp, lyp, lzp = [], [], []  # Initialize empty lists
        
        for i in range(npp):
            x = get_input(f"Point {i} coord.x", 0.5, float)
            y = get_input(f"Point {i} coord.y", 0.5, float)
            z = get_input(f"Point {i} coord.z", 0.5, float)
            lxp.append(x)
            lyp.append(y)
            lzp.append(z)

    result = {"npp": npp}
    for i in range(npp):
        result[f"pt{i+1}"] = f"{lxp[i]},{lyp[i]},{lzp[i]}"  # Return as a space-separated string
    return result


import configparser
from enum import Enum, auto
import math


# A general input function to handle user input
def get_input(prompt, default=None, dtype=str):
    user_input = input(f"{prompt} [{default}]: ").strip()
    
    if not user_input:  # If the user does not enter anything, return the default
        return default
    
    try:
        return dtype(user_input)  # Convert the user input to the specified type
    except ValueError:
        print(f"Invalid input. Please enter a valid {dtype.__name__}. Using default value: {default}")
        return default

# Customizing the ConfigParser to add a space after '=' in the output file
class CustomConfigParser(configparser.ConfigParser):
    def write(self, fp):
        # Override the default writing method to ensure no space before '=' and a space after '='
        for section in self.sections():
            fp.write(f'[{section}]\n')
            for key, value in self.items(section):
                fp.write(f'{key}= {value}\n')  # Space after '='
            fp.write('\n')

## Writing out data
def generate_ini(filename=DEFAULT_FILENAME):
    config = CustomConfigParser()
    """
    This function generates a configuration INI file by asking the user for inputs for various parameters.
    The parameters cover settings for the simulation, including process, decomposition, domain, etc.
    
    Args:
    filename (str): The name of the INI file to save the configuration to.
    """
    #config = configparser.ConfigParser()
        
    print(message + "process" + message)  
    process_settings = get_process_settings()
    if process_settings:
      config["process"] = process_settings

    print(message + "decomposition" + message)  
    decomp_settings = get_decomp_settings()
    if decomp_settings:
      config["decomposition"] = decomp_settings

    print(message + "domain" + message)  
    domain_settings = get_domain_settings()
    if domain_settings:
      config["domain"] = domain_settings

    print(message + "flow" + message)  
    flow_settings = get_flow_settings()
    if flow_settings:
      config["flow"] = flow_settings
    
    print(message + "thermo" + message)  
    thermo_settings = get_thermo_settings()
    if thermo_settings:
      config["thermo"] = thermo_settings

    print(message + "mhd" + message)  
    mhd_settings = get_mhd_settings()
    if mhd_settings:
      config["mhd"] = mhd_settings

    print(message + "mesh" + message)  
    mesh_settings = get_mesh_settings()
    if mesh_settings:
      config["mesh"] = mesh_settings

    print(message + "bc" + message)  
    bc_settings = get_bc_settings()
    if bc_settings:
      config["bc"] = bc_settings

    print(message + "scheme" + message)  
    scheme_settings = get_scheme_settings()
    if scheme_settings:
      config["scheme"] = scheme_settings

    print(message + "simcontrol" + message)  
    simcontrol_settings = get_simcontrol_settings()
    if simcontrol_settings:
      config["simcontrol"] = simcontrol_settings

    print(message + "io" + message) 
    io_settings = get_io_settings()
    if io_settings:
      config["io"] = io_settings

    print(message + "probe" + message)  
    probe_settings = get_probe_settings(
        domain_settings["lxx"],
        domain_settings["lzz"],
        domain_settings["lyt"],
        domain_settings["lyb"]
    )
    if probe_settings:  # Check if thermal_settings is not None
        config["probe"] = probe_settings


 # Write to file
    with open(filename, "w") as configfile:
        config.write(configfile)

    print(f"Configuration saved to {filename}")

if __name__ == "__main__":
    generate_ini()
