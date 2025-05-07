# CHAPSim2

[![Python Version](https://img.shields.io/badge/Python-%3E=3.6-blue.svg)](https://www.python.org/)
[![License](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Project Repository](https://img.shields.io/badge/Repository-GitHub-lightgrey?logo=github)](https://github.com/CHAPSim/CHAPSim2)

---

# Overview

**CHAPSim2** (**CH**annel **A**nd **P**ipe flow **Sim**ulation) is a high-fidelity Direct Numerical Simulation (DNS) solver for incompressible flow and heat transfer with full MPI parallelization. This repository contains the latest stable version of the software.

### License

This software is released under the BSD-3-Clause License. See the LICENSE file for details.

### Features

- **Incompressible Direct Numerical Simulation (DNS)** capabilities
- **Fortran** source code + **Python Interface** for input generation and visualization
- Works with both **Cartesian and cylindrical coordinates**
- **High Accuracy & Stability**: Uses a **fully staggered approach**, solving conservative variables directly (mass flux, heat flux), and other key variables for better numerical stability
- **Advanced Numerical Methods**: Offers optional **2nd to 6th-order accurate** finite difference schemes, with both implicit compact and explicit options
- **Powerful FFT Integration**: Supports **3-D FFT** and **2-D FFT** with arbitrary grid stretching for various boundary conditions **via 2decomp&FFT library**
- **Proven Scalability**:
  - 2-D pencil domain decomposition using 2decomp&FFT library
  - **Successfully tested on ARCHER2**, scaling efficiently up to 32,000+ cores
- **Handles Complex Geometries**:
  - Features **immersed boundary methods (IBM)**, allowing simulations of complex geometry
- **Heat Transfer Studies**:
  - Includes **conjugate heat transfer** capabilities for nuclear thermal hydraulics and energy applications

---

# Project Structure

```
CHAPSim2/
├── src/                  # Source code files
├── obj/                  # Object files
├── bin/                  # Compiled executables
├── build/                # Build directory for CHAPSim2
├── lib/                  # External libraries
│   ├── 2decomp-fft/      # Domain decomposition and 3D FFT library 
│   └── fishpack4.1/      # 1D/2D FFT library 
├── prepost/              # Pre/post-processing tools
│   ├── autoinput/        # Python scripts to generate input files 
│   └── useful_scripts/   # Utility scripts for local or HPC execution
├── docs/                 # Documentation
└── build_make.sh         # Build script
```

### Dependencies

- **2decomp-fft**: Parallel FFT library used by CHAPSim2
- **Make, CMake**: Build system tools
- **Fortran compiler**: gfortran or compatible Fortran 90 compiler
- **MPI library**: OpenMPI, MPICH or compatible MPI implementation


---

# Installation

### Download

```bash
git clone git@github.com:CHAPSim/CHAPSim2.git
cd CHAPSim2
```

### Build

1. Make the build script executable:

```bash
chmod +x build_make.sh
```

2. Run the build script:

```bash
./build_make.sh
```

3. When prompted, select options for:
   - Rebuilding the 2decomp library
   - Running `make clean` before compilation
   - Building in debug mode

Upon successful completion, the compiled executable `CHAPSim` will be available in the `bin/` directory.

---

# Running Simulations

### Setup

Copy the run script to your simulation case directory:

```bash
cp prepost/useful_scripts/run_local.sh /path/to/your/case/
```

### Execution

1. Run the script:

```bash
./run_local.sh
```

2. Follow the prompts to configure:
   - Number of processors (default: 1)
   - Debugging tools (Valgrind/LLDB)

3. The script will:
   - Create a timestamped directory with source files
   - Configure unique log filenames
   - Execute CHAPSim with your specified options

Simulation output will be saved to a timestamped log file (e.g., `output_chapsim2_2025-05-07_15.30.log`).


---

# Documentation

Comprehensive documentation is available in the repository:

```bash
/docs/index.html
```
---

# Support

For issues, questions, or further information, please contact the CHAPSim2 development team at UKRI-STFC:
Email: wei.wang@stfc.ac.uk

### Contributing

We welcome contributions from the computational fluid dynamics community. Please feel free to submit pull requests or report issues through our GitHub repository.

### Acknowledgments

CHAPSim2 builds upon CHAPSim under the Project of [CCP-NTH](https://ccpnth.ac.uk/). This work made use of computational support by CoSeC, the Computational Science Centre for Research Communities, through CCP-NTH.

### Current Status and Roadmap

CHAPSim2 is under active development. We are currently implementing new features and adding more canonical simulation cases to enhance the software's capabilities.

