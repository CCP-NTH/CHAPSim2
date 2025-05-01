# Installation

## Prerequisites

Before you can install and use the solver, you need to have the following software installed on your system:

* **Fortran Compiler:** We recommend using gfortran (version 10 or later).
    ```bash
    # Example for Debian/Ubuntu
    sudo apt-get update
    sudo apt-get install gfortran
    ```
    ```bash
    # Example for macOS (using Homebrew)
    brew install gfortran
    ```
    ```bash
    # For Windows, you can download MinGW-w64 with gfortran. Refer to [link to MinGW-w64 installation guide].
    ```

* **[Other Dependencies if any, e.g., MPI library]:** [Provide installation instructions]

## Downloading the Solver

If you are downloading the source code from our Git repository:

```bash
git clone git@github.com:weiwangstfc/CHAPSim2.git
cd CHAPSim2
```

## Build Instructions

## Environment Setup

## Verification

```bash
cd test
./run_test.sh
```