# CHAPSim2 Input File Generator

[![Python Version](https://img.shields.io/badge/Python-%3E=3.6-blue.svg)](https://www.python.org/)
[![License](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Project Repository](https://img.shields.io/badge/Repository-GitHub-lightgrey?logo=github)](https://github.com/CHAPSim/CHAPSim2)

## 📄 Overview

The `autoinput.py` script streamlines the creation of structured input files required by the CFD solver [CHAPSim2](https://github.com/weiwangstfc/CHAPSim2). Generating these configuration files manually can be error-prone and time-consuming. This utility provides an efficient and user-friendly way to define all necessary simulation parameters, ensuring accuracy and enhancing the reproducibility of your simulations.

## ✨ Key Features

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

## 🛠️ System Requirements

* **Python:** Version 3.6 or higher is required to run the script.
* **Dependencies:** This script relies solely on standard Python libraries, eliminating the need for external package installations.

## ⚙️ Installation (No installation required)

This script is self-contained. Simply download or clone the repository containing `autoinput.py`.

## 🚀 Usage

### Interactive Input File Generation

1.  Open your terminal or command prompt.
2.  Navigate to the directory where `autoinput.py` is located.
3.  Execute the script using the Python interpreter:

    ```bash
    python autoinput.py
    ```

4.  The script will then guide you through a series of questions to configure your CHAPSim2 input file. Provide the requested parameters as prompted.

### Output File

Upon completion, the script will generate a configuration file named `input_chapsim_auto.ini` in the same directory where you ran the script.

### Integrating with CHAPSim2

1.  Locate the generated `input_chapsim_auto.ini` file.
2.  Move this file to the directory where you intend to run your CHAPSim2 simulation case.
3.  Rename the file to `input_chapsim.ini`.
4.  CHAPSim2 will now read the simulation parameters from this file when you execute your case.

## 📜 License

This script is released under the **BSD 3-Clause License**. For the full license text, please refer to the [LICENSE](link_to_license_file_if_available) file in the repository or visit [https://opensource.org/licenses/BSD-3-Clause](https://opensource.org/licenses/BSD-3-Clause).

## ✍️ Author Information

**Wei Wang**
Senior Computational Scientist
Scientific Computing Department
UKRI-STFC
[![Email](https://img.shields.io/badge/Email-wei.wang%40stfc.ac.uk-lightgrey?logo=mail)](mailto:wei.wang@stfc.ac.uk)
[![GitHub](https://img.shields.io/badge/GitHub-weiwangstfc-lightgrey?logo=github)](https://github.com/weiwangstfc)
[![Website](https://img.shields.io/badge/Website-STFC-lightgrey?logo=internet-explorer)](your_stfc_website_link_if_available)