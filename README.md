# About CHAPSim 
A **CH**annel **A**nd **P**ipe flow **sim**ulation solver, *CHAPSim*, is described here. *CHAPSim* is an incompressible Direct Numerical Simulation (DNS) code for flow and heat transfer with MPI parallelization. This repository contains the latest stable version. 

## Code Development History
The early version of CHAPSim was developed by Dr Mehdi Seddighi-Moornani in f77 platform for simulations of unsteady turbulent flows in a channel during his PhD study starting from 2007. The code has later been revamped by Dr Wei Wang for solving thermo-fluids problems in f90 platform in 2014/2015. All this early development work was carried out while Drs Seddighi-Moornani and Wang were working with Professor Shuisheng He at Universities of Aberdeen and Sheffield. In 2020, CHAPSim was selected by CCP.NTH to be developed as an open-source community code in partnership with STFC Daresbury Laboratory (Contact: Dr Wei Wang), Liverpool John Moores University (Contact: Dr Mehdi Seddighi) and University of Sheffield (Contact: Professor Shuisheng He) and released under the GNU General Public License, that is a free, copyleft license.

## License
GNU General Public License
This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License or (at your option) any later version. This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program; If not, write to the 
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. Also, add information on how to contact you by electronic and paper mail.

## Contact: Main contributors
* Wei Wang, DL-STFC-UKRI. (wei.wang@stfc.ac.uk, wei.wang@sheffield.ac.uk)
* Shuisheng He, The University of Sheffield. (s.he@sheffield.ac.uk)

# Code Building
## Source Download and Compilation
Acquire the source code by cloning the git repository:
```
git clone git@github.com:CHAPSim/CHAPSim2.git
```
Compile the codes:
```
./build_make.sh
```
Run the code:
```
mpirun -n 4 ./bin/CHAPSim2
```
## Updating an existing source tree
If you have previously downloaded *CHAPSim* using git clone, you can update the existing source tree using git pull rather than starting a new:
```
cd CHAPSim2
git pull
```

