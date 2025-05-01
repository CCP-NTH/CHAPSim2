============
Installation
============
The latest version of `ChapSim` supports `makefile` to compile the code. The only requirements is a 
Fortran 90-compatible compiler and a working MPI library.


`Download`
==========
Source code and examples of `ChapSim` can be acquired by cloning the git repository:

.. code-block:: bash

   $ git clone git@github.com:weiwangstfc/CHAPSim2.git

`Compilation`
=============
The build system for `ChapSim` is based on `makefile`. You could use your own Fortran compiler and 
MPI via a MPI Fortran wrapper. For example, you could use a default `mpif90` or `ftn` if you use 
Archer2 default fortran compiler.

.. code-block:: bash

   $ export FC=mpif90
   $ export FC=ftn

Before compiling, below folders should be created if they do not exsit.

.. code-block:: bash

   $ mkdir obj bin

By defult, the library `2DECOMP&FFT` should be in the folder `lib`. The version `2decomp_fft_updated` 
is prefered. If this library is not compiled, please compile the library following below commands:

.. code-block:: bash

   $ cd lib/2decomp_fft_updated
   $ mkdir lib include
   $ make all

Compiling flags and options could be updated by changing `src/makefile.inc`

To compile `ChapSim2` source for production, 

.. code-block:: bash

   $ make all

To compile `ChapSim2` source for debugging, run the below command.

.. code-block:: bash

   $ make all cfg=gnu

The executable file `chapsim` is located in the `bin` directory.