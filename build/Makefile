#################################################################################################
# Makefile for CHAPSim2, by Wei Wang, July 2021                                                 #
# Updated: Jan 2025 - Incremental compilation enabled                                           #
# Usage:                                                                                        #
#       make all         to make all files with -O3 optimization                                #
#       make cfg=gnu     to debug for gfortran compiler (with debug_steps)                      #
#       make cfg=gnu-nodebug to debug for gfortran compiler (without debug_steps)              #
#       make cfg=intel   to debug for intel compiler                                            #
#       make cfg=cray    to make for cray                                                       #
#################################################################################################
.SUFFIXES:

# Program name and directories
PROGRAM = CHAPSim
DIR_SRC = ../src
DIR_BIN = ../bin
DIR_OBJ = ../obj
DIR_BLD = ../build # This directory seems to be for .mod files, not used in object output directly
DIR_LIB = ../lib/fishpack4.1

# Default configuration if 'cfg' is not specified
ifeq ($(cfg),)
    cfg := default
endif

# Common Fortran Compiler Flags
COMMON_FOPTS = -fallow-argument-mismatch -Wno-unused -Wno-unused-dummy-argument -Wno-maybe-uninitialized
COMMON_FFLGS = -DDOUBLE_PREC -fdefault-real-8 -fdefault-double-8
FOPTS_WARNING = -Wunused-variable -Wunused-parameter -Wunused-dummy-argument

# Compiler and flags based on configuration
ifeq ($(cfg), gnu-g)
  FC     = mpif90
  FOPTS  = -g -O0 -Wall -Wextra -fbacktrace -fbounds-check -fcheck=all -ffpe-trap=invalid,zero,overflow \
           -ffree-line-length-512 -Wuninitialized -pedantic -finit-real=snan -fstack-protector -fimplicit-none \
           -cpp -fopenmp $(COMMON_FOPTS) #$(FOPTS_WARNING)
  FFLGS  = $(COMMON_FFLGS)
  FDEBG  = # No DEBUG_STEPS for this config
else ifeq ($(cfg), gnu-debug)
  FC     = mpif90
  FOPTS  = -g -O0 -Wall -Wextra -fbacktrace -fbounds-check -fcheck=all -ffpe-trap=invalid,zero,overflow \
           -ffree-line-length-512 -Wuninitialized -pedantic -finit-real=snan -fstack-protector -fimplicit-none \
           -cpp -fopenmp $(COMMON_FOPTS)
  FFLGS  = $(COMMON_FFLGS)
  FDEBG  = -DDEBUG_STEPS
else ifeq ($(cfg), intel)
  FC     = mpiifort
  FOPTS  = -g -assume ieee_fpe_flags -check all -check bounds -check uninit -debug all \
           -fp-stack-check fpe0 -fpe3 -fpe-all=3 -ftrapuv -ftz -warn all,nounused -fpp \
           -convert big_endian
  FFLGS  = -DDOUBLE_PREC
  FDEBG  = -DDEBUG_STEPS -DDEBUG_FFT
else ifeq ($(cfg), cray)
  FC     = ftn
  FOPTS  = -cpp
  FFLGS  =
  FDEBG  = -DDEBUG_STEPS -DDEBUG_FFT
else ifeq ($(cfg), gnu-o3)
  FC     = mpif90
  FOPTS  = -O3 -Wall -fbacktrace -fbounds-check -fcheck=all -ffpe-trap=invalid,zero,overflow \
           -finit-local-zero -ffree-line-length-512 -cpp -fopenmp -fimplicit-none $(COMMON_FOPTS)
  FFLGS  = -DDOUBLE_PREC
  FDEBG  =
else
  FC     = mpif90
  FOPTS  = -O3 -Wall -fbacktrace -fbounds-check -fcheck=all -ffpe-trap=invalid,zero,overflow \
           -finit-local-zero -ffree-line-length-512 -cpp -fopenmp -fimplicit-none $(COMMON_FOPTS)
  FFLGS  = -DDOUBLE_PREC
  FDEBG  =
endif

# HDF5 paths (added)
HDF5_INC = # -I/opt/homebrew/include
HDF5_LIB = # -L/opt/homebrew/lib -lhdf5_fortran -lhdf5

# FFT library and include paths
INCLUDE = -I../lib/2decomp-fft/build/opt/include $(HDF5_INC)
# Check if the lib64 directory exists, otherwise fall back to lib
LIB_DIR = $(if $(wildcard ../lib/2decomp-fft/build/opt/lib64), ../lib/2decomp-fft/build/opt/lib64, ../lib/2decomp-fft/build/opt/lib)
# Set the LIBS variable based on the determined library directory
LIBS = -L$(LIB_DIR) -ldecomp2d $(HDF5_LIB)

# Object file lists
OBJS1 = $(addprefix $(DIR_OBJ)/, \
      modules.o \
      tools_general.o \
      input_thermo.o \
      bc_dirichlet.o \
      bc_ndomain_interior.o \
      bc_general.o \
      input_general.o \
      basics_algorithms.o \
      basics_operations2.o \
      tools_solver.o \
      para_conversion.o \
      geometry.o \
      io_tools.o \
      io_monitor.o \
      io_visulisation.o \
      post_statistics.o \
      domain_decomposition.o \
      poisson_fishpack.o \
      poisson_1stderivcomp.o \
      poisson_interface.o \
      bc_convective_outlet.o \
      eq_continuity.o \
      eq_energy.o \
      eq_mhd.o \
      io_restart.o \
      eq_momentum2.o \
      initialisation.o \
      test_algrithms.o \
      chapsim.o)

OBJS2 = $(addprefix $(DIR_OBJ)/, fftpack.o)

# Compilation flags for FFT library (Fortran 77 files)
F77_OPTS = -O2 -w -fdefault-real-8 -fdefault-double-8 -fallow-argument-mismatch

# Total number of object files for progress display
TOTAL_OBJS := $(words $(OBJS1) $(OBJS2))
COUNTER = 0

# Function to display compilation progress
define progress
    $(eval COUNTER := $(shell expr $(COUNTER) + 1))
    $(eval PERCENT := $(shell expr $(COUNTER) \* 100 / $(TOTAL_OBJS)))
    @printf "(%2d/%2d) [%3d%%] Compiling %s\n" $(COUNTER) $(TOTAL_OBJS) $(PERCENT) $(notdir $<)
endef

# Default rule
default: all

# Main target for building the program
$(DIR_BIN)/$(PROGRAM): $(OBJS1) $(OBJS2)
	@echo -e "\nLinking $(PROGRAM)..."
	@mkdir -p $(@D) # Ensure the binary directory exists
	@$(FC) $(FOPTS) $(FFLGS) $(FDEBG) -o $@ $^ $(LIBS)
	@echo -e "====== Successfully compiled $(PROGRAM) ====== \n"

# Rule to compile Fortran 90 files
$(DIR_OBJ)/%.o: $(DIR_SRC)/%.f90
	@mkdir -p $(@D) # Ensure the object directory exists
	$(call progress)
	@$(FC) $(INCLUDE) $(FOPTS) $(FFLGS) $(FDEBG) -c -o $@ $<

# Rule to compile Fortran 77 files
$(DIR_OBJ)/%.o: $(DIR_LIB)/%.f
	@mkdir -p $(@D) # Ensure the object directory exists
	$(call progress)
	@$(FC) $(F77_OPTS) -c -o $@ $<

# All target (sets cfg to default optimization if not specified)
all:
	$(MAKE) $(DIR_BIN)/$(PROGRAM) cfg=$(cfg)

# Clean target
clean:
	@echo "Cleaning built files and directories... \n"
	@rm -rf $(DIR_OBJ) $(DIR_BIN)/$(PROGRAM)
	@find $(DIR_BLD) -name "*.mod" -delete -o -name "*.smod" -delete # More robust clean for module files

# Phony targets
.PHONY: all clean default
