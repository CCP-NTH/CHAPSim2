#################################################################################################
# Makefile for CHAPSim2, by Wei Wang, July 2021                                                 #
# Updated: Jan 2025 - Incremental compilation enabled                                           #
# Usage:                                                                                        #
#       make all        to make all files with -O2                                              #
#       make cfg=gnu    to debug for gfortran compiler                                          #
#       make cfg=intel  to debug for intel compiler                                             #
#       make cfg=cray   to make for cray                                                        #
#################################################################################################
.SUFFIXES:

# Program name and directories
PROGRAM = CHAPSim
DIR_SRC = ../src
DIR_BIN = ../bin
DIR_OBJ = ../obj
DIR_BLD = ../build
DIR_LIB = ../lib/fishpack4.1

# Compiler and flags based on configuration
ifeq ($(cfg), gnu)
  FC     = mpif90
  FOPTS  = -g -O0 -Wall -Wextra -fbacktrace -fbounds-check -fcheck=all -ffpe-trap=invalid,zero,overflow \
           -ffree-line-length-512 -Wuninitialized -Wmaybe-uninitialized -Wno-unused -cpp \
          -pedantic -finit-real=nan -fstack-protector -finit-real=snan
  FOPTS += -fallow-argument-mismatch -fopenmp
  FFLGS  = -DDOUBLE_PREC -fdefault-real-8 -fdefault-double-8
  FDEBG  = -DDEBUG_STEPS
else ifeq ($(cfg), intel)
  FC     = mpiifort
  FOPTS  = -g -assume ieee_fpe_flags -check all -check bounds -check uninit -debug all \
           -fp-stack-check fpe0 -fpe3 -fpe-all=3 -ftrapuv -ftz -warn all,nounused -fpp
  FFLGS  = -DDOUBLE_PREC
  FDEBG  = -DDEBUG_STEPS -DDEBUG_FFT
else ifeq ($(cfg), cray)
  FC     = ftn
  FOPTS  = -cpp
  FFLGS  =
  FDEBG  = -DDEBUG_STEPS -DDEBUG_FFT
else
  FC     = mpif90
  FOPTS  = -O3 -Wall -fbacktrace -fbounds-check -fcheck=all -ffpe-trap=invalid,zero,overflow \
           -finit-local-zero -ffree-line-length-512 -Wno-unused -cpp
  FOPTS += -fallow-argument-mismatch -fopenmp
  FFLGS  = -DDOUBLE_PREC
  FDEBG  =
endif

# FFT library and include paths
INCLUDE = -I../lib/2decomp-fft/build/opt/include
LIBS    = -L../lib/2decomp-fft/build/opt/lib -ldecomp2d

# Object file lists
OBJS1 = $(addprefix $(DIR_OBJ)/, \
      modules.o \
			tools_general.o \
			input_thermo.o \
			bc_dirichlet.o \
      bc_convective_outlet.o \
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
F77_OPTS = -O2 -fdefault-real-8 -fdefault-double-8 -fallow-argument-mismatch

# Default rule
default: all

# Linking rule
$(DIR_BIN)/$(PROGRAM): $(OBJS1) $(OBJS2)
	@echo "\n ====== Linking... ====== \n"
	$(FC) $(FOPTS) $(FFLGS) $(FDEBG) -o $@ $^ $(LIBS)

# Rule to compile Fortran 90 files
$(DIR_OBJ)/%.o: $(DIR_SRC)/%.f90
	@mkdir -p $(DIR_OBJ)
	$(FC) $(INCLUDE) $(FOPTS) $(FFLGS) $(FDEBG) -c -o $@ $<

# Rule to compile Fortran 77 files
$(DIR_OBJ)/%.o: $(DIR_LIB)/%.f
	@mkdir -p $(DIR_OBJ)
	$(FC) $(F77_OPTS) -c -o $@ $<

# All target
all: $(DIR_BIN)/$(PROGRAM)
	@echo "\n ====== Successfully compiled ====== \n"

# Clean target
clean:
	@echo "\n ====== Cleaning build files... ====== \n"
	@rm -f $(DIR_OBJ)/*.o $(DIR_OBJ)/*.mod $(DIR_BLD)/*.mod $(DIR_BLD)/*.smod $(DIR_BIN)/$(PROGRAM)

# Phony targets
.PHONY: all clean default
