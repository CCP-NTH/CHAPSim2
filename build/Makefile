#################################################################################################
# Makefile for CHAPSim2, by Wei Wang, July 2021                                                 #
# Updated: Mar 2026 - robust stop-on-error logging, resumable build, direct/script build support #
# Usage:                                                                                        #
#       make all                       interactive build menu                                    #
#       make cfg=gnu-g real-all         direct debug build without prompt                        #
#       make cfg=gnu-debug real-all     direct debug build with DEBUG_STEPS                      #
#       make cfg=gnu-o3 real-all        direct optimised build                                   #
#       make cfg=intel real-all         direct Intel build                                       #
#       make cfg=cray real-all          direct Cray build                                        #
#       make clean                      clean build files                                        #
#################################################################################################

.SUFFIXES:
SHELL := /bin/bash
.SHELLFLAGS := -ec
.DELETE_ON_ERROR:

PROGRAM = CHAPSim
DIR_SRC = ../src
DIR_BIN = ../bin
DIR_OBJ = ../obj
DIR_BLD = ../build
DIR_LIB = ../lib/fishpack4.1

cfg ?= default

UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ARCH_FLAGS :=
ifeq ($(UNAME_S),Darwin)
    ifeq ($(UNAME_M),arm64)
        ARCH_FLAGS := -arch arm64
        $(info Detected: Apple Silicon ($(UNAME_M)))
    else ifeq ($(UNAME_M),x86_64)
        ARCH_FLAGS := -arch x86_64
        $(info Detected: Intel Mac ($(UNAME_M)))
    endif
else ifeq ($(UNAME_S),Linux)
    $(info Detected: Linux ($(UNAME_M)))
else
    $(info Detected: $(UNAME_S) ($(UNAME_M)))
endif

COMMON_FOPTS = $(ARCH_FLAGS) -fallow-argument-mismatch -Wno-unused -Wno-unused-dummy-argument \
               -Wno-maybe-uninitialized -fdiagnostics-color=always -Wuninitialized \
               -finit-real=snan -finit-integer=-999999 -ffpe-trap=invalid,zero,overflow \
               -Wall -Wextra -fbacktrace -fbounds-check -fimplicit-none -cpp -fopenmp \
               -finit-local-zero -ffree-line-length-512 -DDOUBLE_PREC \
               -fdefault-real-8 -fdefault-double-8

FOPTS_WARNING = -Wunused-parameter -Wunused-dummy-argument

ifeq ($(cfg),gnu-g)
  FC     = mpif90
  FOPTS  = -g -O0 $(COMMON_FOPTS) $(FOPTS_WARNING)
  FDEBG  =
else ifeq ($(cfg),gnu-debug)
  FC     = mpif90
  FOPTS  = -g -O0 $(COMMON_FOPTS) $(FOPTS_WARNING)
  FDEBG  = -DDEBUG_STEPS
else ifeq ($(cfg),intel)
  FC     = mpiifort
  FOPTS  = -g -assume ieee_fpe_flags -check all -check bounds -check uninit -debug all \
           -fp-stack-check fpe0 -fpe3 -fpe-all=3 -ftrapuv -ftz -warn all,nounused -fpp \
           -convert big_endian
  FDEBG  = -DDEBUG_STEPS -DDEBUG_FFT
else ifeq ($(cfg),cray)
  FC     = ftn
  FOPTS  = -cpp
  FDEBG  = -DDEBUG_STEPS -DDEBUG_FFT
else ifeq ($(cfg),gnu-o3)
  FC     = mpif90
  FOPTS  = -O3 $(COMMON_FOPTS)
  FDEBG  =
else ifeq ($(cfg),nvhpc-cpu)
  FC     = mpif90
  FOPTS  = -O3 -Kieee -Minfo=accel -target=multicore -acc -cpp -DDOUBLE_PREC
  FDEBG  =
else
  FC     = mpif90
  FOPTS  = -O3 $(COMMON_FOPTS)
  FDEBG  =
endif

HDF5_INC =
HDF5_LIB =

INCLUDE = -I../lib/2decomp-fft/build/opt/include $(HDF5_INC)

LIB_FILE_64 := $(wildcard ../lib/2decomp-fft/build/opt/lib64/libdecomp2d.a)
LIB_FILE    := $(wildcard ../lib/2decomp-fft/build/opt/lib/libdecomp2d.a)

ifneq ($(LIB_FILE_64),)
    DECOMP_LIB = ../lib/2decomp-fft/build/opt/lib64/libdecomp2d.a
    $(info Using library: lib64/libdecomp2d.a)
else ifneq ($(LIB_FILE),)
    DECOMP_LIB = ../lib/2decomp-fft/build/opt/lib/libdecomp2d.a
    $(info Using library: lib/libdecomp2d.a)
else
    DECOMP_LIB =
endif

# FFT backend, as recorded by build_cmake_2decomp.sh when the library was built.
# Empty CHAPSIM_FFT_LDFLAGS for the default generic backend, which needs no
# external library; -L<dir> -lfftw3 when the build opted in with CHAPSIM_FFT=fftw.
# Reading it rather than re-detecting keeps the link consistent with the library:
# an FFTW-configured libdecomp2d.a that is linked without -lfftw3 fails on about
# forty undefined fftw_* references.
-include ../lib/2decomp-fft/build/opt/chapsim_fft_backend.mk

LIBS = $(DECOMP_LIB) $(HDF5_LIB) $(CHAPSIM_FFT_LDFLAGS)

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
      flow_gradient.o \
      tools_solver.o \
      para_conversion.o \
      geometry.o \
      io_tools.o \
      io_metadata.o \
      io_monitor.o \
      io_visulisation.o \
      post_statistics.o \
      domain_decomposition.o \
      poisson_fishpack.o \
      poisson_1stderivcomp_fft2d.o \
      poisson_interface.o \
      eq_continuity.o \
      bc_convective_outlet.o \
      eq_energy.o \
      eq_mhd.o \
      io_restart.o \
      eq_les.o \
      eq_momentum2.o \
      initialisation.o \
      test_algrithms.o \
      chapsim.o)

OBJS2 = $(addprefix $(DIR_OBJ)/, fftpack.o)

F77_OPTS = $(ARCH_FLAGS) -O2 -w -fdefault-real-8 -fdefault-double-8 -fallow-argument-mismatch

TOTAL_OBJS := $(words $(OBJS1) $(OBJS2))
COUNTER = 0

define progress
    $(eval COUNTER := $(shell expr $(COUNTER) + 1))
    $(eval PERCENT := $(shell expr $(COUNTER) \* 100 / $(TOTAL_OBJS)))
    @printf "(%2d/%2d) [%3d%%] Compiling %s\n" $(COUNTER) $(TOTAL_OBJS) $(PERCENT) $(notdir $<)
endef

default: all

# Interactive target only.
# For scripts, use: make cfg=gnu-g real-all
all:
	@if [ "$(origin cfg)" = "command line" ]; then \
		echo "Direct build requested: cfg=$(cfg)"; \
		"$(MAKE)" --no-print-directory real-all cfg="$(cfg)"; \
	else \
		clean_build=no; \
		if [ -t 0 ]; then \
			read -r -t 10 -p "Perform a clean build of CHAPSim? (y/n) [no] (10s timeout): " ans || true; \
			clean_build=$${ans:-no}; \
			echo ""; \
		else \
			echo "Non-interactive shell detected: using incremental build."; \
		fi; \
		echo "CHAPSim build mode: a=default, b=gnu-o3, c=gnu-g, d=gnu-debug, e=intel, f=cray, g=nvhpc-cpu"; \
		build_mode=a; \
		if [ -t 0 ]; then \
			read -r -t 10 -p "Select CHAPSim build mode (a,b,c,d,e,f,g) [a] (10s timeout): " mode || true; \
			build_mode=$${mode:-a}; \
			echo ""; \
		fi; \
		case "$${build_mode}" in \
			a|"") chosen_cfg=default ;; \
			b) chosen_cfg=gnu-o3 ;; \
			c) chosen_cfg=gnu-g ;; \
			d) chosen_cfg=gnu-debug ;; \
			e) chosen_cfg=intel ;; \
			f) chosen_cfg=cray ;; \
			g) chosen_cfg=nvhpc-cpu ;; \
			*) echo "Invalid build mode '$${build_mode}' - using default."; chosen_cfg=default ;; \
		esac; \
		case "$$(printf "%s" "$${clean_build}" | tr "[:upper:]" "[:lower:]")" in \
			y|yes) "$(MAKE)" --no-print-directory clean ;; \
		esac; \
		"$(MAKE)" --no-print-directory real-all cfg="$${chosen_cfg}"; \
	fi

real-all: check-lib prep-build-log $(DIR_BIN)/$(PROGRAM)
	@echo ""
	@echo "========================================================================="
	@echo "  CHAPSim2 Build System"
	@echo "  Configuration: $(cfg)"
	@echo "========================================================================="
	@echo ""

prep-build-log:
	@mkdir -p $(DIR_BLD)
	@if [ ! -f build.log ]; then \
		echo "Build log created: $$(date)" > build.log; \
	else \
		echo "" >> build.log; \
		echo "Build resumed: $$(date)" >> build.log; \
	fi

check-lib:
	@if [ -z "$(DECOMP_LIB)" ]; then \
		echo "❌ Error: libdecomp2d.a not found in ../lib/2decomp-fft/build/opt/lib or lib64."; \
		echo "Please build 2decomp-fft first."; \
		exit 1; \
	fi
	@ar -t "$(DECOMP_LIB)" > /dev/null 2>&1 || \
	{ echo "❌ Error: Library $(DECOMP_LIB) is corrupted or unreadable. Rebuild the library."; exit 1; }

$(DIR_BIN)/$(PROGRAM): $(OBJS1) $(OBJS2)
	@echo ""
	@echo "========================================================================="
	@echo "  Linking $(PROGRAM)..."
	@echo "  Architecture: $(UNAME_M) on $(UNAME_S)"
	@echo "  Compiler: $(FC)"
	@echo "  Library: $(DECOMP_LIB)"
	@echo "========================================================================="
	@mkdir -p $(@D)
	@$(FC) $(FOPTS) $(FDEBG) $(ARCH_FLAGS) -o $@ $^ $(LIBS) >> build.log 2>&1 || \
	  { echo "❌ Link failed. Last 40 lines of build.log:"; tail -40 build.log; exit 1; }
	@echo ""
	@echo "========================================================================="
	@echo "  ✅ Successfully compiled $(PROGRAM)"
	@echo "  Binary location: $@"
	@echo "========================================================================="
	@echo ""

$(DIR_OBJ)/%.o: $(DIR_SRC)/%.f90
	@mkdir -p $(@D)
	$(call progress)
	@$(FC) $(INCLUDE) $(FOPTS) $(FDEBG) -c $< -o $@ >> build.log 2>&1 || \
	  { echo "❌ Compile failed for $<. Last 40 lines of build.log:"; tail -40 build.log; exit 1; }

$(DIR_OBJ)/%.o: $(DIR_LIB)/%.f
	@mkdir -p $(@D)
	$(call progress)
	@$(FC) $(F77_OPTS) -c $< -o $@ >> build.log 2>&1 || \
	  { echo "❌ Compile failed for $<. Last 40 lines of build.log:"; tail -40 build.log; exit 1; }

verify-lib: check-lib
	@echo "Verifying 2decomp library..."
	@echo "✅ Library found: $(DECOMP_LIB)"
	@echo "File type:"
	@file $(DECOMP_LIB)
	@echo ""
	@echo "Archive contents (first 10):"
	@ar -t $(DECOMP_LIB) | head -10
	@echo "..."
	@echo "Total objects: $$(ar -t $(DECOMP_LIB) | wc -l)"
	@if ar -t $(DECOMP_LIB) | grep -E "^/$$|^//" > /dev/null 2>&1; then \
		echo ""; \
		echo "⚠️  WARNING: Found suspicious '/' entries!"; \
		ar -t $(DECOMP_LIB) | grep -E "^/$$|^//"; \
	else \
		echo ""; \
		echo "✅ No suspicious entries found"; \
	fi

clean:
	@echo "Cleaning built files and directories..."
	@echo ""
	@rm -rf $(DIR_OBJ) $(DIR_BIN)/$(PROGRAM) build.log
	@[ ! -d $(DIR_BLD) ] || find $(DIR_BLD) \( -name "*.mod" -o -name "*.smod" \) -delete 2>/dev/null || true
	@echo "✅ Clean complete"
	@echo ""

help:
	@echo "CHAPSim2 Makefile - Available targets:"
	@echo ""
	@echo "  make all                       - Interactive build menu"
	@echo "  make cfg=gnu-o3 real-all        - Direct build with -O3 optimization"
	@echo "  make cfg=gnu-g real-all         - Direct build with debug symbols (-g -O0)"
	@echo "  make cfg=gnu-debug real-all     - Direct build with debug + DEBUG_STEPS"
	@echo "  make cfg=intel real-all         - Direct build with Intel compiler"
	@echo "  make cfg=cray real-all          - Direct build with Cray compiler"
	@echo "  make cfg=nvhpc-cpu real-all     - Direct build with NVHPC CPU config"
	@echo "  make clean                      - Remove build artifacts"
	@echo "  make clean && make cfg=gnu-g real-all"
	@echo "                                  - Fresh scripted debug build"
	@echo "  make verify-lib                 - Verify 2decomp library integrity"
	@echo "  make help                       - Show this help message"
	@echo ""
	@echo "Current system:"
	@echo "  OS: $(UNAME_S)"
	@echo "  Architecture: $(UNAME_M)"
	@echo "  cfg: $(cfg)"
	@echo "  Compiler: $(FC)"
	@echo "  Library: $(DECOMP_LIB)"
	@echo ""

.PHONY: all clean default verify-lib help prep-build-log real-all check-lib
