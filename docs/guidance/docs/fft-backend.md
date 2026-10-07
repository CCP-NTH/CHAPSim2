# FFT Backend: Generic or FFTW

The pressure-Poisson solver is solved spectrally, so every timestep runs a set of
forward and inverse FFTs through 2decomp-fft. Two transform backends are
available, and the choice is made **when the 2decomp-fft library is built**, not
at run time.

| Backend | Where it comes from | When to use it |
| --- | --- | --- |
| `generic` (**default**) | The Glassman transform bundled inside `lib/2decomp-fft`. No external dependency. | Default for all builds. Portable, reproducible, and what the GitHub Actions test suites run. |
| `fftw_f03` | A separately installed [FFTW 3](https://www.fftw.org/) using its Fortran 2003 interface. | Production runs, where FFT throughput matters. |

**The default is `generic` and nothing needs to be installed or configured to
get it.** FFTW is an explicit opt-in.

## Building with the Default Generic Backend

Nothing to do:

```bash
./build_chapsim.sh
# or, without the prompts:
CHAPSIM_MODE=non-interactive ./build_chapsim.sh
```

The build prints the backend it selected:

```
FFT backend selected: generic
```

If FFTW happens to be installed on the machine, the build notes that it is
available but not enabled, and tells you how to turn it on. It does **not**
switch to it on its own.

## Switching On FFTW

### 1. Install FFTW (double precision)

CHAPSim2 is built with `-DDOUBLE_PREC`, so it needs the double-precision library
`libfftw3`. A single-precision-only install (`libfftw3f` alone) will not link.

| Platform | How |
| --- | --- |
| Debian / Ubuntu | `sudo apt install libfftw3-dev` (installs under `/usr`) |
| macOS (Homebrew) | `brew install fftw` (prefix from `brew --prefix fftw`) |
| From source | `./configure --prefix=/usr/local && make && make install` |
| ARCHER2 / Cray | `module load cray-fftw` |

### 2. Build with `CHAPSIM_FFT=fftw`

```bash
CHAPSIM_FFT=fftw ./build_chapsim.sh
```

If FFTW is not in the default prefix, point `FFTW_ROOT` at it:

```bash
CHAPSIM_FFT=fftw FFTW_ROOT=/usr ./build_chapsim.sh          # Debian/Ubuntu package
CHAPSIM_FFT=fftw FFTW_ROOT=/opt/fftw-3.3.10 ./build_chapsim.sh
```

| Variable | Default | Meaning |
| --- | --- | --- |
| `CHAPSIM_FFT` | `generic` | `generic`, or `fftw` (`fftw_f03` is accepted as the same thing). Case-insensitive. |
| `FFTW_ROOT` | `/usr/local` | Installation prefix of FFTW. The build looks for `include/fftw3.h` or `include/fftw3.f03`, and for `libfftw3.*` under `lib`, `lib64` or a multiarch directory such as `lib/x86_64-linux-gnu`. |

On ARCHER2 and other Cray programming environments the build detects the
environment and loads `cray-fftw` itself, setting `FFTW_ROOT` from
`CRAY_FFTW_PREFIX`. You still have to ask for the backend with
`CHAPSIM_FFT=fftw`.

### 3. Switching back

```bash
./build_chapsim.sh     # CHAPSIM_FFT unset means generic
```

Changing the backend **rebuilds the 2decomp-fft library and the solver from
clean**, automatically, because the transform is compiled into
`libdecomp2d.a` and its module files. The build detects the mismatch against the
previously built backend and forces the rebuild rather than silently reusing a
library that does not match the request. Expect the full rebuild time, not an
incremental one.

## If FFTW Is Requested But Not Found

The build **stops with an error**. It does not quietly fall back to the generic
backend, because a silent downgrade would leave you believing a production run
was using FFTW when it was not. The message reports the prefix searched and
whether the header and the library were each found:

```
❌ CHAPSIM_FFT=fftw was requested, but no usable FFTW was found.
   Looked under FFTW_ROOT: /usr/local
   header (include/fftw3.h or fftw3.f03): false
   library (lib, lib64 or lib/<arch>-linux-gnu): not found
```

Install FFTW, or correct `FFTW_ROOT`, or drop `CHAPSIM_FFT` to build generic.

## Verifying Which Backend a Binary Actually Uses

Do not rely on a build banner alone. Three independent checks:

```bash
# 1. What the build selected
./build_chapsim.sh 2>&1 | grep 'FFT backend selected:'

# 2. What the library was recorded as, and how the solver links
cat lib/2decomp-fft/build/opt/chapsim_fft_backend.mk

# 3. What is actually in the executable: 0 for generic, several hundred for FFTW
nm bin/CHAPSim | grep -c fftw
```

The marker file is written by the library build and read by `build/Makefile`, so
the link flags always match the library. For a generic build it reads:

```make
CHAPSIM_FFT_BACKEND = generic
CHAPSIM_FFT_LDFLAGS =
```

and for an FFTW build, something like:

```make
CHAPSIM_FFT_BACKEND = fftw_f03
CHAPSIM_FFT_LDFLAGS = -L/usr/local/lib -lfftw3
```

## What Changes When You Switch

**Results change at roundoff level.** The two backends compute the same
transform by different algorithms, so they do not produce bit-identical fields.
Measured across the 12 standard regression cases (280 reported metrics):

| | |
| --- | --- |
| Bitwise identical metrics | 157 / 280 |
| Largest absolute difference | `1.0e-05` on a pressure drop of `-1500.69`, i.e. `6.7e-09` relative |
| Largest relative difference on any metric of non-trivial magnitude | `1.3e-07` |

Both backends pass the standard suite 12/12 against the committed references,
with every gated metric far inside tolerance. The reference values themselves
were generated with the generic backend. Larger relative differences appear only
on metrics whose true value is zero (machine-zero Poisson projection residuals),
where a relative comparison is meaningless.

**Performance is expected to improve, but the figure below is not a benchmark.**
On the regression cases — single run per case, `NP=4`, a shared desktop, 30-second
cases dominated by setup and I/O — the suite total was 394.3 s with generic
against 364.9 s with FFTW, a factor of 1.08. These cases spend only a small
fraction of each step in the FFT, so treat 8% as a weak lower bound. On
production meshes the FFT fraction is far larger and the gap should widen.
Measure it on your own case and mesh before quoting a number.

## Choosing

- **Testing, CI, regression work, sharing a case with someone else:** generic.
  No dependency to install, nothing to go wrong, and it is what the reference
  metrics were produced with.
- **Production runs on large meshes:** FFTW, after verifying with `nm` that the
  binary really has it.

## See Also

- [Installation and First Run](installation.md)
- [Regression and Smoke Tests](testing.md)
- [Troubleshooting](troubleshooting.md)
