# Third-party components and data

CHAPSim2 itself is distributed under the BSD 3-Clause licence in `LICENSE`
(Copyright (c) 2026, Science and Technology Facilities Council (STFC), UKRI).
This file lists the components distributed with CHAPSim2 that are covered by
their own terms, and the notices those terms require.

## 1. FISHPACK 4.1: FFTPACK routines (`lib/fishpack4.1/`)

- **Files.**
  - `lib/fishpack4.1/fftpack.f`: compiled into the solver. It is used only by
    the alternative FFT backend of the Poisson solver (`FFT_FISHPACK_2DFFT`),
    kept as a backup and for testing; the default path uses 2DECOMP&FFT.
  - `lib/fishpack4.1/fftpack.f.singleprecision`: the unmodified single-precision
    original, kept for reference.
- **Origin.** FFTPACK as distributed in FISHPACK version 4.1 by the National
  Center for Atmospheric Research (NCAR), Computational and Information Systems
  Laboratory (CISL), University Corporation for Atmospheric Research (UCAR).
  Author: Paul N. Swarztrauber. Latest revision November 1988 (version 4.1).
  Each file keeps its original banner, "copyright (c) 1999 by UCAR".
- **Modifications in CHAPSim2.** `fftpack.f` was converted to double precision.
  The changes are a `WPFISHPACK` kind module, `REAL(WP)` declarations with
  `INTENT`, and `_WP` literal constants. The algorithms are unchanged.
- **Licence.** The FISHPACK Software License, as published by NCAR/CISL with the
  FISHPACK distribution at
  `https://www2.cisl.ucar.edu/resources/legacy/fishpack/license`, retrieved from
  the Internet Archive (snapshot 2021-12-22) on 2026-11-13. Reproduced in full:

> Copyright © 2004 the University Corporation for Atmospheric Research ("UCAR").
> All rights reserved. Developed by NCAR's Computational and Information Systems
> Laboratory, UCAR.
>
> Redistribution and use of the Software in source and binary forms, with or
> without modification, is permitted provided that the following conditions are
> met:
>
> - Neither the names of NCAR's Computational and Information Systems
>   Laboratory, the University Corporation for Atmospheric Research, nor the
>   names of its sponsors or contributors may be used to endorse or promote
>   products derived from this Software without specific prior written
>   permission.
> - Redistributions of source code must retain the above copyright notices, this
>   list of conditions, and the disclaimer below.
> - Redistributions in binary form must reproduce the above copyright notice,
>   this list of conditions, and the disclaimer below in the documentation
>   and/or other materials provided with the distribution.
>
> THIS SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
> IMPLIED, INCLUDING, BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
> CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, INDIRECT,
> INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES OR OTHER LIABILITY,
> WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
> CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
>
> FISHPACK is a product of the Computational & Information Systems Laboratory at
> the National Center for Atmospheric Research (NCAR).

CHAPSim2 is not endorsed by NCAR, CISL or UCAR.

## 2. 2DECOMP&FFT (`lib/2decomp-fft/`)

Pencil decomposition and parallel FFT library, BSD 3-Clause. See
`lib/2decomp-fft/LICENSE` and `lib/2decomp-fft/CONTRIBUTORS`, distributed
unchanged with the library.

## 3. Generated documentation assets (`docs/code_structure/`)

The FORD-generated code reference includes:

- **Font Awesome Free** (`css/*fontawesome*`, `css/{brands,regular,solid,v4-*}*.css`,
  `webfonts/fa-*`): icons CC BY 4.0, fonts SIL Open Font License 1.1, code MIT.
  Notices are kept in the CSS headers; see https://fontawesome.com/license/free.
- **svg-pan-zoom** (`js/svg-pan-zoom.min.js`): BSD 2-Clause,
  https://github.com/bumbu/svg-pan-zoom.
- **`css/pygments.css`, `css/local.css`:** style sheets produced by Pygments
  (BSD 2-Clause) and FORD.

## 4. Thermophysical property data (`tests/`, `validation/thermal_properties/`)

- **Supercritical-water table.** Every `NIST_WATER_23.5MP.DAT` in this
  repository is generated with CoolProp 8.0.0, using its IAPWS-95 equation of
  state and its viscosity and thermal-conductivity correlations. It was
  produced by `validation/thermal_properties/generate_property_table.py`. The
  file name is kept because the solver reads that name; the contents are not
  NIST data. CoolProp is distributed under the MIT licence; it is not
  distributed with CHAPSim2.
- **NIST tables.** No NIST Standard Reference Data table is distributed with
  CHAPSim2 (the water or CO2 tables generated with the NIST WebBook). Users
  supply their own tables, from NIST under NIST's terms or generated with the
  script above.
