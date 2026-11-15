#!/usr/bin/env bash
# Numerical reproducer for the singular line of poisson_11x_yskip with z walls.
#   tests/tools/check_yskip_zero_mode_zwalls.sh BEFORE_EXE AFTER_EXE WORKDIR
# BEFORE_EXE: a build without the fix that carries the diagnostic;
# AFTER_EXE: the fix. Both must carry the CHAPSIM_DIAG_YSKIP_ZERO diagnostic.
#
# Null space and compatibility. With Neumann pressure on every non-periodic side (x inlet
# and outlet, y and z walls) the discrete Poisson operator has the constants as its null
# space; after the x and z transforms that is the line with zero x and z wavenumbers, a
# y-tridiagonal Neumann operator A with right null vector 1 and left null vector w (the
# volume weights, built as in solve_yskip_tdma_line). A x = r is solvable only if
# w.r = 0; the solver must remove the component (w.r / sum w) and fix the constant
# (gauge row). With z walls the real part of (i, k) = (1, 1) is that line.
#
# Runs tests/functional/LES_duct_scp_zwall_Tw (inputs unchanged) with each executable and
# checks:
#   before: the singular line is solved with zero_mode = F; its residual |A x - r| exceeds
#           the compatibility value (the incompatible part is left in the last row; the
#           constant is fixed only by the zero-pivot guard of Solve_TDMA_standard);
#   after:  zero_mode = T; |A x - (r - compat)| <= 1e-12 max|r| on every solve; x(n) = 0;
#   both:   divergence after projection and the global mass imbalance <= 1e-12, wall
#           velocities exactly zero (no penetration: the Neumann pressure is consistent);
# and prints the field differences after - before (the deliberate change).
# Unaffected paths (results bitwise unchanged by the fix) are checked by the caller with
# a z-periodic heated case and the isothermal z-wall case. Exit 0 if every check passes.
set -uo pipefail
B=${1:?before exe}; A=${2:?after exe}; W=${3:?workdir}
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"; CASE="$HERE/../functional/LES_duct_scp_zwall_Tw"
fail=0
for v in before after; do
  d="$W/$v"; rm -rf "${d:?}"; mkdir -p "$d"; cp -r "$CASE/." "$d/"; rm -f "$d"/output_chapsim2_*.log "$d/regression_test_metrics.json"
  exe=$B; [ $v = after ] && exe=$A
  ( cd "$d" && CHAPSIM_DIAG_YSKIP_ZERO=1 mpirun -np 1 "$exe" > run.log 2>&1 )
  grep -q "finished successfully" "$d/run.log" || { echo "$v: run failed"; exit 1; }
done
python3 - "$W" <<'PY' || fail=1
import sys, re, os, numpy as np
W = sys.argv[1]; bad = False
def diag(v):
    rows = []
    for l in open(os.path.join(W, v, "run.log")):
        if "DIAG_YSKIP0" in l:
            t = l.split(); rows.append((t[-6], *map(float, t[-5:])))
    return rows
def logmax(v, key):
    m, on = 0.0, False
    for l in open(os.path.join(W, v, "run.log")):
        if "Time Step =" in l: on = True
        if on and key in l:
            n = re.findall(r"[-+]?\d\.\d+E[-+]\d+", l)
            if n: m = max(m, abs(float(n[0])))
    return m
b, a = diag("before"), diag("after")
print("solves of the singular line: before %d, after %d" % (len(b), len(a)))
fb = all(r[0] == "F" for r in b); fa = all(r[0] == "T" for r in a)
cmax = max(abs(r[1]) for r in a); rb = max(r[2] for r in b); ra = max(r[3] for r in a); xa = max(abs(r[5]) for r in a)
print("before: zero_mode F on every solve %s; max |A x - r|/max|r| %.2e (max compatibility value %.2e)" % (fb, rb, cmax))
print("after:  zero_mode T on every solve %s; max |A x - (r - compat)|/max|r| %.2e; max |x(n)| %.1e" % (fa, ra, xa))
bad |= not (fb and rb > cmax and fa and ra <= 1e-12 and xa == 0.0)
for v in ("before", "after"):
    vals = {k: logmax(v, k) for k in ("Physical- Mass Consv. (bulk", "Physical- Mass Consv. (inlet",
                                      "Physical- Mass Consv. (outlet", "global mass flux imbalance")}
    print("%s: %s" % (v, ", ".join("%s %.2e" % (k.split("(")[-1].strip() if "(" in k else k, x) for k, x in vals.items())))
    bad |= max(vals.values()) > 1e-12
sys.exit(1 if bad else 0)
PY
for v in before after; do
  f=$(ls "$W/$v/1_data" | sed -n 's/^domain1_flow_restart_\([0-9]*\)\.bin$/\1/p' | sort -n | tail -n 1)
  python3 - "$W/$v" "$f" <<'PY' || fail=1
import sys, os, numpy as np
run, it = sys.argv[1], int(sys.argv[2])
meta = dict(l.strip().split(None, 1) for l in open(os.path.join(run, "1_data", "domain1_flow_restart_meta_%d.dat" % it)) if len(l.split()) > 1)
names = [e.split(":")[0].strip() for e in meta["fields"].split(",") if e.strip()]
shp = {e.split("=")[0].strip(): tuple(int(x) for x in e.split("=")[1].split(",")) for e in meta["field_shapes"].split(";") if e.strip()}
raw = np.fromfile(os.path.join(run, "1_data", "domain1_flow_restart_%d.bin" % it), dtype="<f8"); o = 0; f = {}
for n in names:
    k = int(np.prod(shp[n])); f[n] = raw[o:o+k].reshape(shp[n], order="F"); o += k
w = max(np.abs(f["qz"][:, :, 0]).max(), np.abs(f["qz"][:, :, -1]).max(), np.abs(f["qy"][:, 0, :]).max(), np.abs(f["qy"][:, -1, :]).max())
print("%s: wall-normal velocity on the walls %.1e" % (os.path.basename(run), w))
sys.exit(0 if w == 0.0 else 1)
PY
done
[ $fail = 0 ] && echo "REPRODUCER PASS (defect shown before, resolved after)" || echo "REPRODUCER FAIL"
exit $fail
