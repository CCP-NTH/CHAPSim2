#!/usr/bin/env bash
# =============================================================================
# CHAPSim regression case lists — the single definition, sourced by every runner
# (run_regression.sh locally, run_regression_tests_archer2.sh on HPC).
#
# Add a case here and it appears in every runner. The ARCHER2 driver previously
# carried its own hardcoded copy, which drifted to 17 cases against the local
# 32 — and the 15 it was missing were exactly the newest work (all MHD, all LES
# and all three high-order cases), so HPC silently validated only the parts that
# were already covered.
#
# Paths in FUNCTIONAL_CASES are relative to this directory.
# =============================================================================

STANDARD_CASES=(
  tgv_iso
  tgv_scp
  channel_iso_periodic
  channel_iso_inout
  channel_scp_periodic_Tw
  channel_scp_inout_qw
  annular_iso_periodic
  annular_scp_inout_Tw
  annular_scp_inout_qw
  pipe_iso_periodic
  pipe_scp_periodic_Tw
  pipe_scp_inout_qw
)

EXTENDED_CASES=(
  tgv_iso
  tgv_scp
  channel_iso_periodic
  channel_iso_inout
  channel_scp_periodic_Tw
  channel_scp_periodic_qw
  channel_scp_inout_Tw
  channel_scp_inout_qw
  annular_iso_periodic
  annular_iso_inout
  annular_scp_periodic_Tw
  annular_scp_periodic_qw
  annular_scp_inout_Tw
  annular_scp_inout_qw
  pipe_iso_periodic
  pipe_iso_inout
  pipe_scp_periodic_Tw
  pipe_scp_periodic_qw
  pipe_scp_inout_Tw
  pipe_scp_inout_qw
  # High-order coverage. Every case above runs cd2, so the compact/4th-order
  # boundary closures and the pipe-axis ghost layers are never exercised by them
  # (at cd2 the coefficient multiplying those ghosts is exactly zero).
  #   annular_iso_periodic_cd4 : cylindrical without the axis singularity
  #   pipe_iso_periodic_cp6    : cylindrical with it, tripping off so the random
  #                              field carries m=1 - the only mode surviving r->0
  #   channel_scp_inout_qw_cp4 : wall-bounded + thermal + inlet/outlet + compact
  annular_iso_periodic_cd4
  pipe_iso_periodic_cp6
  channel_scp_inout_qw_cp4
)

FUNCTIONAL_CASES=(
  ../functional/LES_channel_iso_peridic
  ../functional/LES_channel_scp_inout_Tw
  ../functional/MHD_channel_iso_peridic
  ../functional/MHD_channel_scp_inout_Tw
  # Both MHD cases above are Cartesian, which is how a missing r^2 weighting on
  # the electric-potential Poisson source went unnoticed: it is a no-op in a
  # channel and an O(1) charge-conservation failure in every cylindrical case.
  # This one is a pipe with a transverse B, so the axis and the r-scalings are
  # both exercised, and it is gated on max|div(j)|.
  ../functional/MHD_pipe_iso_periodic
  # Every case above ships a wall-normal B = (0, By, 0). Under that field five of
  # the nine products in cross_production_mhd vanish identically, so bx and bz -
  # and the interpolation and transpose chains that place them on the staggered
  # faces - are never executed at all. These two carry B = (10, 20, 5) so all nine
  # are live, in Cartesian and in cylindrical (where a general transverse field
  # also exercises both the Br and the Btheta halves of the m=1 decomposition and
  # the opposite axis parities of by = r*Br, even, and bz = Btheta, odd).
  ../functional/MHD_channel_iso_periodic_Btilt
  ../functional/MHD_pipe_iso_periodic_Btilt
  # Both LES cases above are Cartesian, which is how the WALE model came to be
  # fed a Cartesian velocity-gradient tensor in cylindrical coordinates: the
  # derivatives were taken of the stored variables (qy = r*u_r) with none of the
  # 1/r or u/r metric terms, and nothing gated LESmode on the coordinate system.
  # The annulus is cylindrical but never touches the axis, so it isolates the
  # metric terms; the pipe adds the axis reconstruction on top (tripping off, so
  # the random field carries m=1 - the only mode that survives r->0).
  ../functional/LES_annular_iso_periodic
  ../functional/LES_pipe_iso_periodic
  # Those two gate the cylindrical gradient assembly only through the strength of
  # their baselines: both would still pass if the metric terms came back slightly
  # wrong, because nothing in their references is an exact quantity. This one is.
  # Its initial field is a rigid rotation u_theta = Omega*r, for which S_ij = 0
  # identically on any grid, so "max. initial strain rate S_ijS_ij" is gated at
  # 1e-20 absolute and the Cartesian assembly scores Omega^2/2 = 0.5 - twenty
  # orders of separation, with no tolerance to argue about. It is also the only
  # case whose reference does not have to be regenerated when a baseline moves.
  ../functional/LES_pipe_solidrotation
  # Every case above is z-periodic, so no shipped case ever entered the
  # z-direction wall branches: the thermal momentum assembly's fbcz property
  # buffers, the z half of the subgrid boundary classification, and three
  # transposes whose decomposition descriptor is only wrong when a z-staggered
  # extent differs from the cell-centred one (it cannot, under periodicity).
  # This duct has no-slip isothermal walls in BOTH y and z with an x
  # inlet/outlet, and is heated hard enough at all four walls that the subgrid
  # enthalpy flux is O(1e-4) rather than the O(1e-9) of the existing thermal
  # LES case - large enough that a wrong wall closure moves the metric.
  ../functional/LES_duct_scp_zwall_Tw
  # The same duct with ithermo= .false. - the inputs differ in that one line.
  # The thermal case cannot cover the isothermal momentum branch, and that branch
  # holds the fourth descriptor fix (qxiz_pcp_zpencil must transpose with dpcp,
  # not dpcc). Like the three thermal ones it is a no-op under z periodicity and
  # an out-of-bounds access on a z wall at NP > 1, so this pair is what makes all
  # four reachable. It also pins the isothermal floor: with no SGS contribution
  # on a wall face the total face viscosity is exactly the molecular 1.0.
  ../functional/LES_duct_iso_zwall
)
