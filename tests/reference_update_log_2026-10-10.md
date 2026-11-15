# Reference update 2026-10-10 — Bc formulation, full-step pressure metrics, inlet-temperature fix (approved by Wei Wang 2026-10-10)

Approved proposals applied as separate commits (A1, A2, A3 + harness, B, C, C-Bc). Evidence: the development review records of the CPC B3 review (case review, final reference proposals A, B and C, final report), which are not part of this repository.

## Solver changes behind the moves
* Bc transported-density recovery (patches 00–05, 08) replaces the lagged one-pass recovery (Δt-independent
  instability above λ ≈ 0.5; pressure component ∝ 1/Δt).
* Full-step pressure output P̄ = Σ α_k p^k (`pr_fullstep`, patches 11, 11b, 13, 17), output only.
* Inlet-temperature input fix (patches 20/21, 24/24b) and functional input update (25): `tini` token, explicit values
  never replaced, Dirichlet boundary temperatures range-checked.

## Baselines that would move
| Patch | Cases | Nature | Basis |
|---|---|---|---|
| 18a A1 | channel/annular/pipe_scp_inout_{Tw,qw}, channel_scp_inout_qw_cp4 (7) | **numerical regression references** (not physical validation) for the Bc formulation; p³ keys kept at Bc values as stage-3 regression values | CPC_B3_REFERENCE_A_CASE_REVIEW (closure 1e-13…4e-9, c = 0, R_M ≤ 2e-19, wall heat = imposed) |
| 18b A2 | F:LES_channel_scp_inout_Tw, F:MHD_channel_scp_inout_Tw, F:LES_duct_scp_zwall_Tw (3) | **regression-only**; SGS and Lorentz coupling not independently verified | same review |
| 18c A3 + 27 | tgv_scp, channel/annular/pipe_scp_periodic_{Tw,qw} (7) | regression references for the usable metrics; mass metrics (`global mass balance`, `total mass drift from run start`, `total mass`) tagged `known_limitation` in `tolerances.json` (closed-domain compatibility, not physical mass conservation) — still gated against the documented algorithm output; harness prints `[LIMIT]` | CPC_B3_CLOSED_DOMAIN_FINDING, CPC_B3_REFERENCE_A_CASE_REVIEW |
| 19 B v2 (+ final tolerance note) | channel_iso_inout, channel_scp_inout_qw, annular_scp_inout_{Tw,qw}, pipe_scp_inout_qw (5 × 2 keys) | new P̄ keys; runner exports `CHAPSIM_B3_PBAR=1`; new-key tolerances rel 1e-3, abs 1e-8 | CPC_B3_REFERENCE_B_REVIEW, portability study `cases/pbtol` |
| 23 C | mesh_mapping/channel_scp_inout step5_clamp_run, step7_extend_run | old references encode the 0 K inlet defect | CPC_B3_REFERENCE_C_REVIEW |

## Untouched
All isothermal, isothermal-LES/MHD cases (unchanged results); existing tolerances (A3 adds only labels and two gated
mass keys with rel tolerances; B adds tolerances for new keys only); inlet_database (blocked: missing dataset).

## Checker semantics (B tolerances)
`check_metrics.py` fails a metric only when every applicable criterion fails (abs OR rel passes). For the new P̄ keys
(abs 1e-8, rel 1e-3) the relative criterion governs (smallest |value| 6.0e-4); measured spread across NP 1/2/4/8, two
NP-4 layouts and a −O0 build: 2.6e-5 relative (39× margin).

## Historical references
The previous values remain in git history (parent commits). The mesh-mapping references replaced by C were generated
with an inlet at 0 K (input-default defect) and are not physically meaningful; the legacy thermal references contained
the 1/Δt pressure component of the one-pass recovery.
