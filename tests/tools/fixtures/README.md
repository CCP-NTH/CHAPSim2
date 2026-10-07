# tests/tools/fixtures

Small tracked inputs for the standalone checkers in `tests/tools/`. A checker must not
depend on untracked run output, or it cannot run in CI.

| File | Used by | Provenance |
| --- | --- | --- |
| `domain1_flow_meta_50.dat` | `check_checkpoint_manifest_metadata.py` | Byte-for-byte copy of a real legacy scalar metadata file, as written by `write_restart_metadata` (`src/io_restart.f90:1483-1485`): `iteration`, `time`, `dt` label-value lines. Kept so the legacy fallback reader `read_restart_metadata` keeps being exercised against a genuine pre-manifest artefact rather than a hand-written one. |

Do not regenerate these from the current solver without saying why in the work log — the
point of the legacy one is that it predates the checkpoint manifest.
