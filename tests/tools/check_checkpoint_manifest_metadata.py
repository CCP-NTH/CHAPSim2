#!/usr/bin/env python3
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RESTART_SRC = ROOT / "src" / "io_restart.f90"
METADATA_SRC = ROOT / "src" / "io_metadata.f90"
# Tracked legacy artefact. This used to point at tests/channel_iso_periodic/1_data,
# an untracked leftover run directory, so the check could never run on a clean
# checkout. See tests/tools/fixtures/README.md.
SAMPLE = ROOT / "tests" / "tools" / "fixtures"


def require(condition, message):
    if not condition:
        raise SystemExit(message)


restart_text = RESTART_SRC.read_text()
metadata_text = METADATA_SRC.read_text()
text = restart_text + "\n" + metadata_text
require("write_checkpoint_manifest" in text, "missing write_checkpoint_manifest helper")
require("read_checkpoint_restart_metadata" in text, "missing manifest-aware reader")
require("'checkpoint_meta'" in text, "checkpoint_meta file keyword is not generated")
require("'CHAPSim_checkpoint_v1'" in text, "checkpoint manifest version header is not written/read")
require(
    "call read_checkpoint_restart_metadata(dm%idom, 'flow_meta'" in restart_text,
    "flow restart reader does not prefer checkpoint manifest",
)
require(
    "call read_checkpoint_restart_metadata(dm%idom, 'thermo_meta'" in restart_text,
    "thermo restart reader does not prefer checkpoint manifest",
)
require(
    "use checkpoint_metadata_mod, only: write_checkpoint_manifest" in (ROOT / "src" / "post_statistics.f90").read_text(),
    "statistics bundle writers do not refresh checkpoint manifest",
)
require(
    "call write_restart_metadata(dm%idom, 'flow_meta'" not in restart_text,
    "flow restart writers still emit legacy flow_meta",
)
require(
    "call write_restart_metadata(dm%idom, 'thermo_meta'" not in restart_text,
    "thermo restart writers still emit legacy thermo_meta",
)
require(
    "call read_restart_metadata(idom, keyword, iter, time, dt, found)" in restart_text,
    "legacy scalar metadata fallback was removed",
)
require(
    "remove_legacy_restart_metadata" in restart_text,
    "writers do not remove stale legacy scalar metadata in overwrite mode",
)
require(
    "call remove_legacy_restart_metadata(dm%idom, 'flow_meta'" in restart_text,
    "flow checkpoint writer does not remove stale flow_meta",
)
require(
    "call remove_legacy_restart_metadata(dm%idom, 'thermo_meta'" in restart_text,
    "thermo checkpoint writer does not remove stale thermo_meta",
)

sample_file = SAMPLE / "domain1_flow_meta_50.dat"
require(
    sample_file.is_file(),
    f"legacy flow_meta fixture is missing: {sample_file.relative_to(ROOT)}",
)
flow_meta = sample_file.read_text()
require("iteration 50" in flow_meta, "sample legacy flow_meta is missing iteration")
require(
    "time" in flow_meta and "dt" in flow_meta,
    "sample legacy flow_meta is missing scalar restart state",
)
# The fixture only matters if the fallback reader still parses this layout, so
# pin the writer that produced it as well.
require(
    "'iteration', iter" in restart_text,
    "legacy metadata writer no longer emits the iteration label",
)
require(
    "'time',      time" in restart_text and "'dt',        dt" in restart_text,
    "legacy metadata writer no longer emits the time/dt labels",
)

print("Checkpoint manifest metadata checks OK")
