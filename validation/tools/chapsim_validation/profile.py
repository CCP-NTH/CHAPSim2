"""Readers for CHAPSim2 time-and-space averaged profile outputs."""

from __future__ import annotations

from pathlib import Path

import numpy as np


def load_profile_column(
    input_dir: str | Path,
    dns_time: str | int,
    name: str,
    domain_id: int = 1,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Load one averaged profile as index, coordinate, value arrays.

    Supports legacy per-field files and current bundled profile files.
    """
    root = Path(input_dir)
    label = str(dns_time)

    split_path = root / f"domain{domain_id}_tsp_avg_{name}_{label}.dat"
    if split_path.exists():
        return _load_three_column_profile(split_path)

    field_name = name if name.startswith("tsp_avg_") else f"tsp_avg_{name}"
    for bundle_path in _candidate_bundle_paths(root, label, domain_id):
        columns = _read_profile_columns(bundle_path)
        if field_name not in columns:
            continue
        data = np.loadtxt(bundle_path, comments="#")
        data = np.atleast_2d(data)
        column_index = columns.index(field_name)
        return data[:, 0], data[:, 1], data[:, column_index]

    raise FileNotFoundError(
        f"Could not find profile '{name}' for iteration {label} under {root}. "
        "Checked legacy split files and bundled CHAPSim_profile_ascii_v1 files."
    )


def _load_three_column_profile(path: Path) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    data = np.loadtxt(path)
    data = np.atleast_2d(data)
    if data.shape[1] < 3:
        raise ValueError(f"{path} must contain at least 3 columns: index, coordinate, value")
    return data[:, 0], data[:, 1], data[:, 2]


def _candidate_bundle_paths(root: Path, label: str, domain_id: int) -> list[Path]:
    preferred = root / f"domain{domain_id}_tsp_avg_flow_yprofile_{label}.dat"
    candidates = []
    if preferred.exists():
        candidates.append(preferred)
    candidates.extend(
        path
        for path in sorted(root.glob(f"domain{domain_id}_tsp_avg_*_yprofile_{label}.dat"))
        if path != preferred
    )
    return candidates


def _read_profile_columns(path: Path) -> list[str]:
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            stripped = line.strip()
            if stripped.startswith("# columns:"):
                return stripped.split(":", 1)[1].split()
            if not stripped.startswith("#"):
                break
    raise ValueError(f"{path} does not contain a '# columns:' profile header")
