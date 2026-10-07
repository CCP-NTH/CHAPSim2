#!/usr/bin/env python3
"""Check inlet/outlet database filename-offset support.

The offset lets a run read a database written under a different iteration
numbering (`ndb_file_offset`). It only works if *every* path from a solver
iteration to a database file name goes through `xoutlet_database_file_iter`.
A single call site that passes the raw iteration instead reads or writes the
wrong file, and does so silently.

This checks that property rather than exact call text: the earlier version
pinned two literal call strings and went stale the moment the bundled read
was routed through `read_xoutlet_database_bundle_interp`, while the offset
itself was still applied correctly.
"""

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "src"
MODULES = (SRC / "modules.f90").read_text()
INPUT_GENERAL = (SRC / "input_general.f90").read_text()
IO_RESTART = (SRC / "io_restart.f90").read_text()

HELPER = "xoutlet_database_file_iter"

# The two entry points that turn a solver iteration into a database file name.
ENTRY_POINTS = ("write_instantaneous_xoutlet", "read_instantaneous_xinlet")

# Calls inside those entry points that take a file index, and the 1-based
# position of that index in the argument list. Positions follow the signatures
# in io_tools.f90 (read/write_one_3d_array, generate_pathfile_name) and
# io_restart.f90 (the bundle helpers).
FILENAME_CONSUMERS = {
    "read_one_3d_array": 4,
    "write_one_3d_array": 4,
    "generate_pathfile_name": 6,
    "cleanup_xoutlet_database_bundle_files": 2,
    "read_xoutlet_database_bundle": 2,
    "read_xoutlet_database_bundle_interp": 2,
    "read_xoutlet_database_per_field_interp": 2,
    "write_xoutlet_database_bundle": 2,
}
CONSUMER_CALL = re.compile(
    r"\b(" + "|".join(sorted(FILENAME_CONSUMERS, key=len, reverse=True)) + r")\s*\("
)


def require(text: str, pattern: str, description: str) -> None:
    if pattern not in text:
        raise AssertionError(f"Missing {description}: {pattern}")


def strip_comments(text: str) -> str:
    """Drop Fortran `!` comments, respecting quoted strings.

    Needed because the write side keeps a commented-out pressure-database
    block that still names a raw iteration; analysing it would report a bug in
    code that never runs.
    """
    out = []
    for line in text.splitlines():
        quote = ""
        cut = len(line)
        for i, ch in enumerate(line):
            if quote:
                if ch == quote:
                    quote = ""
            elif ch in "'\"":
                quote = ch
            elif ch == "!":
                cut = i
                break
        out.append(line[:cut])
    return "\n".join(out)


def routine_body(text: str, name: str) -> str:
    """Return the source of subroutine `name`, without its `end subroutine`."""
    match = re.search(
        rf"^\s*subroutine\s+{name}\b.*?^\s*end subroutine",
        text,
        re.MULTILINE | re.DOTALL,
    )
    if not match:
        raise AssertionError(f"Cannot find subroutine {name} in io_restart.f90")
    return match.group(0)


def call_arguments(body: str, open_paren: int) -> str:
    """Return the argument text of a call whose '(' sits at `open_paren`."""
    depth = 0
    for i in range(open_paren, len(body)):
        if body[i] == "(":
            depth += 1
        elif body[i] == ")":
            depth -= 1
            if depth == 0:
                args = body[open_paren + 1 : i]
                # Fortran line continuations split arguments across lines.
                return re.sub(r"&\s*(?:!.*)?\n\s*", " ", args)
    raise AssertionError("Unbalanced parentheses in io_restart.f90")


def split_arguments(args: str) -> list[str]:
    """Split a Fortran argument list on top-level commas."""
    parts: list[str] = []
    depth = 0
    quote = ""
    current = ""
    for ch in args:
        if quote:
            current += ch
            if ch == quote:
                quote = ""
            continue
        if ch in "'\"":
            quote = ch
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        elif ch == "," and depth == 0:
            parts.append(current.strip())
            current = ""
            continue
        current += ch
    parts.append(current.strip())
    return parts


def check_offset_reaches_every_filename() -> None:
    """No raw solver iteration may become a database file index."""
    source = strip_comments(IO_RESTART)
    for name in ENTRY_POINTS:
        body = routine_body(source, name)

        # A routine may hoist the conversion into a local, as the write side
        # does with `file_iter = xoutlet_database_file_iter(dm, iter)`.
        hoisted = set(re.findall(rf"(\w+)\s*=\s*{HELPER}\s*\(", body))

        # A reader or writer added under a new name would otherwise slip past
        # the table above unchecked — which is how the previous version of
        # this checker went stale.
        for callee in re.findall(r"call\s+((?:read|write)_xoutlet_database_\w+)", body):
            if callee not in FILENAME_CONSUMERS:
                raise AssertionError(
                    f"{name}: calls {callee}, which is not in FILENAME_CONSUMERS; "
                    f"add it with the position of its file-index argument"
                )

        checked = 0
        for call in CONSUMER_CALL.finditer(body):
            callee = call.group(1)
            args = split_arguments(call_arguments(body, call.end() - 1))
            position = FILENAME_CONSUMERS[callee]
            if len(args) < position:
                raise AssertionError(
                    f"{name}: call to {callee} has {len(args)} arguments, "
                    f"fewer than the file index at position {position} — the "
                    f"signature changed and this checker needs updating"
                )
            index = args[position - 1]
            if index.startswith(f"{HELPER}(") or index in hoisted:
                checked += 1
                continue
            raise AssertionError(
                f"{name}: the file index passed to {callee} bypasses "
                f"{HELPER}: {index!r}"
            )

        if checked == 0:
            raise AssertionError(
                f"{name}: found no database filename call to check — the "
                f"routine was renamed or restructured, so this checker is "
                f"asserting nothing"
            )


def main() -> None:
    require(MODULES, "integer :: ndb_file_offset", "domain database file-offset field")
    require(INPUT_GENERAL, "'ndb_file_offset'", "optional input key")
    require(INPUT_GENERAL, "domain(:)%ndb_file_offset", "domain-wide offset propagation")
    require(IO_RESTART, HELPER, "database filename-offset helper")
    require(IO_RESTART, "file_iter = iter + dm%ndb_file_offset", "offset arithmetic")
    check_offset_reaches_every_filename()
    print("Inlet/outlet database filename offset OK")


if __name__ == "__main__":
    main()
