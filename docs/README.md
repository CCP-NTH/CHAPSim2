# CHAPSim2 Documentation

CHAPSim2 documentation is organized into three components:

- **User Guidance**: Comprehensive documentation covering installation, benchmark cases, practical workflows, input reference, numerical methodology, and troubleshooting
- **Code Structure Reference**: Automatically generated FORD documentation providing Fortran API and source-code structure details
- **Diagrams**: Source diagrams used by guidance pages, diagnostics, and architecture notes

## Accessing the Documentation

Browser access to documentation:

```bash
google-chrome docs/guidance/html/index.html
```

## Documentation Regeneration

**User Guidance (MkDocs format):**

Regerate the user guidance documentation from source Markdown files (requires MkDocs):

```bash
cd docs/guidance/
mkdocs build
```

**Code Structure Reference (FORD format):**

Regenerate the Fortran API reference from source annotations:

```bash
cd docs/code_structure/
ford ford.md
```

The output lands in the git-ignored `docs/code_structure/doc/`; copy it over the
published copy in `docs/code_structure/` once reviewed. See
`docs/code_structure/README.md` for why the project file must stay in Markdown form.

**Diagrams:**

Source diagrams are maintained under:

```text
docs/diagrams/
```

**Static HTML Preview (no dependencies required):**

If MkDocs is unavailable, regenerate static HTML previews:

```bash
python3 docs/guidance/build_static.py
```
