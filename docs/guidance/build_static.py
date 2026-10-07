#!/usr/bin/env python3
"""Build lightweight static HTML previews for the guidance Markdown files.

This is intentionally small and dependency-free. It supports the Markdown
features used in this guide: headings, paragraphs, bullet/numbered lists,
tables, fenced code blocks, inline code, and simple links.
"""

from __future__ import annotations

import html
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parent
SOURCE_DIR = ROOT / "docs"
OUTPUT_DIR = ROOT / "html"

SECTIONS = [
    ("Home", [("index.md", "index.html", "Overview")]),
    (
        "Getting Started",
        [
            ("installation.md", "installation.html", "Installation"),
            ("fft-backend.md", "fft-backend.html", "FFT Backend"),
        ],
    ),
    ("Benchmark Cases", [("benchmark-cases.md", "benchmark-cases.html", "Benchmark and Validation Cases")]),
    (
        "User Guide and How-To's",
        [
            ("user-guide.md", "user-guide.html", "Overview"),
            ("input-file.md", "input-file.html", "Input File Guide"),
            ("mesh-reviewer.md", "mesh-reviewer.html", "Mesh Reviewer"),
            ("restart-io.md", "restart-io.html", "Restart I/O"),
            ("mesh-restart.md", "mesh-restart.html", "Mesh Restart"),
            ("postprocessing.md", "postprocessing.html", "Postprocessing"),
            ("testing.md", "testing.html", "Testing"),
        ],
    ),
    ("Reference", [("reference.md", "reference.html", "Reference")]),
    ("Methodology", [("methodology.md", "methodology.html", "Methodology")]),
    ("Troubleshooting", [("troubleshooting.md", "troubleshooting.html", "Troubleshooting")]),
]

PAGES = [page for _, pages in SECTIONS for page in pages]


def inline_markup(text: str) -> str:
    escaped = html.escape(text)
    escaped = re.sub(r"\*\*([^*]+)\*\*", r"<strong>\1</strong>", escaped)
    escaped = re.sub(r"`([^`]+)`", r"<code>\1</code>", escaped)

    def link_repl(match: re.Match[str]) -> str:
        label = match.group(1)
        href = match.group(2)
        if href.endswith(".md"):
            href = Path(href).with_suffix(".html").name
        return f'<a href="{html.escape(href, quote=True)}">{label}</a>'

    return re.sub(r"\[([^\]]+)\]\(([^)]+)\)", link_repl, escaped)


def is_table_separator(line: str) -> bool:
    stripped = line.strip()
    return bool(re.fullmatch(r"\|?[\s:\-|]+\|?", stripped)) and "---" in stripped


def table_cells(line: str) -> list[str]:
    stripped = line.strip().strip("|")
    return [cell.strip() for cell in stripped.split("|")]


def render_markdown(markdown_text: str) -> str:
    lines = markdown_text.splitlines()
    out: list[str] = []
    paragraph: list[str] = []
    in_code = False
    code_lang = ""
    code_lines: list[str] = []
    list_type: str | None = None
    table_open = False
    i = 0

    def flush_paragraph() -> None:
        nonlocal paragraph
        if paragraph:
            out.append(f"<p>{inline_markup(' '.join(paragraph))}</p>")
            paragraph = []

    def close_list() -> None:
        nonlocal list_type
        if list_type:
            out.append(f"</{list_type}>")
            list_type = None

    def close_table() -> None:
        nonlocal table_open
        if table_open:
            out.append("</tbody></table>")
            table_open = False

    while i < len(lines):
        line = lines[i]

        if in_code:
            if line.startswith("```"):
                out.append(
                    f'<pre><code class="language-{html.escape(code_lang)}">'
                    f"{html.escape(chr(10).join(code_lines))}</code></pre>"
                )
                in_code = False
                code_lang = ""
                code_lines = []
            else:
                code_lines.append(line)
            i += 1
            continue

        if line.startswith("```"):
            flush_paragraph()
            close_list()
            close_table()
            in_code = True
            code_lang = line[3:].strip()
            code_lines = []
            i += 1
            continue

        if not line.strip():
            flush_paragraph()
            close_list()
            close_table()
            i += 1
            continue

        heading = re.match(r"^(#{1,6})\s+(.*)$", line)
        if heading:
            flush_paragraph()
            close_list()
            close_table()
            level = len(heading.group(1))
            out.append(f"<h{level}>{inline_markup(heading.group(2))}</h{level}>")
            i += 1
            continue

        if line.strip().startswith("|") and i + 1 < len(lines) and is_table_separator(lines[i + 1]):
            flush_paragraph()
            close_list()
            close_table()
            headers = table_cells(line)
            out.append("<table><thead><tr>")
            out.extend(f"<th>{inline_markup(cell)}</th>" for cell in headers)
            out.append("</tr></thead><tbody>")
            table_open = True
            i += 2
            continue

        if table_open and line.strip().startswith("|"):
            cells = table_cells(line)
            out.append("<tr>")
            out.extend(f"<td>{inline_markup(cell)}</td>" for cell in cells)
            out.append("</tr>")
            i += 1
            continue

        bullet = re.match(r"^\s*-\s+(.*)$", line)
        numbered = re.match(r"^\s*\d+\.\s+(.*)$", line)
        if bullet or numbered:
            flush_paragraph()
            close_table()
            wanted = "ul" if bullet else "ol"
            if list_type != wanted:
                close_list()
                out.append(f"<{wanted}>")
                list_type = wanted
            text_parts = [(bullet or numbered).group(1)]
            j = i + 1
            while j < len(lines):
                continuation = lines[j]
                if not continuation.strip():
                    break
                if (
                    continuation.startswith("```")
                    or re.match(r"^(#{1,6})\s+", continuation)
                    or re.match(r"^\s*-\s+", continuation)
                    or re.match(r"^\s*\d+\.\s+", continuation)
                    or (
                        continuation.strip().startswith("|")
                        and j + 1 < len(lines)
                        and is_table_separator(lines[j + 1])
                    )
                ):
                    break
                text_parts.append(continuation.strip())
                j += 1
            text = " ".join(text_parts)
            out.append(f"<li>{inline_markup(text)}</li>")
            i = j
            continue

        close_list()
        close_table()
        paragraph.append(line.strip())
        i += 1

    flush_paragraph()
    close_list()
    close_table()
    return "\n".join(out)


def page_template(title: str, body: str) -> str:
    nav_sections = []
    for section, pages in SECTIONS:
        links = " ".join(
            f'<a href="{html.escape(target)}">{html.escape(label)}</a>'
            for _, target, label in pages
        )
        nav_sections.append(
            f'<div class="nav-section"><span>{html.escape(section)}</span>{links}</div>'
        )
    nav = "\n".join(nav_sections)
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{html.escape(title)} - CHAPSim2 Guidance</title>
  <style>
    body {{
      font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      line-height: 1.55;
      margin: 0;
      color: #17202a;
      background: #f7f9fc;
    }}
    main {{
      max-width: 1100px;
      margin: 0 auto;
      padding: 34px 24px 64px;
      background: white;
      min-height: 100vh;
    }}
    .layout {{
      display: grid;
      grid-template-columns: 245px minmax(0, 1fr);
      gap: 32px;
      align-items: start;
    }}
    .brand {{
      display: flex;
      align-items: center;
      gap: 14px;
      margin-bottom: 22px;
    }}
    .brand img {{
      width: 64px;
      height: 64px;
      object-fit: contain;
      flex: 0 0 auto;
    }}
    nav {{
      position: sticky;
      top: 18px;
      border-right: 1px solid #d7dee8;
      padding-right: 18px;
      color: #53657a;
      font-size: 0.92rem;
    }}
    .nav-section {{
      margin-bottom: 18px;
    }}
    .nav-section span {{
      display: block;
      margin-bottom: 6px;
      color: #17202a;
      font-weight: 700;
    }}
    .nav-section a {{
      display: block;
      margin: 4px 0;
    }}
    a {{
      color: #2457c5;
    }}
    h1, h2, h3 {{
      line-height: 1.25;
      margin-top: 1.5em;
    }}
    h1 {{
      margin-top: 0;
      font-size: 2.1rem;
    }}
    code {{
      background: #eef2f7;
      padding: 0.1em 0.3em;
      border-radius: 4px;
    }}
    pre {{
      overflow-x: auto;
      padding: 14px;
      background: #111827;
      color: #f9fafb;
      border-radius: 8px;
    }}
    pre code {{
      background: transparent;
      padding: 0;
      color: inherit;
    }}
    table {{
      width: 100%;
      border-collapse: collapse;
      margin: 18px 0;
      font-size: 0.95rem;
    }}
    th, td {{
      border: 1px solid #d7dee8;
      padding: 8px 10px;
      vertical-align: top;
    }}
    th {{
      background: #eef2f7;
      text-align: left;
    }}
    .doc-home {{
      display: inline-block;
      margin-top: 12px;
      font-weight: 650;
    }}
    @media (max-width: 820px) {{
      .layout {{
        display: block;
      }}
      nav {{
        position: static;
        border-right: 0;
        border-bottom: 1px solid #d7dee8;
        padding: 0 0 18px;
        margin-bottom: 28px;
      }}
      .nav-section a {{
        display: inline-block;
        margin-right: 12px;
      }}
    }}
  </style>
</head>
<body>
  <main>
    <div class="brand">
      <img src="../../chapsim_logo.png" alt="CHAPSim2 logo">
      <strong>CHAPSim2 User Guidance</strong>
    </div>
    <div class="layout">
      <nav>{nav}<a class="doc-home" href="../../index.html">Documentation Home</a></nav>
      <article>
{body}
      </article>
    </div>
  </main>
</body>
</html>
"""


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    for source_name, target_name, label in PAGES:
        source = SOURCE_DIR / source_name
        target = OUTPUT_DIR / target_name
        body = render_markdown(source.read_text())
        target.write_text(page_template(label, body))
        print(f"wrote {target.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
