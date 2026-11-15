#!/usr/bin/env python3
"""Create a CHAPSim run directory from a template case with section-aware key overrides.

    mkcase.py TEMPLATE_DIR RUN_DIR [section.key=value ...] [--copy-data PATTERN_DIR:GLOB ...]

Copies TEMPLATE_DIR/input_chapsim.ini (edited), the property table(s) and TEMPLATE_DIR/1_data/*.
A key that is absent from its section is appended at the end of that section.
"""
import glob, os, re, shutil, sys


def edit_ini(text, overrides):
    lines = text.splitlines()
    out, section, seen = [], None, set()
    sec_end = {}
    for line in lines:
        m = re.match(r'^\s*\[(\w+)\]', line)
        if m:
            section = m.group(1)
            out.append(line)
            sec_end[section] = len(out)
            continue
        km = re.match(r'^(\s*)([A-Za-z_][\w]*)(\s*=\s*)(.*)$', line)
        if km and section is not None and (section, km.group(2)) in overrides:
            key = (section, km.group(2))
            out.append(f"{km.group(1)}{km.group(2)}{km.group(3)}{overrides[key]}")
            seen.add(key)
        else:
            out.append(line)
        if section is not None:
            sec_end[section] = len(out)
    missing = [k for k in overrides if k not in seen]
    # insert missing keys at the end of their sections (from the bottom up)
    for sec, key in sorted(missing, key=lambda k: -sec_end.get(k[0], len(out))):
        if sec not in sec_end:
            sys.exit(f"section [{sec}] not in template")
        out.insert(sec_end[sec], f"{key}= {overrides[(sec, key)]}")
        for s in sec_end:
            if sec_end[s] >= sec_end[sec] and s != sec:
                sec_end[s] += 1
        sec_end[sec] += 1
    return "\n".join(out) + "\n"


def main():
    tmpl, run = sys.argv[1], sys.argv[2]
    overrides, copies, inserts = {}, [], []
    args = sys.argv[3:]
    i = 0
    while i < len(args):
        a = args[i]
        if a == '--copy-data':
            copies.append(args[i + 1]); i += 2; continue
        k, v = a.split('=', 1)
        sec, key = k.split('.', 1)
        if '@' in key:  # section.key@after=value : insert the new line after key 'after'
            key, after = key.split('@', 1)
            inserts.append((sec, key, after, v))
        else:
            overrides[(sec, key)] = v
        i += 1
    if os.path.exists(run):
        sys.exit(f"{run} exists")
    os.makedirs(os.path.join(run, '1_data'))
    with open(os.path.join(tmpl, 'input_chapsim.ini')) as f:
        text = f.read()
    text = edit_ini(text, overrides)
    for sec, key, after, v in inserts:
        lines, section, done = text.splitlines(), None, False
        for n, line in enumerate(lines):
            m = re.match(r'^\s*\[(\w+)\]', line)
            if m:
                section = m.group(1)
            elif section == sec and re.match(rf'^\s*{after}\s*=', line):
                lines.insert(n + 1, f"{key}= {v}"); done = True; break
        if not done:
            sys.exit(f"{sec}.{after} not found for insertion of {key}")
        text = "\n".join(lines) + "\n"
    with open(os.path.join(run, 'input_chapsim.ini'), 'w') as f:
        f.write(text)
    for t in glob.glob(os.path.join(tmpl, '*.DAT')):
        shutil.copy(t, run)
    for t in glob.glob(os.path.join(tmpl, '1_data', '*')):
        shutil.copy(t, os.path.join(run, '1_data'))
    for c in copies:
        src, pat = c.split(':', 1)
        n = 0
        for t in glob.glob(os.path.join(src, '1_data', pat)):
            shutil.copy(t, os.path.join(run, '1_data')); n += 1
        if n == 0:
            sys.exit(f"no files {pat} in {src}/1_data")


if __name__ == '__main__':
    main()
