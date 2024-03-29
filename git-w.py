#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
git-w.py

A tool to normalize line endings to reduce diffs.
It may normalize line endings to either "\n" (unix) or "\r\n" (windows)
depending on which one best reduces the diff size.
"""

import subprocess


def command_output(cmd):
    return subprocess.Popen(cmd.split(), stdout=subprocess.PIPE).stdout.read().decode("utf-8")


DIFF_STAT_CMD = "git diff HEAD --numstat"


def parse_diffs_line(line):
    (added, removed, filename) = line.split(None, 2)
    return filename.strip(), int(added) + int(removed)


def diffs(options=""):
    for line in command_output(DIFF_STAT_CMD + (" " if options else "") + options).splitlines():
        yield parse_diffs_line(line)


diffs_w = dict(diffs("-w"))

for (filename, stats) in diffs():
    if stats <= diffs_w.get(filename, 0):
        continue

    def check_success(label):
        new_stats_txt = command_output(f"{DIFF_STAT_CMD} {filename}")
        new_stats = parse_diffs_line(new_stats_txt)[
            1] if new_stats_txt.strip() else 0
        if new_stats >= stats:
            return False
        print(
            f"{filename}: Changed to {label} line endings, reducing diff by {stats-new_stats} lines.")
        return True

    with open(filename, "rb") as file:
        content = file.read()

    def write(new_content):
        with open(filename, "wb") as file:
            file.write(new_content)

    unix_style = content.replace(b"\r", b"")
    write(unix_style)
    if check_success("unix"):
        continue
    windows_style = unix_style.replace(b"\n", b"\r\n")
    write(windows_style)
    if check_success("windows"):
        continue
    write(content)
    print(f"{filename}: No simple fix")
