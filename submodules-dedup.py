"""
Set up submodules without duplication.

When using `git submodule --init [--recursive]` for several projects,
common submodules would store duplicate information.

When usubmodules-dedup.py instead, a common store for the submodules under ~/submodules is used.
"""

import argparse
from contextlib import contextmanager
import os
from pathlib import Path
import subprocess


def command_output(cmd):
    return subprocess.Popen(cmd.split(), stdout=subprocess.PIPE).stdout.read().decode("utf-8")


def strip_prefix(text, prefix):
    if not text.startswith(prefix):
        return text
    return text[len(prefix):]


def strip_suffix(text, suffix):
    # From http://stackoverflow.com/a/1038999/40916
    if not text.endswith(suffix):
        return text
    return text[:len(text)-len(suffix)]


def list_submodules():
    for line in command_output("git config --file .gitmodules --get-regexp submodule\..*\.url").splitlines():
        [key, value] = line.split(" ", 1)
        name = key.split(".", 1)[1].rsplit(".", 1)[0]
        yield {
            "local": command_output(f"git config --file .gitmodules submodule.{name}.path").strip(),
            "remote": strip_suffix(value, ".git"),
        }


@contextmanager
def cwd(path):
    "Scoped chdir (from https://stackoverflow.com/a/37996581/40916)"
    oldpwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(oldpwd)


base_path = Path.home().joinpath("submodules")


def init_submodules(recursive, level=0):
    indent = "    "*level
    for submodule in list_submodules():
        print(f"""{indent}{submodule["local"]}""")
        shared_path = base_path.joinpath(
            strip_prefix(submodule["remote"], "https://"))
        if command_output(f"""git submodule status {submodule["local"]}""").startswith("-"):
            shared_path.parent.mkdir(parents=True, exist_ok=True)
            if not shared_path.exists():
                command_output(
                    f"""git clone {submodule["remote"]} {shared_path}""")
            print(f"""{indent}  => {shared_path}""")
            command_output(
                f"""git submodule update --reference {shared_path} --init {submodule["local"]}""")
        if recursive:
            with cwd(submodule["local"]):
                init_submodules(recursive, level+1)


parser = argparse.ArgumentParser()


parser.add_argument(
    "--recursive",
    help="Initialize nested submodules",
    default=False,
    action="store_true",
)


args = parser.parse_args()


init_submodules(args.recursive)