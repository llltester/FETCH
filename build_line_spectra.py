#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
SBFL Pipeline Orchestrator -- [warning_files_only_warning_lines]run_all_datasets.py
===============================================================================

Processes 7 datasets (PMD, CheckStyle, Codacy, Codeql, ErrorProne,
Sonarqube, Spotbugs) through the full three-step SBFL pipeline.

All file operations are confined within the repository root (REPO_ROOT).

Each dataset gets its own self-contained working tree:

    <REPO_ROOT>/SATs_result/<Name>/
        Data/    (tokens/, n_score/, score/)
        Result/  (<ModelName>/file_result/, line_result/)
        work/    (working tree -- wrapper scripts and symlinks)
            dataset/   -> symlink to datasets/original-datasets/<Name>_Dataset/
            Data/      -> symlink to ../Data/
            Result/     -> symlink to ../Result/
            src/
                extract_new_csv.py  (per-dataset wrapper)
                score.py           (per-dataset wrapper)
                main.py           (per-dataset wrapper)
            tools/   -> symlink
            models/  -> copy (base_model.py rewritten)
            utils/
                __init__.py
                helper.py  (per-dataset, sets root_path)
                config.py  (copied)

Pipeline steps
--------------
Step 1 - extract_new_csv.py
  INPUT  : dataset/File-level/ + Line-level/
  OUTPUT : Data/tokens/<release>_tokens.csv
           Data/n_score/<release>_cf.json  |  <release>_cs.json

Step 2 - score.py
  INPUT  : Data/tokens/ + Data/n_score/ + dataset/File-level/
  OUTPUT : Data/score/<release>_tarantula_normal.json
           Data/score/<release>_ochiai_normal.json
           Data/score/<release>_op2_normal.json
           Data/score/<release>_barinel_normal.json
           Data/score/<release>_dstar_normal.json

Step 3 - main.py
  INPUT  : Data/score/*.json
  OUTPUT : Result/<ModelName>/file_result/<project>/<release>-result.csv
           Result/<ModelName>/line_result/<project>/<release>-result.csv
"""

import subprocess
import os
import stat
import sys

# ==============================================================================
# Global paths — all relative to REPO_ROOT (the directory containing this script)
# ==============================================================================

# REPO_ROOT is the directory that contains this script.
# All paths are derived from it so that the entire project is relocatable.
REPO_ROOT = os.path.dirname(os.path.abspath(__file__))

# Template scripts (read-only, never modified in-place)
BASE_SCRIPTS = os.path.join(REPO_ROOT, "templates", "sound", "src")

# Raw input datasets
ORIGINAL_DS_DIR = os.path.join(REPO_ROOT, "datasets", "original-datasets")

# All outputs go under REPO_ROOT
SATs_RESULT_ROOT = os.path.join(REPO_ROOT, "SATs_result")

# Use the Python interpreter running this script (supports f-strings)
PYTHON = sys.executable

ALL_DATASETS = [
    "PMD",
    "CheckStyle",
    "Codacy",
    "Codeql",
    "ErrorProne",
    "Sonarqube",
    "Spotbugs",
]

# Release list for test subdirectory extraction
TEST_RELEASE_LIST = [
    'activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0',
    'camel-2.10.0', 'camel-2.11.0',
    'derby-10.5.1.1',
    'groovy-1_6_BETA_2',
    'hbase-0.95.2',
    'hive-0.12.0',
    'jruby-1.5.0', 'jruby-1.7.0.preview1',
    'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3',
]


# ==============================================================================
# Helpers
# ==============================================================================

def _write(path, data):
    """Write data (bytes or str) to path.  Creates parent dirs."""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    mode = "wb" if isinstance(data, bytes) else "w"
    with open(path, mode) as fh:
        fh.write(data)
    os.chmod(path, stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP | stat.S_IROTH)


def _symlink(src, dst):
    if os.path.islink(dst):
        os.unlink(dst)
    elif os.path.exists(dst):
        raise FileExistsError(f"Cannot symlink {dst}: already exists")
    os.symlink(src, dst)


def _replace_line(code_bytes, old_line, new_line):
    """Replace the first line in code_bytes that starts with old_line.
    Preserves the original line ending (CRLF or LF).
    Works on the raw bytes so CRLF inside string literals is safe.
    Returns the modified bytes.
    """
    lines = code_bytes.split(b'\n')
    found = False
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped == old_line.encode():
            # Preserve original indentation
            indent = line[:len(line) - len(line.lstrip())]
            lines[i] = indent + new_line.encode()
            found = True
            break
    if not found:
        raise ValueError(f"Could not find line: {old_line!r}")
    return b'\n'.join(lines)


# ==============================================================================
# Per-dataset file generators (all use binary mode to preserve CRLF)
# ==============================================================================

def _make_helper(name, dataset_dir, data_dir, result_dir):
    """Write utils/helper.py with dataset-specific root_path and result_path."""
    orig_ds = os.path.join(ORIGINAL_DS_DIR, f"{name}_Dataset")
    src = os.path.join(BASE_SCRIPTS, "utils", "helper.py")
    with open(src, "rb") as fh:
        code = fh.read()

    old = "root_path = r'/root/workspace/zyf/work4/warning-files-full-lines/PMD_Dataset'"
    new = f"root_path = {repr(orig_ds)}"
    code = _replace_line(code, old, new)

    old = "result_path = f'{root_path}{result_string}'"
    new = f"result_path = {repr(result_dir)}"
    code = _replace_line(code, old, new)

    path = os.path.join(dataset_dir, "src", "utils", "helper.py")
    _write(path, code)
    return path


def _make_config(name, dataset_dir, data_dir=None, result_dir=None):
    """Write utils/config.py (identical to original, no path changes needed)."""
    src = os.path.join(BASE_SCRIPTS, "utils", "config.py")
    with open(src, "rb") as fh:
        code = fh.read()
    path = os.path.join(dataset_dir, "src", "utils", "config.py")
    _write(path, code)
    return path


def _make_extract_wrapper(name, dataset_dir, data_dir, result_dir):
    """Write extract_new_csv.py with dataset-specific paths (binary mode)."""
    orig_ds = os.path.join(ORIGINAL_DS_DIR, f"{name}_Dataset")

    src = os.path.join(BASE_SCRIPTS, "extract_new_csv.py")
    with open(src, "rb") as fh:
        code = fh.read()

    # Replace line-level path (inside get_n_score function body)
    old_file_save = "file_save_path = '/root/workspace/zyf/work4/warning-files-full-lines/PMD_Dataset/File-level'"
    new_file_save = f"file_save_path = {repr(str(orig_ds) + '/File-level')}"
    code = _replace_line(code, old_file_save, new_file_save)

    old_line_save = "line_save_path = '/root/workspace/zyf/work4/warning-files-full-lines/PMD_Dataset/Line-level'"
    new_line_save = f"line_save_path = {repr(str(orig_ds) + '/Line-level')}"
    code = _replace_line(code, old_line_save, new_line_save)

    # Replace tokens output path
    old_tok = "tokens_file_name = (\n        '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/tokens/'"
    new_tok = f"tokens_file_name = (\n        {repr(str(data_dir) + '/tokens/')}"
    code = code.replace(old_tok.encode(), new_tok.encode())

    # Replace n_score output paths
    old_cf = "cf_file_name = (\n        '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/'"
    new_cf = f"cf_file_name = (\n        {repr(str(data_dir) + '/n_score/')}"
    code = code.replace(old_cf.encode(), new_cf.encode())

    old_cs = "cs_file_name = (\n        '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/'"
    new_cs = f"cs_file_name = (\n        {repr(str(data_dir) + '/n_score/')}"
    code = code.replace(old_cs.encode(), new_cs.encode())

    # Top-level dataset_string and result_string
    code = _replace_line(code,
        "dataset_string = '/root/workspace/zyf/work4/warning-files-full-lines/PMD_Dataset'",
        f"dataset_string = {repr(str(orig_ds))}"
    )
    code = _replace_line(code,
        "result_string = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Result'",
        f"result_string = {repr(str(result_dir))}"
    )

    path = os.path.join(dataset_dir, "src", "extract_new_csv.py")
    _write(path, code)
    return path


def _make_score_wrapper(name, dataset_dir, data_dir=None, result_dir=None):
    """Write score.py with Data/ pointing to the per-dataset Data dir (binary)."""
    if data_dir is None:
        data_dir = os.path.join(SATs_RESULT_ROOT, name, "Data")
    src = os.path.join(BASE_SCRIPTS, "score.py")
    with open(src, "rb") as fh:
        code = fh.read()

    old = "/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/"
    new = str(data_dir) + "/"
    code = code.replace(old.encode(), new.encode())

    path = os.path.join(dataset_dir, "src", "score.py")
    _write(path, code)
    return path


def _make_models_dir(name, dataset_dir, data_dir):
    """
    Copy BASE_SCRIPTS/models/ into the per-dataset work tree, then overwrite
    base_model.py with per-dataset paths.

    base_model.py contains two types of hard-coded paths that need rewriting:
      1. ORIGINAL_DS_DIR = '/root/.../[Datasets]_warning-files-only-warning-lines/original-datasets'
         (module-level constant, used for loading V_train defect lines)
      2. /root/.../SOUND-PMD-dataset/Data/score/..., /root/.../SOUND-PMD-dataset/Data/graph/...
         (inside __init__, used for loading SBFL score files)
    """
    import shutil

    src_models = os.path.join(BASE_SCRIPTS, "models")
    dst_models = os.path.join(dataset_dir, "src", "models")

    # If dst_models is an existing symlink, drop it so we can copy a real tree.
    if os.path.islink(dst_models) or os.path.isfile(dst_models):
        os.unlink(dst_models)
    if os.path.isdir(dst_models):
        shutil.rmtree(dst_models)

    shutil.copytree(
        src_models,
        dst_models,
        ignore=shutil.ignore_patterns("__pycache__", "*.pyc"),
    )

    # Rewrite base_model.py with per-dataset paths.
    src = os.path.join(BASE_SCRIPTS, "models", "base_model.py")
    with open(src, "rb") as fh:
        code = fh.read()

    # 1) ORIGINAL_DS_DIR constant
    old_orig_ds = b"ORIGINAL_DS_DIR = '/root/workspace/zyf/work4/[Datasets]_warning-files-only-warning-lines/original-datasets'"
    new_orig_ds = f"ORIGINAL_DS_DIR = {repr(ORIGINAL_DS_DIR)}".encode()
    code = code.replace(old_orig_ds, new_orig_ds)

    # 2) All Data/score/ and Data/graph/ paths in __init__
    old_root = b"/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/"
    new_root = str(data_dir).encode() + b"/"
    code = code.replace(old_root, new_root)

    dst = os.path.join(dst_models, "base_model.py")
    _write(dst, code)
    return dst


def _make_main_wrapper(name, dataset_dir, data_dir=None, result_dir=None):
    """Write main.py into the per-dataset work tree.

    main.py does not contain hard-coded data paths (all file references are
    delegated to BaseModel / helper.py), so it is copied verbatim.
    """
    src = os.path.join(BASE_SCRIPTS, "main.py")
    with open(src, "rb") as fh:
        code = fh.read()

    path = os.path.join(dataset_dir, "src", "main.py")
    _write(path, code)
    return path


# ==============================================================================
# Per-dataset environment setup
# ==============================================================================

def prepare_dataset(name):
    """
    Build a self-contained working tree for one dataset.

    Returns (dataset_dir, step1, step2, step3, result_dir)
      step1/2/3 = paths to the per-dataset wrapper scripts.
    """
    dataset_dir = os.path.join(SATs_RESULT_ROOT, name, "work")
    data_dir    = os.path.join(SATs_RESULT_ROOT, name, "Data")
    result_dir  = os.path.join(SATs_RESULT_ROOT, name, "Result")
    orig_ds     = os.path.join(ORIGINAL_DS_DIR, f"{name}_Dataset")

    os.makedirs(dataset_dir, exist_ok=True)

    # Ensure Data/Result subdirs exist
    for sub in ("tokens", "n_score", "score"):
        os.makedirs(os.path.join(data_dir, sub), exist_ok=True)
    os.makedirs(result_dir, exist_ok=True)

    # Symlinks inside the dataset tree so scripts find them via relative paths
    _symlink(orig_ds,    os.path.join(dataset_dir, "dataset"))
    _symlink(data_dir,   os.path.join(dataset_dir, "Data"))
    _symlink(result_dir, os.path.join(dataset_dir, "Result"))

    # Per-dataset utils/ (helper.py needs dataset-specific root_path)
    utils_dir = os.path.join(dataset_dir, "src", "utils")
    os.makedirs(utils_dir, exist_ok=True)
    _write(os.path.join(utils_dir, "__init__.py"), b"# auto-generated\n")

    _make_helper(name, dataset_dir, data_dir, result_dir)
    _make_config(name, dataset_dir, data_dir, result_dir)

    # Symlink tools/ and tools.py (dataset-agnostic).  models/ is copied as a
    # real tree because base_model.py contains hard-coded score/graph paths
    # that need to be rewritten to point at the per-dataset Data dir.
    _symlink(os.path.join(BASE_SCRIPTS, "tools"),
             os.path.join(dataset_dir, "src", "tools"))
    _symlink(os.path.join(BASE_SCRIPTS, "tools.py"),
             os.path.join(dataset_dir, "src", "tools.py"))
    _make_models_dir(name, dataset_dir, data_dir)

    step1 = _make_extract_wrapper(name, dataset_dir, data_dir, result_dir)
    step2 = _make_score_wrapper(name, dataset_dir, data_dir)
    step3 = _make_main_wrapper(name, dataset_dir, data_dir)

    print(f"  [{name}] prepared at {dataset_dir}")
    print(f"         Data   -> {data_dir}")
    print(f"         Result -> {result_dir}")
    return dataset_dir, step1, step2, step3, result_dir


# ==============================================================================
# Pipeline execution
# ==============================================================================

def run_step(step_label, script_path, cwd):
    """Run one script.  Raises on non-zero exit."""
    sep = "=" * 70
    print(f"\n{sep}")
    print(f"  [{step_label}]")
    print(f"  cwd = {cwd}")
    print(sep)
    result = subprocess.run([PYTHON, script_path], cwd=cwd)
    if result.returncode != 0:
        raise RuntimeError(f"[{step_label}] failed with code {result.returncode}")
    print(f"\n  [{step_label}] done\n")


def copy_test_releases(result_dir):
    """
    在每个模型的 line_result 目录下新建 test 子目录，
    并复制指定的 release 结果文件到 test 目录。
    只复制粘贴，不修改原目录。

    目录结构：
        <result_dir>/
            <ModelName>/
                line_result/
                    <project>/<release>-result.csv
                    test/                        ← test 在 line_result 内部
                        <release>-result.csv
    """
    import shutil

    candidates = [d for d in os.listdir(result_dir)
                  if os.path.isdir(os.path.join(result_dir, d))]
    model_dirs = [d for d in candidates
                  if os.path.isdir(os.path.join(result_dir, d, "line_result"))]

    if not model_dirs:
        print(f"  [SKIP] no model dirs found under {result_dir}")
        return

    total_copied = 0
    for model in model_dirs:
        model_line_result = os.path.join(result_dir, model, "line_result")
        test_dir = os.path.join(model_line_result, "test")

        os.makedirs(test_dir, exist_ok=True)

        model_copied = 0
        try:
            entries = os.listdir(model_line_result)
        except OSError as e:
            print(f"  [WARN] cannot list {model_line_result}: {e}")
            continue

        for project_dir in entries:
            project_path = os.path.join(model_line_result, project_dir)
            if not os.path.isdir(project_path) or project_dir == "test":
                continue

            for release in TEST_RELEASE_LIST:
                src_file = os.path.join(project_path, f"{release}-result.csv")
                if os.path.isfile(src_file):
                    dst_file = os.path.join(test_dir, f"{release}-result.csv")
                    shutil.copy2(src_file, dst_file)
                    model_copied += 1

        expected = {f"{r}-result.csv" for r in TEST_RELEASE_LIST}
        leftovers = [f for f in os.listdir(test_dir) if f not in expected]
        if leftovers:
            print(f"  [{model}] test: NOTE  {len(leftovers)} extra files kept")

        print(f"  [{model}] test: copied {model_copied} files")
        total_copied += model_copied

    print(f"  Total: {total_copied} files copied to test directories")


def copy_test_releases_all():
    """遍历所有数据集 Result 目录，为每个数据集的每个模型生成 test 目录。"""
    for name in ALL_DATASETS:
        result_dir = os.path.join(SATs_RESULT_ROOT, name, "Result")
        if not os.path.isdir(result_dir):
            print(f"\n[SKIP] {name}: {result_dir} not found")
            continue
        print(f"\n[{name}] processing {result_dir}")
        copy_test_releases(result_dir)


# ==============================================================================
# Main
# ==============================================================================

def main():
    pass

    print("=" * 70)
    print("  SBFL Pipeline Orchestrator")
    print(f"  REPO_ROOT: {REPO_ROOT}")
    print("  Datasets: " + ", ".join(ALL_DATASETS))
    print("=" * 70)

    for name in ALL_DATASETS:
        print(f"\n{'#'*70}")
        print(f"##  {name}")
        print(f"{'#'*70}")

        dataset_dir, step1, step2, step3, result_dir = prepare_dataset(name)

        run_step(f"{name} - Step1 - extract_new_csv.py", step1, os.path.join(dataset_dir, "src"))
        run_step(f"{name} - Step2 - score.py",            step2, os.path.join(dataset_dir, "src"))
        run_step(f"{name} - Step3 - main.py",             step3, os.path.join(dataset_dir, "src"))

        # Step3 结束后，复制指定 release 结果到 test 目录
        copy_test_releases(result_dir)
    print("=" * 70)
    print("  All datasets processed successfully.")
    print("=" * 70)

    # 兜底：再扫一遍所有数据集的 Result 目录
    print("\n[Final pass] ensure test/ exists for every dataset Result dir...")
    copy_test_releases_all()


if __name__ == "__main__":
    main()
