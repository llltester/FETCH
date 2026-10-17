#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Single entry point for the SBFL pipeline.

Usage:
    python main.py                 # run all datasets (PMD, CheckStyle, Codacy, ...)
    python main.py PMD             # run a single dataset
"""
import os
import sys

REPO_ROOT = os.path.dirname(os.path.abspath(__file__))
SCRIPT = os.path.join(REPO_ROOT, "build_line_spectra.py")

# Dynamically import and run the orchestrator's main function.
# We do it this way so that all path computations use REPO_ROOT as the anchor.
import importlib.util
spec = importlib.util.spec_from_file_location("orchestrator", SCRIPT)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

if __name__ == "__main__":
    # If a dataset name is passed, restrict to that one dataset.
    if len(sys.argv) > 1:
        dataset = sys.argv[1]
        orig = mod.ALL_DATASETS[:]
        mod.ALL_DATASETS = [dataset]
        print(f"[main.py] Running single dataset: {dataset}")
        try:
            mod.main()
        finally:
            mod.ALL_DATASETS = orig
    else:
        mod.main()
