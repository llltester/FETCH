# LineDP Reproduction Guide

All file paths below are **relative to `Baseline/GLANCE_and_LineDP/`** unless noted otherwise.

## Quick start

> Requires: `../../datasets/line-level-defect-prediction/Dataset/` (raw CSVs)

```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/GLANCE_and_LineDP

# Run all 3 models (Glance-LR / Glance-MD / LineDP) × 14 releases
python main.py
> Output directories: `result/Glance-LR/`, `result/GLANCE-MD/`, `result/MIT-LineDP/`

# Or run a single model
python main.py MIT-LineDP
> Output directory: `result/MIT-LineDP/`
```

Models are picked from `MODEL_DICT` in `main.py`:
`Glance-LR`, `GLANCE-MD`, `MIT-LineDP`.
