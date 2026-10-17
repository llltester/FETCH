# BAFLineDP Reproduction Guide

All file paths below are **relative to `Baseline/BAFLineDP/`** unless noted otherwise.

## Quick start

> Requires: `../../datasets/line-level-defect-prediction/Dataset/` (raw CSVs)

```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/BAFLineDP

# 1) Preprocess raw CSVs (required by every later step)
python code_preprocessing.py
> Output directory: `datasets/preprocessed_data/`

# 2) Train the BAFLineDP model
python train_model.py
> Output directories: `output/model/BAFLineDP/`, `output/loss/BAFLineDP/`

# 3) Run within-release prediction
python generate_within_prediction.py
> Output directory: `output/prediction/BAFLineDP/within-release/`
```

## Reproduce results without retraining

If the goal is to reproduce the paper's table numbers (not to retrain from scratch),
**skip Step 2** and reuse the checkpoints already shipped in the repo:

```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/BAFLineDP

# 1) Preprocess
python code_preprocessing.py

# 3) Generate predictions using the shipped checkpoints
python generate_within_prediction.py
```

Reused resources:

- Checkpoints: `output/model/BAFLineDP/<proj>/best_model.pth` (9 projects, one
  per project directory, already in the repo).
