# LineDef Reproduction Guide

All file paths below are **relative to `Baseline/LineDef/script/`** unless noted otherwise.

## Quick start

> Requires: `../../../datasets/line-level-defect-prediction/Dataset/` (raw CSVs)

```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/LineDef/script

# 1) Preprocess raw CSVs (required by every later step)
python preprocess_data.py
> Output directory: `./preprocessed_data/`

# 2) Train Word2Vec embeddings (one per project)
bash run_train_word2vec.sh
> Output directory: `../output/Word2Vec_model/<proj>-50dim.bin`

# 3) Train the LineDef model (one checkpoint per project, 20 epochs)
bash run_training_bash.sh
> Output directories: `../output/model/LineDef/<proj>/`, `../output/loss/LineDef/`

# 4) Run within-release prediction
bash run_testing_bash.sh
> Output directory: `../output/prediction/LineDef/within-release/`
```

## Reproduce results without retraining

Skip Steps 2–3 and reuse the shipped `Word2Vec_model/` + `model/LineDef/` resources:

```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/LineDef/script

# 1) Preprocess
python preprocess_data.py

# 4) Generate predictions using the shipped w2v + checkpoints
python generate_prediction.py -dataset <proj> -target_epochs 20
> Output directory: `../output/prediction/LineDef/within-release/`
```

Reused resources:

- Word2Vec: `output/Word2Vec_model/<proj>-50dim.bin` (9 projects, already in the repo).
- Checkpoints: `output/model/LineDef/<proj>/checkpoint_<1-20>epochs.pth` (9 projects × 20 epochs, already in the repo).
