# DeepLineDP Reproduction Guide

All file paths below are **relative to `Baseline/DeepLineDP/script/`** unless noted otherwise.

## Quick start

> Requires: `../../../datasets/line-level-defect-prediction/Dataset/` (raw CSVs)

```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/DeepLineDP/script

# 1) Preprocess raw CSVs (required by every later step)
python preprocess_data.py
> Output directory: `../datasets/preprocessed_data/`

# 2) Train Word2Vec embeddings (one per project)
python train_word2vec.py <proj>     # e.g. activemq
> Output directory: `../output/Word2Vec_model/<proj>-50dim.bin`

# 3) Train the DeepLineDP model (one checkpoint per project, 10 epochs)
python train_model.py -dataset <proj>
> Output directories: `../output/model/DeepLineDP/<proj>/`, `../output/loss/DeepLineDP/`

# 4) Run within-release prediction
python generate_prediction.py -dataset <proj> -target_epochs 7
> Output directory: `../output/prediction/DeepLineDP/within-release/`
```

## Reproduce results without retraining

Skip Steps 2–3 and reuse the shipped `Word2Vec_model/` + `model/DeepLineDP/` resources:

```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/DeepLineDP/script

# 1) Preprocess
python preprocess_data.py

# 4) Generate predictions using the shipped w2v + checkpoints (all 9 projects)
bash generate_prediction.sh
> Output directory: `../output/prediction/DeepLineDP/within-release/`
```

Reused resources:

- Word2Vec: `output/Word2Vec_model/<proj>-50dim.bin` (9 projects, already in the repo).
- Checkpoints: `output/model/DeepLineDP/<proj>/checkpoint_<1-10>epochs.pth` (9 projects × 10 epochs, already in the repo).
