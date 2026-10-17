# FETCH — Reproduction Guide

All paths in this repo are relative to **`FETCH/`** unless noted otherwise.

---

## 1. SAT initial scan results

7 SATs (PMD / CheckStyle / ErrorProne / Spotbugs / Codacy / Codeql / Sonarqube):

```
SATs_initial_scan_result/
├── PMD/         <release>-line-lvl-result.txt
├── CheckStyle/  <release>-result.csv
├── Errorprone/  <release>-line-lvl-result.txt
├── Spotbugs/    <release>-result.csv
├── codacy/      <release>-result.csv
├── codeql/      <release>-result.csv
└── sonarqube/   <release>-result.csv
```

## 2. Datasets

```
datasets/
├── line-level-defect-prediction/
│   └── Dataset/Line-level/
├── original-datasets/
│   └── <SAT>_Dataset/
│       └── File-level/
└── map-warning-lines-only/
    └── <SAT>_Dataset/LineMapping/
```

| Subdirectory | Purpose |
|---|---|
| `datasets/line-level-defect-prediction/` | Ground-truth defect-line labels per release; used by baselines for training/evaluation. |
| `datasets/original-datasets/<SAT>_Dataset/` | Source-code context for every warning line per release; used by SBFL spectrum computation. |
| `datasets/map-warning-lines-only/<SAT>_Dataset/LineMapping/` | `(compressed_line_number → original_line_number)` mapping; used by the RQ scripts to recover the original line numbers. |

---

## 3. FETCH main pipeline

Run the three steps in order to produce all FETCH intermediates.

### Step 3.1 — Compute spectrum scores for SAT warning lines

```bash
cd FETCH
python main.py
```

**Output:** `SATs_result/<SAT>/Result/<spectrum>/line_result/test/<release>-result.csv`,
with columns `(filename, line, spectrum.score, max_jaccard)` for every warning line.

### Step 3.2 — Compute GLANCE's NT / NFC

```bash
python Baseline/GLANCE_and_LineDP/main.py
```

**Output:** `Baseline/GLANCE_and_LineDP/result/Glance_MD_full_threshold/line_result/test/<release>-result.csv`,
with columns `(filename, line, GLANCEscore, NFC, CE, NT)` for every warning line.

### Step 3.3 — Compute adaptive per-release λ

```r
Rscript Auto_lambda_Per_release.R
```

**Output:** `Auto_lambda_Per_release_Results.txt`, recording the per-release λ for every
`(SAT, Spectrum)` combination.

---

## 4. Baselines

Each baseline ships its own README. Follow its instructions:

| Baseline | Entry point |
|---|---|
| GLANCE / LineDP | `Baseline/GLANCE_and_LineDP/README.md` |
| BAFLineDP | `Baseline/BAFLineDP/README.md` |
| DeepLineDP | `Baseline/DeepLineDP/README.md` |
| LineDef | `Baseline/LineDef/README.md` |
| ngram | `Baseline/ngram/README.md` |
| LLM4SA | `Baseline/LLM4SA/README.md` |

---

## 5. RQ1 / RQ2 / RQ3 and Dis

- **`RQ1/`** — `./RQ1/RQ1_Effectiveness.R`
- **`RQ2/`** — `./RQ2/RQ2_ablation.R`
- **`RQ3/`** — `./RQ3/RQ3_Sensitivity_analysis_threshold.R`, `./RQ3/RQ3_Sensitivity_analysis_spectrum.R`
- **`Dis/`** — three discussion analyses (Tarantula-based):
  - `Analysis_step1_Tarantula.R` → `Q1_Q4_Risk_stratum_FDR.csv` (Table 7)
  - `Analysis_step2_Tarantula.R` → `EPxHDE_FDR.csv` (Table 8 Panel A)
  - `Analysis_step3_Tarantula.R` → `Region_Movements_FDR.csv` (Table 8 Panel B)
