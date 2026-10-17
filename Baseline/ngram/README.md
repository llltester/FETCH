# N-gram Baseline

## Quick start

> Requires: Java 11+, and `Baseline/DeepLineDP/datasets/original/` 


```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/ngram

# 1) Tokenize & label each line of every release
python preprocess_data.py
> Output directory: `./preprocessed_data/`

# 2) Convert per-release CSVs into flat text files for SLP
python export_ngram_data.py
> Output directory: `./n_gram_data/`

# 3) Train one n-gram model per project (on its training release) and
#    predict line-level scores on every eval release
javac -cp ".lib/*" n_gram.java
java  -cp ".lib/*" n_gram

> Output directory: `./n_gram_result/<release>-line-lvl-result.txt`
```

`n_gram_result/<release>-line-lvl-result.txt` is a TSV with columns
`train-release  test-release  file-name  line-number  token  token-score  line-score`.
