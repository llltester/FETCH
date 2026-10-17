# LLM4SA Reproduction Guide

All file paths below are **relative to `Baseline/LLM4SA/`** unless noted otherwise.

## Option A: full pipeline (run main.py)

```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/LLM4SA

python src/bug-inspector/main.py
```

> Requires: API keys configured in `conf/jsoninfo.py` + LLM4SA source tree built (ctags / cscope).

## Option B: reuse pre-computed results (skip rerunning)

The 7 tool results are already shipped as `.7z` archives under `test/`. Extract each
into `test/result/`, so the RQ scripts can read them directly:

```bash
cd /root/workspace/zyf/work4/FETCH/Baseline/LLM4SA

mkdir -p test/result
for tool in Checkstyle Codacy Codeql Errorprone PMD Sonarqube Spotbugs; do
    7z x test/${tool}.7z -otest/result/
done
```

After extraction, `test/result/` contains one subdirectory per tool, e.g.
`test/result/Checkstyle/`, `test/result/PMD/`, `test/result/Spotbugs/`, …
