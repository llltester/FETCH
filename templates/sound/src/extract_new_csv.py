from tools import read_java_file_without_comments
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer
import re
import json
import warnings
import time
from tqdm import tqdm
import numpy as np
warnings.simplefilter(action='ignore', category=pd.errors.PerformanceWarning)

dataset_string = '/root/workspace/zyf/work4/warning-files-full-lines/PMD_Dataset'
result_string = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Result'

file_level_path = f'{dataset_string}/File-level/'
line_level_path = f'{dataset_string}/Line-level/'
result_path = f'{result_string}'

file_level_path_suffix = '_ground-truth-files_dataset.csv'
line_level_path_suffix = '_defective_lines_dataset.csv'

releases = [

    'activemq-5.0.0', 'activemq-5.1.0', 'activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0',
    'camel-1.4.0', 'camel-2.9.0', 'camel-2.10.0', 'camel-2.11.0', 
    'derby-10.2.1.6', 'derby-10.3.1.4', 'derby-10.5.1.1',
    'groovy-1_5_7', 'groovy-1_6_BETA_1', 'groovy-1_6_BETA_2', 
    'hbase-0.94.0', 'hbase-0.95.0', 'hbase-0.95.2',
    'hive-0.9.0', 'hive-0.10.0', 'hive-0.12.0', 
    'jruby-1.1', 'jruby-1.4.0', 'jruby-1.5.0', 'jruby-1.7.0.preview1',
    'lucene-2.3.0', 'lucene-2.9.0', 'lucene-3.0.0', 'lucene-3.1',
    'wicket-1.3.0-incubating-beta-1', 'wicket-1.3.0-beta2', 'wicket-1.5.3'

]


# ----------------------------------------------------------------------------
# Optimization notes (vs. the original script)
# ----------------------------------------------------------------------------
# 1. extract_tokens_from:
#    * Removed the tmp.java write/read round-trip and the read_java_file_without_comments
#      disk IO. We inline the same comment-stripping regex on the joined text in memory.
#    * Replaced the per-file CSV read/concat/write cycle with in-memory accumulation:
#      keep each file's CountVectorizer output as a sparse matrix + vocab list, then
#      compute the global vocab union once and stream-write the final CSV.
#    * Output a single CSV directly (the baseline split into part1/part2 then merged,
#      which doubled the concat work).
#
# 2. get_n_score:
#    * Skip the tmp_ratio.java write/read round-trip — read directly from the already
#      loaded `lines` slice.
#    * Inverted the token-membership inner loop. Baseline code did
#          for token in tokens:
#              if token in line_tokens:
#                  n_cf[token] += 1
#      where `tokens` has tens of thousands of entries. We now compute
#          line_token_set = set(_tokenizer(line))
#          for token in (line_token_set & tokens):
#      which scales with the (small) line size, not the vocab size.
#    * Cached the CountVectorizer/tokenizer and the alphabet regex once per call,
#      instead of reconstructing them for every line.
#    * Pre-initialize n_cf/n_cs from the global_vocab so all tokens appear in output
#      JSONs (the reference output also includes the literal 'Bug' key, so we add it).
# ----------------------------------------------------------------------------

_COMMENT_RE = re.compile(r'//.*?\n|/\*.*?\*/', flags=re.DOTALL)

_SKIP_LOG = None


def _log_skip(release, step, file_name, java_text, raw_code):
    pass


def _strip_comments(text: str) -> str:
    """Same semantics as tools.read_java_file_without_comments but on a string."""
    return _COMMENT_RE.sub('', text)


def extract_tokens_from(release):
    tokens_file_name = (
        '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/tokens/'
        + release + '_tokens.csv'
    )

    path = f'{file_level_path}{release}{file_level_path_suffix}'
    with open(path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()

    src_file_indices = [
        lines.index(line) for line in lines
        if r'.java,true,"' in line or r'.java,false,"' in line
    ]
    src_files = [lines[index].split(',')[0] for index in src_file_indices]
    string_labels = [lines[index].split(',')[1] for index in src_file_indices]
    numeric_labels = [1 if label == 'true' else 0 for label in string_labels]
    assert (len(numeric_labels) == len(src_file_indices))

    # Per-file CountVectorizer outputs kept as (sparse, vocab) pairs in memory.
    rows_X = []
    rows_vocab = []
    rows_label = []

    for i in tqdm(range(len(src_file_indices)),
                  desc=f'extract_tokens_from({release})',
                  unit='file'):
        s_index = src_file_indices[i]
        e_index = src_file_indices[i + 1] if i + 1 < len(src_file_indices) else len(lines)
        code_lines = [line.strip() for line in lines[s_index:e_index]]
        code_lines[0] = code_lines[0].split(',')[-1][1:]
        code_lines[-1] = code_lines[-1][:-1]
        if len(code_lines) == 0:
            continue
        raw_code = ''.join(code_lines)
        java_text = _strip_comments(raw_code)
        if java_text == '':
            continue
        try:
            vectorizer = CountVectorizer(lowercase=False, min_df=1)
            X = vectorizer.fit_transform([java_text])
        except ValueError:
            _log_skip(release, 'extract_tokens_from', src_files[i], java_text, raw_code)
            continue
        if X.shape[1] == 0:
            # All tokens were filtered (e.g. only punctuation / numbers remain)
            continue
        rows_X.append(X.tocsr())
        rows_vocab.append(vectorizer.get_feature_names_out())
        rows_label.append(numeric_labels[i])

    # Global vocab (union, preserves first-seen order).
    seen = set()
    global_vocab = []
    for v in rows_vocab:
        for tok in v:
            if tok not in seen:
                seen.add(tok)
                global_vocab.append(tok)
    token_to_idx = {t: i for i, t in enumerate(global_vocab)}
    n_cols = len(global_vocab)

    # Stream the CSV: header + one line per file, with all global columns.
    # Avoids the giant dense pd.concat that dominated baseline runtime.
    with open(tokens_file_name, 'w', encoding='utf-8', newline='') as f:
        f.write(','.join(global_vocab) + ',Bug_labels\n')
        for X, vocab_local, label in zip(rows_X, rows_vocab, rows_label):
            row = np.zeros(n_cols, dtype=np.int64)
            coo = X.tocoo()
            for c, v in zip(coo.col, coo.data):
                row[token_to_idx[vocab_local[c]]] = int(v)
            f.write(','.join(str(x) for x in row) + f',{label}\n')

    return global_vocab


def get_tokens_split(tokens_file_name) -> set:
    tokens_csv = pd.read_csv(tokens_file_name, encoding='ISO-8859-1')
    tokens_set = set(tokens_csv.columns)
    return tokens_set


def get_n_score(release, global_vocab):
    file_save_path = '/root/workspace/zyf/work4/warning-files-full-lines/PMD_Dataset/File-level'
    line_save_path = '/root/workspace/zyf/work4/warning-files-full-lines/PMD_Dataset/Line-level'
    file_level_name = release + '_ground-truth-files_dataset.csv'
    line_level_name = release + '_defective_lines_dataset.csv'
    tokens_file_name = (
        '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/tokens/'
        + release + '_tokens.csv'
    )
    cf_file_name = (
        '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/'
        + release + '_cf.json'
    )
    cs_file_name = (
        '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/'
        + release + '_cs.json'
    )

    n_cf = {t: 0 for t in global_vocab}
    n_cs = {t: 0 for t in global_vocab}
    # The reference output JSONs also carry a 'Bug' key (sourced from the tokens
    # CSV header). Keep it for byte-for-byte compatibility.
    n_cf['Bug_labels'] = 0
    n_cs['Bug_labels'] = 0
    tokens = set(global_vocab)

    path = file_save_path + '/' + file_level_name
    with open(path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()
        src_file_indices = [
            lines.index(line) for line in lines
            if r'.java,true,"' in line or r'.java,false,"' in line
        ]
        src_files = [lines[index].split(',')[0] for index in src_file_indices]
        string_labels = [lines[index].split(',')[1] for index in src_file_indices]
        numeric_labels = [1 if label == 'true' else 0 for label in string_labels]
        assert (len(numeric_labels) == len(src_file_indices))

    # Cache the tokenizer and the alphabet-regex once (avoid per-line construction).
    _vec = CountVectorizer(lowercase=False, min_df=2)
    _tokenizer = _vec.build_tokenizer()
    _alphabet_re = re.compile(r'\b[a-zA-Z]{2,}\b')

    for i in tqdm(range(len(src_file_indices)),
                  desc=f'get_n_score({release})',
                  unit='file'):
        file_name = src_files[i]
        s_index = src_file_indices[i]
        e_index = src_file_indices[i + 1] if i + 1 < len(src_file_indices) else len(lines)
        file_lines = [line.strip() for line in lines[s_index:e_index]]
        file_lines[0] = file_lines[0].split(',')[-1][1:]
        file_lines[-1] = file_lines[-1][:-1]
        # Read source lines directly from memory (skip tmp_ratio.java IO).
        file_lines = [ln + '\n' for ln in file_lines]

        path = line_save_path + '/' + line_level_name
        with open(path, 'r', encoding='utf-8', errors='ignore') as file:
            line_level_lines = file.readlines()
            file_buggy_lines = []
            for line in line_level_lines[1:]:
                temp = line.split(',', 2)
                bug_line_file_name, bug_line = temp[0], temp[2]
                if bug_line_file_name == file_name:
                    file_buggy_lines.append(bug_line)

        for line in file_lines:
            stripped = line.strip()
            s_quoted = '"' + stripped.replace('"', '""') + '"\n'

            if line in file_buggy_lines:
                if _alphabet_re.search(stripped):
                    for token in set(_tokenizer(stripped)):
                        if token in tokens:
                            n_cf[token] += 1
                file_buggy_lines.remove(line)
            elif stripped in file_buggy_lines:
                if _alphabet_re.search(stripped):
                    for token in set(_tokenizer(stripped)):
                        if token in tokens:
                            n_cf[token] += 1
                file_buggy_lines.remove(stripped)
            elif s_quoted in file_buggy_lines:
                if _alphabet_re.search(stripped):
                    for token in set(_tokenizer(s_quoted)):
                        if token in tokens:
                            n_cf[token] += 1
                file_buggy_lines.remove(s_quoted)
            else:
                if _alphabet_re.search(stripped):
                    for token in set(_tokenizer(stripped)):
                        if token in tokens:
                            n_cs[token] += 1

    with open(cf_file_name, 'w') as f:
        json.dump(n_cf, f)
    with open(cs_file_name, 'w') as f:
        json.dump(n_cs, f)


for release in releases:
    t0 = time.perf_counter()
    global_vocab = extract_tokens_from(release)
    t1 = time.perf_counter()
    get_n_score(release, global_vocab)
    t2 = time.perf_counter()
    print(f'[{release}] extract={t1 - t0:.2f}s n_score={t2 - t1:.2f}s total={t2 - t0:.2f}s')