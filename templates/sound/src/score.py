import json
import pandas as pd
from operator import itemgetter
from utils.helper import read_line_level_dataset, read_file_level_dataset
import math

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


def get_tokens_split(tokens_file_name) -> set:
    tokens_csv = pd.read_csv(tokens_file_name, encoding='utf-8')
    raw_set = set(tokens_csv.columns)

    def _fix(name: str) -> str:
        if not any(ord(c) > 127 for c in name):
            return name
        try:
            return name.encode('latin-1').decode('utf-8')
        except (UnicodeEncodeError, UnicodeDecodeError, ValueError):
            return name

    fixed = {_fix(c) for c in raw_set}
    return fixed


def tarantula(n_cf, n_cs, num_of_bug_lines, num_of_lines):
    if n_cf/num_of_bug_lines+n_cs/(num_of_lines-num_of_bug_lines) == 0:
        return 0
    return (n_cf/num_of_bug_lines)/(n_cf/num_of_bug_lines+n_cs/(num_of_lines-num_of_bug_lines))


def ochiai(n_cf, n_cs, num_of_bug_lines):
    if num_of_bug_lines*(n_cf+n_cs) == 0:
        return 0
    return n_cf/math.sqrt(num_of_bug_lines*(n_cf+n_cs))


def op2(n_cf, n_cs, num_of_bug_lines, num_of_lines):
    if num_of_lines - num_of_bug_lines + 1 == 0:
        return 0
    return n_cf - n_cs/(num_of_lines - num_of_bug_lines + 1)


def barinel(n_cf, n_cs):
    if (n_cs+n_cf) == 0:
        return 0
    return 1-n_cs/(n_cs+n_cf)


def dstar(n_cf, n_cs, num_of_bug_lines):
    if n_cs+num_of_bug_lines-n_cf == 0:
        return 0
    return (n_cf*n_cf)/(n_cs+num_of_bug_lines-n_cf)


def get_oracle_lines(release):
    oracle_line_dict, oracle_line_list = read_line_level_dataset(release), set()
    for file_name in oracle_line_dict:
        oracle_line_list.update([f'{file_name}:{line}' for line in oracle_line_dict[file_name]])
    return oracle_line_dict, oracle_line_list


def normalize_score(score_dict: dict):
    max_value = max(score_dict.values())
    min_value = min(score_dict.values())
    if max_value == min_value:
        return {k: 0.0 for k in score_dict}
    normalized_dict = {k: (v - min_value) / (max_value - min_value) for k, v in score_dict.items()}
    return normalized_dict


def evaluate_tarantula_score(release:str):
    n_cf_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cf.json'
    n_cs_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cs.json'

    tokens_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/tokens/' + release + '_tokens.csv'

    tarantula_score_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + release + '_tarantula_normal.json'
    with open(n_cf_file_name, 'r') as f:
        n_cf = json.load(f)
    with open(n_cs_file_name, 'r') as f:
        n_cs = json.load(f)

    tokens = get_tokens_split(tokens_file_name)

    test_text, test_text_lines, test_labels, test_filename, _ = read_file_level_dataset(release)
    num_of_lines = sum([len(lines) for lines in test_text_lines])

    oracle_line_dict, oracle_line_set = get_oracle_lines(release)
    num_of_bug_lines = len(oracle_line_set)

    score = {}
    for token in tokens:
        score[token] = tarantula(n_cf.get(token, 0), n_cs.get(token, 0), num_of_bug_lines, num_of_lines)
    normal_score = normalize_score(score)
    sorted_dict = dict(sorted(normal_score.items(), key=itemgetter(1), reverse=True))
    with open(tarantula_score_file_name, 'w') as f:
        json.dump(sorted_dict, f)


def evaluate_ochiai_score(release:str):
    n_cf_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cf.json'
    n_cs_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cs.json'

    tokens_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/tokens/' + release + '_tokens.csv'

    ochiai_score_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + release + '_ochiai_normal.json'
    with open(n_cf_file_name, 'r') as f:
        n_cf = json.load(f)
    with open(n_cs_file_name, 'r') as f:
        n_cs = json.load(f)

    tokens = get_tokens_split(tokens_file_name)

    oracle_line_dict, oracle_line_set = get_oracle_lines(release)
    num_of_bug_lines = len(oracle_line_set)

    score = {}
    for token in tokens:
        score[token] = ochiai(n_cf.get(token, 0), n_cs.get(token, 0), num_of_bug_lines)
    normal_score = normalize_score(score)
    sorted_dict = dict(sorted(normal_score.items(), key=itemgetter(1), reverse=True))
    with open(ochiai_score_file_name, 'w') as f:
        json.dump(sorted_dict, f)


def evaluate_op2_score(release: str):
    n_cf_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cf.json'
    n_cs_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cs.json'

    tokens_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/tokens/' + release + '_tokens.csv'

    op2_score_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + release + '_op2_normal.json'
    with open(n_cf_file_name, 'r') as f:
        n_cf = json.load(f)
    with open(n_cs_file_name, 'r') as f:
        n_cs = json.load(f)

    tokens = get_tokens_split(tokens_file_name)

    test_text, test_text_lines, test_labels, test_filename, _ = read_file_level_dataset(release)
    num_of_lines = sum([len(lines) for lines in test_text_lines])

    oracle_line_dict, oracle_line_set = get_oracle_lines(release)
    num_of_bug_lines = len(oracle_line_set)

    score = {}
    for token in tokens:
        score[token] = op2(n_cf.get(token, 0), n_cs.get(token, 0), num_of_bug_lines, num_of_lines)
    normal_score = normalize_score(score)
    sorted_dict = dict(sorted(normal_score.items(), key=itemgetter(1), reverse=True))
    with open(op2_score_file_name, 'w') as f:
        json.dump(sorted_dict, f)


def evaluate_barinel_score(release:str):
    n_cf_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cf.json'
    n_cs_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cs.json'

    tokens_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/tokens/' + release + '_tokens.csv'

    barinel_score_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + release + '_barinel_normal.json'
    with open(n_cf_file_name, 'r') as f:
        n_cf = json.load(f)
    with open(n_cs_file_name, 'r') as f:
        n_cs = json.load(f)

    tokens = get_tokens_split(tokens_file_name)

    score = {}
    for token in tokens:
        score[token] = barinel(n_cf.get(token, 0), n_cs.get(token, 0))
    normal_score = normalize_score(score)
    sorted_dict = dict(sorted(normal_score.items(), key=itemgetter(1), reverse=True))
    with open(barinel_score_file_name, 'w') as f:
        json.dump(sorted_dict, f)


def evaluate_dstar_score(release:str):
    n_cf_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cf.json'
    n_cs_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/n_score/' + release + '_cs.json'

    tokens_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/tokens/' + release + '_tokens.csv'

    dstar_score_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + release + '_dstar_normal.json'
    with open(n_cf_file_name, 'r') as f:
        n_cf = json.load(f)
    with open(n_cs_file_name, 'r') as f:
        n_cs = json.load(f)

    tokens = get_tokens_split(tokens_file_name)

    oracle_line_dict, oracle_line_set = get_oracle_lines(release)
    num_of_bug_lines = len(oracle_line_set)

    score = {}
    for token in tokens:
        score[token] = dstar(n_cf.get(token, 0), n_cs.get(token, 0), num_of_bug_lines)
    normal_score = normalize_score(score)
    sorted_dict = dict(sorted(normal_score.items(), key=itemgetter(1), reverse=True))
    with open(dstar_score_file_name, 'w') as f:
        json.dump(sorted_dict, f)


for release in releases:
    evaluate_tarantula_score(release)
    evaluate_ochiai_score(release)
    evaluate_op2_score(release)
    evaluate_barinel_score(release)
    evaluate_dstar_score(release)
