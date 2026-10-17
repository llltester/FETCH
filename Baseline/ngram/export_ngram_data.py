"""
Prepare n-gram training/eval data from preprocessed CSVs.

Usage (from Baseline/ngram/):
    python export_ngram_data.py

Reads (all under Baseline/ngram/):
    ./preprocessed_data/<release>.csv
Writes (all under Baseline/ngram/):
    ./n_gram_data/<release>/src/<filename>.txt
    ./n_gram_data/<release>/line_num/<filename>_line_num.txt
"""
import os

import pandas as pd
from tqdm import tqdm

# Inline the project/release dicts we need (kept identical to DeepLineDP's my_util.py
# so the data layout matches the reference pipeline).
all_train_releases = {
    'activemq': 'activemq-5.0.0', 'camel': 'camel-1.4.0', 'derby': 'derby-10.2.1.6',
    'groovy': 'groovy-1_5_7', 'hbase': 'hbase-0.94.0', 'hive': 'hive-0.9.0',
    'jruby': 'jruby-1.1', 'lucene': 'lucene-2.3.0', 'wicket': 'wicket-1.3.0-incubating-beta-1',
}
all_eval_releases = {
    'activemq': ['activemq-5.1.0', 'activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0'],
    'camel': ['camel-2.9.0', 'camel-2.10.0', 'camel-2.11.0'],
    'derby': ['derby-10.3.1.4', 'derby-10.5.1.1'],
    'groovy': ['groovy-1_6_BETA_1', 'groovy-1_6_BETA_2'],
    'hbase': ['hbase-0.95.0', 'hbase-0.95.2'],
    'hive': ['hive-0.10.0', 'hive-0.12.0'],
    'jruby': ['jruby-1.4.0', 'jruby-1.5.0', 'jruby-1.7.0.preview1'],
    'lucene': ['lucene-2.9.0', 'lucene-3.0.0', 'lucene-3.1'],
    'wicket': ['wicket-1.3.0-beta2', 'wicket-1.5.3'],
}

base_data_dir = './preprocessed_data/'
data_for_ngram_dir = './n_gram_data/'

proj_names = list(all_train_releases.keys())


def export_df_to_files(data_df, code_file_dir, line_file_dir):
    for filename, df in tqdm(data_df.groupby('filename')):

        code_lines = list(df['code_line'])
        code_str = '\n'.join(code_lines)
        code_str = code_str.lower()
        line_num = list(df['line_number'])
        line_num = [str(l) for l in line_num]

        code_filename = filename.replace('/', '_').replace('.java', '') + '.txt'
        line_filename = filename.replace('/', '_').replace('.java', '') + '_line_num.txt'

        with open(code_file_dir + code_filename, 'w') as f:
            f.write(code_str)

        with open(line_file_dir + line_filename, 'w') as f:
            f.write('\n'.join(line_num))


def export_ngram_data_each_release(release, is_train=False):
    file_dir = data_for_ngram_dir + release + '/'
    file_src_dir = file_dir + 'src/'
    file_line_num_dir = file_dir + 'line_num/'

    if not os.path.exists(file_src_dir):
        os.makedirs(file_src_dir)
    if not os.path.exists(file_line_num_dir):
        os.makedirs(file_line_num_dir)

    data_df = pd.read_csv(base_data_dir + release + '.csv', encoding='latin')

    # get clean files for training only
    if is_train:
        data_df = data_df[(data_df['is_test_file'] == False) & (data_df['is_blank'] == False) & (data_df['file-label'] == False)]
    # get defective files for prediction only
    else:
        data_df = data_df[(data_df['is_test_file'] == False) & (data_df['is_blank'] == False) & (data_df['file-label'] == True)]

    data_df = data_df.fillna('')

    export_df_to_files(data_df, file_src_dir, file_line_num_dir)


def export_data_all_releases(proj_name):
    train_rel = all_train_releases[proj_name]
    eval_rels = all_eval_releases[proj_name]

    export_ngram_data_each_release(train_rel, True)
    for rel in eval_rels:
        export_ngram_data_each_release(rel, False)


def export_ngram_data_all_projs():
    for proj in proj_names:
        export_data_all_releases(proj)
        print('finish', proj)


if __name__ == '__main__':
    export_ngram_data_all_projs()
