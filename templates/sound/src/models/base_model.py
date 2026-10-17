import math
import re

import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer

from utils.config import USE_CACHE
from utils.helper import *
from sklearn import metrics
from sklearn.linear_model import LogisticRegression

# Plan_7 核心参数
JACCARD_THRESHOLD = 0.4

VERSION_MAPPING = {
    'activemq-5.2.0': 'activemq-5.1.0',
    'activemq-5.3.0': 'activemq-5.2.0',
    'activemq-5.8.0': 'activemq-5.3.0',
    'camel-2.10.0': 'camel-2.9.0',
    'camel-2.11.0': 'camel-2.10.0',
    'derby-10.5.1.1': 'derby-10.3.1.4',
    'groovy-1_6_BETA_2': 'groovy-1_6_BETA_1',
    'hbase-0.95.2': 'hbase-0.95.0',
    'hive-0.12.0': 'hive-0.10.0',
    'jruby-1.5.0': 'jruby-1.4.0',
    'jruby-1.7.0.preview1': 'jruby-1.5.0',
    'lucene-3.0.0': 'lucene-2.9.0',
    'lucene-3.1': 'lucene-3.0.0',
    'wicket-1.5.3': 'wicket-1.3.0-beta2',
}

LINTER_MAP = {
    'PMD': 'PMD_Dataset',
    'CheckStyle': 'CheckStyle_Dataset',
    'ErrorProne': 'ErrorProne_Dataset',
    'Spotbugs': 'Spotbugs_Dataset',
    'SpotBugs': 'Spotbugs_Dataset',
    'Codacy': 'Codacy_Dataset',
    'Codeql': 'Codeql_Dataset',
    'Sonarqube': 'Sonarqube_Dataset',
}

ORIGINAL_DS_DIR = '/root/workspace/zyf/work4/[Datasets]_warning-files-only-warning-lines/original-datasets'


def tokenize_set(line):
    """将一行代码 tokenize 为 set（去重），用于 Jaccard 计算。"""
    return set(t for t in re.findall(r'\b\w+\b', str(line)) if len(t) > 1)


def jaccard_sim(tokens_a, tokens_b):
    """计算两个 token set 的 Jaccard 相似度。"""
    if not tokens_a or not tokens_b:
        return 0.0
    inter = tokens_a & tokens_b
    union = tokens_a | tokens_b
    return len(inter) / len(union)


class BaseModel(object):
    model_name = 'BaseModel'

    def __init__(self, train_release: str = '', test_release: str = '', test_result_path='', is_realistic=False):
        self.result_path = test_result_path if test_result_path != '' else f'{result_path}/{self.model_name}/'

        self.line_level_result_path = f'{self.result_path}line_result/'

        self.line_level_evaluation_file = f'{self.line_level_result_path}evaluation.csv'
        self.execution_time_file = f'{self.result_path}time.csv'

        self.barinel_score_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/'+train_release+'_barinel_normal.json'
        self.dice_score_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + train_release + '_dice_normal.json'
        self.dstar_score_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + train_release + '_dstar_normal.json'
        self.ochiai_score_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + train_release + '_ochiai_normal.json'
        self.op2_score_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + train_release + '_op2_normal.json'
        self.tarantula_score_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/score/' + train_release + '_tarantula_normal.json'

        self.graph_file_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/graph/'+train_release+'_SF_graph.txt'
        self.barinel_graph_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/graph/'+train_release+'_barinel_SF_graph.txt'
        self.op2_graph_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/graph/'+train_release+'_op2_SF_graph.txt'
        self.dstar_graph_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/graph/'+train_release+'_dstar_SF_graph.txt'
        self.ochiai_graph_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/graph/'+train_release+'_ochiai_SF_graph.txt'
        self.tarantula_graph_name = '/root/workspace/zyf/work4/SOUND-PMD-dataset/Data/graph/'+train_release+'_tarantula_SF_graph.txt'

        self.project_name = train_release.split('-')[0]
        np.random.seed(0)
        self.random_state = 0
        self.threshold_effort = 0.2

        self.train_release = train_release
        self.test_release = test_release

        # --- Plan_7 persistent defect detection: 解析 linter 和 V_train 路径 ---
        linter = None
        for key in LINTER_MAP:
            if key in root_path:
                linter = key
                break
        self.linter = linter
        self.v2_line_level_dir = None
        if linter and linter in LINTER_MAP:
            v2_ds_name = LINTER_MAP[linter]
            v2_line_path = os.path.join(ORIGINAL_DS_DIR, v2_ds_name, 'Line-level')
            if os.path.isdir(v2_line_path):
                self.v2_line_level_dir = v2_line_path
        # V_train 版本（跨版本映射）
        self.train_version = VERSION_MAPPING.get(test_release, test_release)
        # 缓存已加载的 V_train 候选池 {(linter, train_version): [(line_no, tokens_set, src), ...]}
        self._v2_defects_cache = {}
        # 预分配 persistent 相关属性
        self.predicted_max_jaccard = []
        self.predicted_is_persistent = []
        # --- Plan_7 end ---

        self.vector = CountVectorizer(lowercase=False, min_df=2)
        self.clf = LogisticRegression(random_state=0)

        if is_realistic:
            self.train_text, self.train_text_lines, self.train_label, self.train_filename, self.train_text_lines_without_comments = read_file_level_dataset(
                train_release, file_path=f'{root_path}Dataset/File-level/')
        else:
            self.train_text, self.train_text_lines, self.train_label, self.train_filename, self.train_text_lines_without_comments = read_file_level_dataset(
                train_release)
        self.test_text, self.test_text_lines, self.test_labels, self.test_filename, self.test_text_lines_without_comments = read_file_level_dataset(
            test_release)

        self.line_level_result_file = f'{self.line_level_result_path}{self.project_name}/{self.test_release}-result.csv'
        self.commit_buggy_path = f'{dataset_path}{self.test_release.split("-")[0]}'

        self.init_file_path()

        self.test_pred_labels = []
        self.test_pred_scores = []
        self.test_pred_density = dict()

        self.oracle_line_dict, self.oracle_line_set = self.get_oracle_lines()
        self.predicted_buggy_lines = []
        self.predicted_buggy_score = []

        self.num_total_lines = sum([len(lines) for lines in self.test_text_lines])
        self.num_total_lines_without_comments = sum([len(lines) for lines in self.test_text_lines_without_comments])
        self.num_actual_buggy_lines = len(self.oracle_line_set)

        if self.num_total_lines_without_comments != 0:
            self.line_threshold = self.num_actual_buggy_lines / self.num_total_lines_without_comments
        else:
            self.line_threshold = 0.0

        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)

    def init_file_path(self):
        make_path(self.result_path)
        make_path(self.line_level_result_path)
        make_path(f'{self.line_level_result_path}{self.project_name}/')

    def get_oracle_lines(self):
        oracle_line_dict, oracle_line_list = read_line_level_dataset(self.test_release), set()
        for file_name in oracle_line_dict:
            oracle_line_list.update([f'{file_name}:{line}' for line in oracle_line_dict[file_name]])
        return oracle_line_dict, oracle_line_list

    def file_level_prediction(self):
        print(f"Prediction\t=>\t{self.test_release}")
        self.test_pred_labels = np.ones(len(self.test_filename), dtype=int)
        self.test_pred_scores = np.ones(len(self.test_filename), dtype=float)
        # # self.save_file_level_result()

    def line_level_prediction(self):
        print(f'Line level prediction for: {self.model_name}')
        pass

    def analyze_file_level_result(self):
        self.load_file_level_result()

        total_file, identified_file, total_line, identified_line, predicted_file, predicted_line, dropped_line = 0, 0, 0, 0, 0, 0, 0

        for index in range(len(self.test_labels)):
            buggy_line = len(self.test_text_lines[index])
            if self.test_pred_labels[index] == 1:
                predicted_file += 1
                predicted_line += buggy_line

        for index in range(len(self.test_labels)):
            if self.test_labels[index] == 1:
                buggy_line = len(self.oracle_line_dict[self.test_filename[index]])
                if self.test_pred_labels[index] == 1:
                    identified_line += buggy_line
                    identified_file += 1
                else:
                    dropped_line += len(self.oracle_line_dict[self.test_filename[index]])
                total_line += buggy_line
                total_file += 1

        print(f'Buggy file hit info: {identified_file}/{total_file} - {round(identified_file / total_file * 100, 1)}%')
        print(f'Buggy line hit info: {identified_line}/{total_line} - {round(identified_line / total_line * 100, 1)}%')
        print(f'Predicted {predicted_file} buggy files contain {predicted_line} lines')

        append_title = True if not os.path.exists(self.file_level_evaluation_file) else False
        title = 'release,precision,recall,f1-score,accuracy,mcc,identified/total files,max identified/total lines\n'
        with open(self.file_level_evaluation_file, 'a') as file:
            file.write(title) if append_title else None
            file.write(f'{self.test_release},'
                       f'{metrics.precision_score(self.test_labels, self.test_pred_labels)},'
                       f'{metrics.recall_score(self.test_labels, self.test_pred_labels)},'
                       f'{metrics.f1_score(self.test_labels, self.test_pred_labels)},'
                       f'{metrics.accuracy_score(self.test_labels, self.test_pred_labels)},'
                       f'{metrics.matthews_corrcoef(self.test_labels, self.test_pred_labels)},'
                       f'{identified_file}/{total_file},'
                       f'{identified_line}/{total_line},'
                       f'\n')
        return

    def analyze_line_level_result(self):
        self.load_file_level_result()
        total_lines_in_defective_files, buggy_lines_in_defective_files = 0, 0
        for index in range(len(self.test_pred_labels)):
            if self.test_pred_labels[index] == 1:
                total_lines_in_defective_files += len(self.test_text_lines[index])
                if self.test_labels[index] == 1:
                    buggy_lines_in_defective_files += len(self.oracle_line_dict[self.test_filename[index]])

        self.load_line_level_result()

        tp = len(self.oracle_line_set.intersection(self.predicted_buggy_lines[:int(self.num_total_lines_without_comments * 0.2)]))
        fp = int(self.num_total_lines_without_comments * 0.2) - tp
        fn = self.num_actual_buggy_lines - tp
        tn = self.num_total_lines_without_comments - tp - fp - fn
        print(f'Total lines: {self.num_total_lines_without_comments}\n'
              f'Buggy lines: {self.num_actual_buggy_lines}\n'
              f'Predicted lines: {len(self.predicted_buggy_lines)}\n'
              f'TP: {tp}, FP: {fp}, FN: {fn}, TN: {tn}')

        prec = .0 if tp + fp == .0 else tp / (tp + fp)
        recall = .0 if tp + fn == .0 else tp / (tp + fn)

        far = .0 if fp + tn == 0 else fp / (fp + tn)

        d2h = math.sqrt(math.pow(1 - recall, 2) + math.pow(0 - far, 2)) / math.sqrt(2)

        mcc = .0 if tp + fp == .0 or tp + fn == .0 or tn + fp == .0 or tn + fn == .0 else \
            (tp * tn - fp * fn) / math.sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

        f1 = 2 * (prec * recall) / (prec + recall)

        ifa, effort_20, r_0, r_10, r_20, r_30, r_40, r_50, r_60, r_70, r_80, r_90, r_100 = self.rank_strategy()

        buggy_lines_dict = {}
        hit_bugs = set()
        for line in self.predicted_buggy_lines:
            for bug_commit, lines in buggy_lines_dict.items():
                if line in lines:
                    hit_bugs.add(bug_commit)

        label = []
        pred = []
        total_lines = len(self.predicted_buggy_lines)

        for idx, line in enumerate(self.predicted_buggy_lines):
            if line in self.oracle_line_set:
                label.append(1)
            else:
                label.append(0)

            pred.append((total_lines - idx) / total_lines)

        auc = calc_auc(label, pred)

        append_title = True if not os.path.exists(self.line_level_evaluation_file) else False
        print(f'append_title is {append_title}')
        title = 'release,tp,fp,fn,tn,' \
                'precision,recall,far,d2h,mcc,f1,auc,ifa,recall_0,recall_10,recall_20,recall_30,recall_40,recall_50,recall_60,recall_70,recall_80,recall_90,recall_100,effort@20%recall\n'
        with open(self.line_level_evaluation_file, 'a') as file:
            file.write(title) if append_title else None
            file.write(f'{self.test_release},{tp},{fp},{fn},{tn},'
                       f'{prec},{recall},{far},{d2h},{mcc},{f1},{auc},{ifa},{r_0},{r_10},{r_20},{r_30},{r_40},{r_50},{r_60},{r_70},{r_80},{r_90},{r_100},{effort_20}\n')
        return

    def rank_strategy(self):
        ranked_predicted_buggy_lines = []
        test_pred_density = [self.test_pred_density[filename] for filename in self.test_filename]

        defective_file_index = [i for i in np.argsort(test_pred_density)[::-1] if self.test_pred_labels[i] == 1]

        for i in range(len(defective_file_index)):
            defective_filename = self.test_filename[defective_file_index[i]]

            temp_lines, temp_scores = [], []
            for index in range(len(self.predicted_buggy_lines)):
                if self.predicted_buggy_lines[index].startswith(defective_filename):
                    temp_lines.append(self.predicted_buggy_lines[index])
                    temp_scores.append(self.predicted_buggy_score[index])

            sorted_index = np.argsort(temp_scores)[::-1]
            ranked_predicted_buggy_lines.extend(list(np.array(temp_lines)[sorted_index]))

        max_effort = int(self.num_total_lines * self.threshold_effort)
        print(f'Predicted lines: {len(ranked_predicted_buggy_lines)}, Max effort: {max_effort}\n')

        return self.get_rank_performance(ranked_predicted_buggy_lines)

    def get_rank_performance(self, ranked_predicted_buggy_lines):
        count, ifa, recall_20, max_effort = 0, 0, 0, int(self.num_total_lines_without_comments * self.threshold_effort)

        recall_10, recall_30, recall_40, recall_50, recall_60, recall_70, recall_80, recall_90 = 0, 0, 0, 0, 0, 0, 0, 0
        effort_10 = int(self.num_total_lines_without_comments * 0.1)
        effort_30 = int(self.num_total_lines_without_comments * 0.3)
        effort_40 = int(self.num_total_lines_without_comments * 0.4)
        effort_50 = int(self.num_total_lines_without_comments * 0.5)
        effort_60 = int(self.num_total_lines_without_comments * 0.6)
        effort_70 = int(self.num_total_lines_without_comments * 0.7)
        effort_80 = int(self.num_total_lines_without_comments * 0.8)
        effort_90 = int(self.num_total_lines_without_comments * 0.9)

        has_found = False

        for line in ranked_predicted_buggy_lines[:effort_10]:
            if line in self.oracle_line_set:
                recall_10 += 1
        for line in ranked_predicted_buggy_lines[:effort_30]:
            if line in self.oracle_line_set:
                recall_30 += 1
        for line in ranked_predicted_buggy_lines[:effort_40]:
            if line in self.oracle_line_set:
                recall_40 += 1
        for line in ranked_predicted_buggy_lines[:effort_50]:
            if line in self.oracle_line_set:
                recall_50 += 1
        for line in ranked_predicted_buggy_lines[:effort_60]:
            if line in self.oracle_line_set:
                recall_60 += 1
        for line in ranked_predicted_buggy_lines[:effort_70]:
            if line in self.oracle_line_set:
                recall_70 += 1
        for line in ranked_predicted_buggy_lines[:effort_80]:
            if line in self.oracle_line_set:
                recall_80 += 1
        for line in ranked_predicted_buggy_lines[:effort_90]:
            if line in self.oracle_line_set:
                recall_90 += 1

        for line in ranked_predicted_buggy_lines[:max_effort]:
            if line in self.oracle_line_set:
                recall_20 += 1
                if not has_found:
                    ifa = count
                    has_found = True
            count += 1

        effort_20_recall = self.num_total_lines_without_comments
        count = 0
        buggy_lines = 0
        for line in ranked_predicted_buggy_lines:
            count += 1
            if line in self.oracle_line_set:
                buggy_lines += 1
                if buggy_lines / self.num_actual_buggy_lines >= 0.2:
                    effort_20_recall = count
                    break

        return (ifa, effort_20_recall / self.num_total_lines_without_comments, 0,
                recall_10 / self.num_actual_buggy_lines,
                recall_20 / self.num_actual_buggy_lines,
                recall_30 / self.num_actual_buggy_lines,
                recall_40 / self.num_actual_buggy_lines,
                recall_50 / self.num_actual_buggy_lines,
                recall_60 / self.num_actual_buggy_lines,
                recall_70 / self.num_actual_buggy_lines,
                recall_80 / self.num_actual_buggy_lines,
                recall_90 / self.num_actual_buggy_lines, 1)

    def load_v2_defective_lines(self):
        """
        加载 V_train 版本的缺陷行候选池（全局候选，不按文件筛选）。

        数据源: {ORIGINAL_DS_DIR}/{linter}_Dataset/Line-level/{train_version}_defective_lines_dataset.csv

        返回: [(line_number, tokens_set, src), ...]
        """
        if self.v2_line_level_dir is None:
            return []
        cache_key = (self.linter, self.train_version)
        if cache_key in self._v2_defects_cache:
            return self._v2_defects_cache[cache_key]

        csv_path = os.path.join(
            self.v2_line_level_dir,
            f'{self.train_version}_defective_lines_dataset.csv'
        )
        if not os.path.exists(csv_path):
            self._v2_defects_cache[cache_key] = []
            return []

        defects = []
        try:
            with open(csv_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            for line in lines[1:]:  # 跳过标题行
                parts = line.rstrip('\n').split(',', 2)
                if len(parts) < 3:
                    continue
                file_path, line_no_str, src = parts[0], parts[1], parts[2]
                try:
                    line_no = int(line_no_str)
                except ValueError:
                    continue
                tokens = tokenize_set(src)
                defects.append((line_no, tokens, src))
        except Exception:
            defects = []

        self._v2_defects_cache[cache_key] = defects
        return defects

    def compute_persistent_features(self):
        """
        对 predicted_buggy_lines 中的每行，计算:
          - max_jaccard: 与 V_train 所有缺陷行的最大 Jaccard 相似度
          - is_persistent: max_jaccard >= JACCARD_THRESHOLD

        结果存入 self.predicted_max_jaccard 和 self.predicted_is_persistent。
        """
        v2_defects = self.load_v2_defective_lines()
        if not v2_defects:
            self.predicted_max_jaccard = [0.0] * len(self.predicted_buggy_lines)
            self.predicted_is_persistent = [False] * len(self.predicted_buggy_lines)
            return

        max_jaccard_list = []
        is_persistent_list = []

        for buggy_line in self.predicted_buggy_lines:
            # 从 "filename:line_no" 格式中提取源代码行
            parts = buggy_line.split(':')
            if len(parts) < 2:
                max_jaccard_list.append(0.0)
                is_persistent_list.append(False)
                continue
            file_key = ':'.join(parts[:-1])
            try:
                line_idx = int(parts[-1]) - 1  # 转为 0-based
            except ValueError:
                max_jaccard_list.append(0.0)
                is_persistent_list.append(False)
                continue

            # 从 test_text_lines 中找到对应行的源代码
            src_line = None
            for i, fn in enumerate(self.test_filename):
                if fn == file_key:
                    if 0 <= line_idx < len(self.test_text_lines[i]):
                        src_line = self.test_text_lines[i][line_idx]
                    break

            if src_line is None:
                max_jaccard_list.append(0.0)
                is_persistent_list.append(False)
                continue

            v3_tokens = tokenize_set(src_line)
            max_j = 0.0
            for _, v2_tokens, _ in v2_defects:
                j = jaccard_sim(v3_tokens, v2_tokens)
                if j > max_j:
                    max_j = j
                    if max_j >= JACCARD_THRESHOLD:
                        break  # 已知达到阈值，可以提前终止

            max_jaccard_list.append(max_j)
            is_persistent_list.append(max_j >= JACCARD_THRESHOLD)

        self.predicted_max_jaccard = max_jaccard_list
        self.predicted_is_persistent = is_persistent_list

    def save_line_level_result(self):
        # 若 persistent 特征尚未计算，先计算（lazy 模式）
        if not hasattr(self, 'predicted_max_jaccard') or len(self.predicted_max_jaccard) != len(self.predicted_buggy_lines):
            self.compute_persistent_features()
        data = {
            'predicted_buggy_lines': self.predicted_buggy_lines,
            'predicted_buggy_score': self.predicted_buggy_score,
            'max_jaccard': self.predicted_max_jaccard,
        }
        data = pd.DataFrame(data, columns=['predicted_buggy_lines', 'predicted_buggy_score',
                                           'max_jaccard'])
        data.to_csv(self.line_level_result_file, index=False)

    def load_line_level_result(self):
        if len(self.predicted_buggy_lines) == 0:
            df = pd.read_csv(self.line_level_result_file)
            self.predicted_buggy_lines = list(df['predicted_buggy_lines'])
            self.predicted_buggy_score = list(df['predicted_buggy_score'])
            self.num_predict_buggy_lines = len(self.predicted_buggy_lines)
            # 尝试加载新列（向后兼容旧 2 列 CSV）
            if 'max_jaccard' in df.columns:
                self.predicted_max_jaccard = list(df['max_jaccard'])
            else:
                self.predicted_max_jaccard = []
            if 'is_persistent' in df.columns:
                self.predicted_is_persistent = list(df['is_persistent'])
            else:
                self.predicted_is_persistent = []
