import numpy as np
import os
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.linear_model import LogisticRegression
import json
from .base_model import BaseModel 

class BarinelwithoutCA(BaseModel):
    model_name = 'BarinelwithoutCA'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        self.vector = CountVectorizer(lowercase=False, min_df=2)


    def line_level_prediction(self):
        super(BarinelwithoutCA, self).line_level_prediction()
        if os.path.exists(self.line_level_result_file):
            return

        print(f'Predicting line level defect prediction of {self.model_name}')

        predicted_lines, predicted_score = [], []

        predicted_lines_raw, predicted_score_raw = [], []
        predicted_lines_clean, predicted_score_clean = [], []

        sort_key = list(
            zip(self.test_pred_scores,
                list(
                    len(self.test_text_lines[i]) for i in range(len(self.test_pred_scores))
                )
                )
        )
        sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]

        defective_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 1]

        clean_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 0]

        tokenizer = self.vector.build_tokenizer()

        with open(self.barinel_score_name, 'r') as f:
            token_score = json.load(f)

        for i in range(len(defective_file_index)):
            defective_filename = self.test_filename[defective_file_index[i]]
            if defective_filename not in self.oracle_line_dict:
                self.oracle_line_dict[defective_filename] = []
            defective_file_line_list = self.test_text_lines[defective_file_index[i]]

            num_of_lines = len(defective_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(defective_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]

                if defective_file_line_list[line_index].startswith('*') or defective_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0

                if line_index in self.oracle_line_dict[defective_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_raw.extend([hit_count[i] for i in sorted_index])
            predicted_lines_raw.extend([f'{defective_filename}:{i + 1}' for i in sorted_index])

        for i in range(len(clean_file_index)):
            clean_filename = self.test_filename[clean_file_index[i]]
            if clean_filename not in self.oracle_line_dict:
                self.oracle_line_dict[clean_filename] = []
            clean_file_line_list = self.test_text_lines[clean_file_index[i]]

            num_of_lines = len(clean_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(clean_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]
                if clean_file_line_list[line_index].startswith('*') or clean_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0
                if line_index in self.oracle_line_dict[clean_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_clean.extend([hit_count[i] for i in sorted_index])
            predicted_lines_clean.extend([f'{clean_filename}:{i + 1}' for i in sorted_index])

        index_list_clean = [(i, x) for i, x in enumerate(predicted_score_clean)]
        sorted_list_clean = sorted(index_list_clean, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index_clean = [x[0] for x in sorted_list_clean]

        index_list = [(i, x) for i, x in enumerate(predicted_score_raw)]
        sorted_list = sorted(index_list, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index = [x[0] for x in sorted_list]

        for i in range(len(index_list)):
            predicted_lines.extend([predicted_lines_raw[sorted_index[i]]])
            predicted_score.extend([predicted_score_raw[sorted_index[i]]])

        for i in range(len(index_list_clean)):
            predicted_lines.extend([predicted_lines_clean[sorted_index_clean[i]]])
            predicted_score.extend([predicted_score_clean[sorted_index_clean[i]]])

        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)

        self.save_line_level_result()

    def rank_strategy(self):
        ranked_predicted_buggy_lines = self.predicted_buggy_lines
        max_effort = int(self.num_total_lines_without_comments * self.threshold_effort)
        print(f'Predicted lines: {len(ranked_predicted_buggy_lines)}, Max effort: {max_effort}\n')
        return self.get_rank_performance(ranked_predicted_buggy_lines)


class DstarWithoutCA(BaseModel):
    model_name = 'DstarWithoutCA'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        self.vector = CountVectorizer(lowercase=False, min_df=2)


    def line_level_prediction(self):
        super(DstarWithoutCA, self).line_level_prediction()
        if os.path.exists(self.line_level_result_file):
            return

        print(f'Predicting line level defect prediction of {self.model_name}')

        predicted_lines, predicted_score = [], []

        predicted_lines_raw, predicted_score_raw = [], []
        predicted_lines_clean, predicted_score_clean = [], []

        sort_key = list(
            zip(self.test_pred_scores,
                list(
                    len(self.test_text_lines[i]) for i in range(len(self.test_pred_scores))
                )
                )
        )
        sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]

        defective_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 1]

        clean_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 0]

        tokenizer = self.vector.build_tokenizer()

        with open(self.dstar_score_name, 'r') as f:
            token_score = json.load(f)

        for i in range(len(defective_file_index)):
            defective_filename = self.test_filename[defective_file_index[i]]
            if defective_filename not in self.oracle_line_dict:
                self.oracle_line_dict[defective_filename] = []
            defective_file_line_list = self.test_text_lines[defective_file_index[i]]

            num_of_lines = len(defective_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(defective_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]

                if defective_file_line_list[line_index].startswith('*') or defective_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0

                if line_index in self.oracle_line_dict[defective_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_raw.extend([hit_count[i] for i in sorted_index])
            predicted_lines_raw.extend([f'{defective_filename}:{i + 1}' for i in sorted_index])

        for i in range(len(clean_file_index)):
            clean_filename = self.test_filename[clean_file_index[i]]
            if clean_filename not in self.oracle_line_dict:
                self.oracle_line_dict[clean_filename] = []
            clean_file_line_list = self.test_text_lines[clean_file_index[i]]

            num_of_lines = len(clean_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(clean_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]
                if clean_file_line_list[line_index].startswith('*') or clean_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0
                if line_index in self.oracle_line_dict[clean_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_clean.extend([hit_count[i] for i in sorted_index])
            predicted_lines_clean.extend([f'{clean_filename}:{i + 1}' for i in sorted_index])

        index_list_clean = [(i, x) for i, x in enumerate(predicted_score_clean)]
        sorted_list_clean = sorted(index_list_clean, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index_clean = [x[0] for x in sorted_list_clean]

        index_list = [(i, x) for i, x in enumerate(predicted_score_raw)]
        sorted_list = sorted(index_list, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index = [x[0] for x in sorted_list]

        for i in range(len(index_list)):
            predicted_lines.extend([predicted_lines_raw[sorted_index[i]]])
            predicted_score.extend([predicted_score_raw[sorted_index[i]]])

        for i in range(len(index_list_clean)):
            predicted_lines.extend([predicted_lines_clean[sorted_index_clean[i]]])
            predicted_score.extend([predicted_score_clean[sorted_index_clean[i]]])

        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)

        self.save_line_level_result()

    def rank_strategy(self):
        ranked_predicted_buggy_lines = self.predicted_buggy_lines
        max_effort = int(self.num_total_lines_without_comments * self.threshold_effort)
        print(f'Predicted lines: {len(ranked_predicted_buggy_lines)}, Max effort: {max_effort}\n')
        return self.get_rank_performance(ranked_predicted_buggy_lines)


class OchiaiWithoutCA(BaseModel):
    model_name = 'OchiaiWithoutCA'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        self.vector = CountVectorizer(lowercase=False, min_df=2)


    def line_level_prediction(self):
        super(OchiaiWithoutCA, self).line_level_prediction()
        if os.path.exists(self.line_level_result_file):
            return

        print(f'Predicting line level defect prediction of {self.model_name}')

        predicted_lines, predicted_score = [], []

        predicted_lines_raw, predicted_score_raw = [], []
        predicted_lines_clean, predicted_score_clean = [], []

        sort_key = list(
            zip(self.test_pred_scores,
                list(
                    len(self.test_text_lines[i]) for i in range(len(self.test_pred_scores))
                )
                )
        )
        sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]

        defective_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 1]

        clean_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 0]

        tokenizer = self.vector.build_tokenizer()

        with open(self.ochiai_score_name, 'r') as f:
            token_score = json.load(f)

        for i in range(len(defective_file_index)):
            defective_filename = self.test_filename[defective_file_index[i]]
            if defective_filename not in self.oracle_line_dict:
                self.oracle_line_dict[defective_filename] = []
            defective_file_line_list = self.test_text_lines[defective_file_index[i]]

            num_of_lines = len(defective_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(defective_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]

                if defective_file_line_list[line_index].startswith('*') or defective_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0

                if line_index in self.oracle_line_dict[defective_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_raw.extend([hit_count[i] for i in sorted_index])
            predicted_lines_raw.extend([f'{defective_filename}:{i + 1}' for i in sorted_index])

        for i in range(len(clean_file_index)):
            clean_filename = self.test_filename[clean_file_index[i]]
            if clean_filename not in self.oracle_line_dict:
                self.oracle_line_dict[clean_filename] = []
            clean_file_line_list = self.test_text_lines[clean_file_index[i]]

            num_of_lines = len(clean_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(clean_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]
                if clean_file_line_list[line_index].startswith('*') or clean_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0
                if line_index in self.oracle_line_dict[clean_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_clean.extend([hit_count[i] for i in sorted_index])
            predicted_lines_clean.extend([f'{clean_filename}:{i + 1}' for i in sorted_index])

        index_list_clean = [(i, x) for i, x in enumerate(predicted_score_clean)]
        sorted_list_clean = sorted(index_list_clean, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index_clean = [x[0] for x in sorted_list_clean]

        index_list = [(i, x) for i, x in enumerate(predicted_score_raw)]
        sorted_list = sorted(index_list, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index = [x[0] for x in sorted_list]

        for i in range(len(index_list)):
            predicted_lines.extend([predicted_lines_raw[sorted_index[i]]])
            predicted_score.extend([predicted_score_raw[sorted_index[i]]])

        for i in range(len(index_list_clean)):
            predicted_lines.extend([predicted_lines_clean[sorted_index_clean[i]]])
            predicted_score.extend([predicted_score_clean[sorted_index_clean[i]]])

        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)

        self.save_line_level_result()

    def rank_strategy(self):
        ranked_predicted_buggy_lines = self.predicted_buggy_lines
        max_effort = int(self.num_total_lines_without_comments * self.threshold_effort)
        print(f'Predicted lines: {len(ranked_predicted_buggy_lines)}, Max effort: {max_effort}\n')
        return self.get_rank_performance(ranked_predicted_buggy_lines)


class Op2WithoutCA(BaseModel):
    model_name = 'Op2WithoutCA'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        self.vector = CountVectorizer(lowercase=False, min_df=2)


    def line_level_prediction(self):
        super(Op2WithoutCA, self).line_level_prediction()
        if os.path.exists(self.line_level_result_file):
            return

        print(f'Predicting line level defect prediction of {self.model_name}')

        predicted_lines, predicted_score = [], []

        predicted_lines_raw, predicted_score_raw = [], []
        predicted_lines_clean, predicted_score_clean = [], []

        sort_key = list(
            zip(self.test_pred_scores,
                list(
                    len(self.test_text_lines[i]) for i in range(len(self.test_pred_scores))
                )
                )
        )
        sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]

        defective_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 1]

        clean_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 0]

        tokenizer = self.vector.build_tokenizer()

        with open(self.op2_score_name, 'r') as f:
            token_score = json.load(f)

        for i in range(len(defective_file_index)):
            defective_filename = self.test_filename[defective_file_index[i]]
            if defective_filename not in self.oracle_line_dict:
                self.oracle_line_dict[defective_filename] = []
            defective_file_line_list = self.test_text_lines[defective_file_index[i]]

            num_of_lines = len(defective_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(defective_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]

                if defective_file_line_list[line_index].startswith('*') or defective_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0

                if line_index in self.oracle_line_dict[defective_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_raw.extend([hit_count[i] for i in sorted_index])
            predicted_lines_raw.extend([f'{defective_filename}:{i + 1}' for i in sorted_index])

        for i in range(len(clean_file_index)):
            clean_filename = self.test_filename[clean_file_index[i]]
            if clean_filename not in self.oracle_line_dict:
                self.oracle_line_dict[clean_filename] = []
            clean_file_line_list = self.test_text_lines[clean_file_index[i]]

            num_of_lines = len(clean_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(clean_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]
                if clean_file_line_list[line_index].startswith('*') or clean_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0
                if line_index in self.oracle_line_dict[clean_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_clean.extend([hit_count[i] for i in sorted_index])
            predicted_lines_clean.extend([f'{clean_filename}:{i + 1}' for i in sorted_index])

        index_list_clean = [(i, x) for i, x in enumerate(predicted_score_clean)]
        sorted_list_clean = sorted(index_list_clean, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index_clean = [x[0] for x in sorted_list_clean]

        index_list = [(i, x) for i, x in enumerate(predicted_score_raw)]
        sorted_list = sorted(index_list, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index = [x[0] for x in sorted_list]

        for i in range(len(index_list)):
            predicted_lines.extend([predicted_lines_raw[sorted_index[i]]])
            predicted_score.extend([predicted_score_raw[sorted_index[i]]])

        for i in range(len(index_list_clean)):
            predicted_lines.extend([predicted_lines_clean[sorted_index_clean[i]]])
            predicted_score.extend([predicted_score_clean[sorted_index_clean[i]]])

        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)

        self.save_line_level_result()

    def rank_strategy(self):
        ranked_predicted_buggy_lines = self.predicted_buggy_lines
        max_effort = int(self.num_total_lines_without_comments * self.threshold_effort)
        print(f'Predicted lines: {len(ranked_predicted_buggy_lines)}, Max effort: {max_effort}\n')
        return self.get_rank_performance(ranked_predicted_buggy_lines)


class TarantulaWithoutCA(BaseModel):
    model_name = 'TarantulaWithoutCA'

    def __init__(self, train_release: str = '', test_release: str = '', is_realistic=False):
        super().__init__(train_release, test_release, is_realistic=is_realistic)

        self.vector = CountVectorizer(lowercase=False, min_df=2)


    def line_level_prediction(self):
        super(TarantulaWithoutCA, self).line_level_prediction()
        if os.path.exists(self.line_level_result_file):
            return

        print(f'Predicting line level defect prediction of {self.model_name}')

        predicted_lines, predicted_score = [], []

        predicted_lines_raw, predicted_score_raw = [], []
        predicted_lines_clean, predicted_score_clean = [], []

        sort_key = list(
            zip(self.test_pred_scores,
                list(
                    len(self.test_text_lines[i]) for i in range(len(self.test_pred_scores))
                )
                )
        )
        sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]

        defective_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 1]

        clean_file_index = [i for i in sorted_index if self.test_pred_labels[i] == 0]

        tokenizer = self.vector.build_tokenizer()

        with open(self.tarantula_score_name, 'r') as f:
            token_score = json.load(f)

        for i in range(len(defective_file_index)):
            defective_filename = self.test_filename[defective_file_index[i]]
            if defective_filename not in self.oracle_line_dict:
                self.oracle_line_dict[defective_filename] = []
            defective_file_line_list = self.test_text_lines[defective_file_index[i]]

            num_of_lines = len(defective_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(defective_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]

                if defective_file_line_list[line_index].startswith('*') or defective_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0

                if line_index in self.oracle_line_dict[defective_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_raw.extend([hit_count[i] for i in sorted_index])
            predicted_lines_raw.extend([f'{defective_filename}:{i + 1}' for i in sorted_index])

        for i in range(len(clean_file_index)):
            clean_filename = self.test_filename[clean_file_index[i]]
            if clean_filename not in self.oracle_line_dict:
                self.oracle_line_dict[clean_filename] = []
            clean_file_line_list = self.test_text_lines[clean_file_index[i]]

            num_of_lines = len(clean_file_line_list)
            hit_count = np.zeros(num_of_lines, dtype=float)
            buggy_count = np.zeros(num_of_lines, dtype=int)

            for line_index in range(num_of_lines):
                tokens_in_line = tokenizer(clean_file_line_list[line_index])
                for token in tokens_in_line:
                    if token in token_score:
                        hit_count[line_index] += token_score[token]
                if clean_file_line_list[line_index].startswith('*') or clean_file_line_list[line_index].startswith('/'):
                    hit_count[line_index] = -1.0
                if line_index in self.oracle_line_dict[clean_filename]:
                    buggy_count[line_index] = 1

            sort_key = list(zip(hit_count, buggy_count))
            sorted_index = [i[0] for i in sorted(enumerate(sort_key), key=lambda x: (-x[1][0], x[1][1]))]
            sorted_index = [i for i in sorted_index if hit_count[i] >= 0.0]

            predicted_score_clean.extend([hit_count[i] for i in sorted_index])
            predicted_lines_clean.extend([f'{clean_filename}:{i + 1}' for i in sorted_index])

        index_list_clean = [(i, x) for i, x in enumerate(predicted_score_clean)]
        sorted_list_clean = sorted(index_list_clean, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index_clean = [x[0] for x in sorted_list_clean]

        index_list = [(i, x) for i, x in enumerate(predicted_score_raw)]
        sorted_list = sorted(index_list, key=lambda x: (x[1], -x[0]), reverse=True)
        sorted_index = [x[0] for x in sorted_list]

        for i in range(len(index_list)):
            predicted_lines.extend([predicted_lines_raw[sorted_index[i]]])
            predicted_score.extend([predicted_score_raw[sorted_index[i]]])

        for i in range(len(index_list_clean)):
            predicted_lines.extend([predicted_lines_clean[sorted_index_clean[i]]])
            predicted_score.extend([predicted_score_clean[sorted_index_clean[i]]])

        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)

        self.save_line_level_result()

    def rank_strategy(self):
        ranked_predicted_buggy_lines = self.predicted_buggy_lines
        max_effort = int(self.num_total_lines_without_comments * self.threshold_effort)
        print(f'Predicted lines: {len(ranked_predicted_buggy_lines)}, Max effort: {max_effort}\n')
        return self.get_rank_performance(ranked_predicted_buggy_lines)
