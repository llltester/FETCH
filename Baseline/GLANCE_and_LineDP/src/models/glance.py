# -*- coding:utf-8 -*-
from src.utils.config import USE_CACHE
from src.utils.helper import *
from src.models.base_model import BaseModel


def call_number(statement):
    statement = statement.strip('\"') 
    score = 0
    for char in statement:
        if char == '(':
            score += 1
    return score


################################## Glance ###################################################################
# Overwrite method line_level_prediction(), which shared by all Glance based approaches.

class Glance(BaseModel):
    model_name = 'Glance'

    def __init__(self, train_release='', test_release='', line_threshold=0.5, test_result_path='', is_realistic=False):

        super().__init__(train_release, test_release, test_result_path, is_realistic)
        # Line level classification threshold
        self.line_threshold = line_threshold 
        self.tokenizer = self.vector.build_tokenizer() 
        self.tags = ['todo', 'hack', 'fixme', 'xxx'] 

    def line_level_prediction(self):
        super(Glance, self).line_level_prediction()
        if USE_CACHE and os.path.exists(self.line_level_result_file):
            return

        predicted_lines, predicted_score, predicted_density, predicted_line_numbers, rank,  file_label, functioncall, controlelements, numbers_of_tokens = [], [], [], [], [], [], [], [], []

        # Indices of defective files in descending order according to the prediction scores
        defective_file_index = [i for i in np.argsort(self.test_pred_scores)[::-1] if self.test_pred_labels[i] == 1]


        for i in range(len(defective_file_index)):
            
            defective_filename = self.test_filename[defective_file_index[i]]

            if defective_filename not in self.oracle_line_dict:
                self.oracle_line_dict[defective_filename] = []
         
            defective_file_line_list = self.test_text_lines[defective_file_index[i]]


            num_of_lines = len(defective_file_line_list) 
            hit_count = np.zeros(num_of_lines, dtype=int)
            cc_count = np.zeros(num_of_lines, dtype=int)
            nfc_call = np.zeros(num_of_lines, dtype=int)
            num_of_tokens = np.zeros(num_of_lines, dtype=int)
            
            for line_index in range(num_of_lines):
                tokens_in_line = self.tokenizer(defective_file_line_list[line_index])
                nt = len(tokens_in_line)
                nfc = call_number(defective_file_line_list[line_index])

                num_of_tokens[line_index] = nt

                if nt == 0:
                    hit_count[line_index] = 0
                else:
                    hit_count[line_index] = nt * (nfc + 1)
                
                   
                if nfc > 0:
                    nfc_call[line_index] = nfc
                else:
                    nfc_call[line_index] = 0
                 
                if 'for' in tokens_in_line:
                    cc_count[line_index] = 1
                if 'while' in tokens_in_line:
                    cc_count[line_index] = 1
                if 'do' in tokens_in_line:
                    cc_count[line_index] = 1
                if 'if' in tokens_in_line:
                    cc_count[line_index] = 1
                if 'else' in tokens_in_line:
                    cc_count[line_index] = 1
                if 'switch' in tokens_in_line:
                    cc_count[line_index] = 1
                if 'case' in tokens_in_line:
                    cc_count[line_index] = 1
                if 'continue' in tokens_in_line:
                    cc_count[line_index] = 1
                if 'break' in tokens_in_line:
                    cc_count[line_index] = 1
                if 'return' in tokens_in_line:
                    cc_count[line_index] = 1
                

            sorted_index = np.argsort(-hit_count,kind = 'mergesort').tolist()[:int(len(hit_count) * self.line_threshold)]
            

            sorted_index = [i for i in sorted_index if hit_count[i] > 0]
            # ================= Considering CC statements =====================

            resorted_index = [i for i in sorted_index if cc_count[i]]  
            resorted_index.extend([i for i in sorted_index if not cc_count[i]])  
           
            predicted_line_numbers.extend([self.test_line_numbers[defective_file_index[i]][h] for h in resorted_index])
            predicted_score.extend([hit_count[i] for i in resorted_index])
            numbers_of_tokens.extend([num_of_tokens[i] for i in resorted_index])
            predicted_lines.extend([f'{defective_filename}:{self.test_line_numbers[defective_file_index[i]][h]}' for h in resorted_index])
            rank.extend([i + 1 for i in range(len(resorted_index))]) 
            
            try:
                density = f'{len(np.where(hit_count > 0)) / len(hit_count)}'
            except ZeroDivisionError:
                pass
            
            predicted_density.extend([density for i in resorted_index])# NOTE may be removed later
            file_label.extend(self.test_labels[defective_file_index[i]] for h in resorted_index)
            
            ## 
            functioncall.extend([nfc_call[i] for i in resorted_index])
            controlelements.extend([cc_count[i] for i in resorted_index])

            

        
        self.predicted_buggy_lines = predicted_lines
        self.predicted_buggy_score = predicted_score
        self.predicted_density = predicted_density
        self.num_predict_buggy_lines = len(self.predicted_buggy_lines)  # Require in the super class.
        
        
        
        self.tokensnumber = numbers_of_tokens
        self.file_level_label = file_label
        self.functioncall = functioncall
        self.controlelements = controlelements
        self.rank = rank
        self.predicted_buggy_line_numbers = predicted_line_numbers

        # Save line level result and buggy density
        self.save_line_level_GLANCE_result()
        self.save_buggy_density_file()


################################## Glance-MD_full_threshold Unsupervised ###################################################
# Overwrite method file_level_prediction(), which is unique for different Glance based approaches.

class Glance_MD_full_threshold(Glance):
    """
    File level classifier: ManualDown
    """
    model_name = 'Glance_MD_full_threshold'

    def __init__(self, train_release='', test_release='', line_threshold = 1, file_threshold = 1, test=False,
                 is_realistic=False):
        test_result_path = ''
        if test:
            self.model_name = f'Glance-MD-{str(int(file_threshold * 100))}-{str(int(line_threshold * 100))}'
            test_result_path = f'{root_path}Result/Dis1/{self.model_name}/'
        super().__init__(train_release, test_release, line_threshold, test_result_path, is_realistic)
        # File level classification threshold for unsupervised methods
        self.file_threshold = file_threshold

    def file_level_prediction(self):
        if USE_CACHE and os.path.exists(self.file_level_result_file):
            return

        num_of_files = len(self.test_text)
        test_prediction = np.zeros(num_of_files, dtype=int).tolist()  
        
        loc, debts, score = [], [], []
        for file_index in range(num_of_files):
            
            loc.append(len([line for line in self.test_text_lines[file_index] if line.strip() != '']))
            
            debts.append(len([tag for tag in self.tags if tag in self.test_text[file_index].lower()]))

        
        score = loc
        
        sorted_index = np.argsort(score).tolist()[::-1]

        file_count = 0
        for index in sorted_index:
            if file_count <= len(loc) * self.file_threshold:  #
                test_prediction[index] = 1
                file_count += 1
            else:
                break

        self.test_pred_labels = test_prediction
        self.test_pred_scores = np.array(score)

        # Save file level result
        self.save_file_level_result()

################################## Glance-LR Supervised ###############################################

class Glance_LR(Glance):
    """
    File level classifier: Logistic Regression
    """
    model_name = 'BASE-Glance-LR'
    
    def __init__(self, train_release='', test_release='', line_threshold=0.5, test=False, is_realistic=False):
        test_result_path = ''
        if test:
            self.model_name = f'Glance-LR-{str(int(line_threshold * 100))}'
            test_result_path = f'{root_path}Result/Dis1/{self.model_name}/'
        super().__init__(train_release, test_release, line_threshold, test_result_path, is_realistic)

    def file_level_prediction(self):
        """
        NOTE: This method should be implemented by sub class (i.e., Glance)
        """
        print(f"Prediction\t=>\t{self.test_release}")
        if USE_CACHE and os.path.exists(self.file_level_result_file):
            return

        # 2. Convert text feature into numerical feature, classifier
        # Neither perform lowercase, stemming, nor lemmatization. Remove tokens that appear only once
        train_vtr = self.vector.fit_transform(self.train_text)
        test_vtr = self.vector.transform(self.test_text)
        # 3. Predict defective files, test_predictions
        self.clf.fit(train_vtr, self.train_label)

        self.test_pred_labels = self.clf.predict(test_vtr)
        # Obtain the prediction scores of each buggy lines.
        self.test_pred_scores = np.array([score[1] for score in self.clf.predict_proba(test_vtr)])

        # Save file level result
        self.save_file_level_result()


