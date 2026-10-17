# -*- coding:utf-8 -*-
import warnings
import sys,os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import time
import shutil
from src.models.tools import *
from src.models.explain import *
from src.models.glance import *
from src.models.natural import *
from src.utils.helper import result_path  

all_eval_releases = (
    'activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0',
    'camel-2.10.0', 'camel-2.11.0',
    'derby-10.5.1.1',
    'groovy-1_6_BETA_2',
    'hbase-0.95.2',
    'hive-0.12.0',
    'jruby-1.5.0', 'jruby-1.7.0.preview1',
    'lucene-3.0.0', 'lucene-3.1',
    'wicket-1.5.3'
)


def copy_line_result_to_test(model_name, eval_releases=all_eval_releases):
    """
    After each model runs, copy the files in line_level results that belong to all_eval_releases
    to the result/{model_name}/line_result/test/ directory.
    Source path: result/{model_name}/line_result/{project_name}/{release}-result.csv
    Destination path: result/{model_name}/line_result/test/{release}-result.csv
    """
    for release in eval_releases:
        project = release.split('-')[0]
        src_file = f'{result_path}{model_name}/line_result/{project}/{release}-result.csv'
        dst_file = f'{result_path}{model_name}/line_result/test/{release}-result.csv'
        if os.path.exists(src_file):
            os.makedirs(os.path.dirname(dst_file), exist_ok=True)
            shutil.copy2(src_file, dst_file)
            print(f'  [copy] {release} -> {dst_file}')
        else:
            print(f'  [skip] {src_file} not found, skip')


#sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(main.py))))

warnings.filterwarnings('ignore')
simplefilter(action='ignore', category=FutureWarning)

# The model name and its corresponding python class implementation
MODEL_DICT = {'MIT-LineDP': LineDP,
              'Glance-LR': Glance_LR,
              'GLANCE-MD': Glance_MD_full_threshold,
              }


# ========================= Run RQ1 experiments =================================
def run_cross_release_predict(prediction_model, save_time=False):
    # time
    release_name, build_time_list, pred_time_list = [], [], []
    for project, releases in get_project_releases_dict().items():
        for i in range(len(releases) - 1):
            # 1. Loading data. train data index = i, test data index = i + 1
            print(f'========== {prediction_model.model_name} CR PREDICTION for {releases[i + 1]} ================'[:100])
            # ####### Build time #######
            t_start = time.time()
            model = prediction_model(releases[i], releases[i + 1], is_realistic=True)
            t_end = time.time()
            build_time_list.append(t_end - t_start)


            # ####### Pred time #######
            t_start = time.time()
            model.file_level_prediction()
            # model.analyze_file_level_result()
            model.line_level_prediction()
            # model.analyze_line_level_result()
            t_end = time.time()
            pred_time_list.append(t_end - t_start)
            release_name.append(releases[i + 1])

            data = {'release_name': release_name, 'build_time': build_time_list, 'pred_time': pred_time_list}
            data = pd.DataFrame(data, columns=['release_name', 'build_time', 'pred_time'])
            data.to_csv(model.execution_time_file, index=False) if save_time else None

    # ===== After each model runs, copy the line_result of all_eval_releases to the test directory =====
    copy_line_result_to_test(prediction_model.model_name)


def run_default():
    
    run_cross_release_predict(Glance_LR)
    run_cross_release_predict(Glance_MD_full_threshold)      
    run_cross_release_predict(LineDP)

    pass


def parse_args():
    # If there is no additional parameters in the command line, run the default models.
    if len(sys.argv) == 1:
        run_default()
    # Run the specific models.
    else:
        model_name = sys.argv[1]
        if model_name in MODEL_DICT.keys():
            run_cross_release_predict(MODEL_DICT[model_name])


if __name__ == '__main__':
    parse_args()
