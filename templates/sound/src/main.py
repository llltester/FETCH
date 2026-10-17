# from models.glance import *
# from models.linedp import *
from utils.config import *
from utils.helper import *
from models.mymodel import *
import warnings

warnings.filterwarnings('ignore')
warnings.simplefilter(action='ignore', category=FutureWarning)


def get_project_releases_dict():
    project_releases_dict = {}
    for release in PROJECT_RELEASE_LIST:
        project = release.split('-')[0]
        if project not in project_releases_dict:
            project_releases_dict[project] = [release]
        else:
            project_releases_dict[project].append(release)

    return project_releases_dict


def run_cross_release_predict(prediction_model):
    for project, releases in get_project_releases_dict().items():
        for i in range(len(releases) - 1):
            print(f'========== {prediction_model.model_name} CR PREDICTION for {releases[i + 1]} ================'[:60])
            model = prediction_model(releases[i], releases[i + 1])

            model.file_level_prediction()
            # model.analyze_file_level_result()
            model.line_level_prediction()
            # model.analyze_line_level_result()


def run_default():
# 
    # # RQ3
    run_cross_release_predict(BarinelwithoutCA)
    run_cross_release_predict(DstarWithoutCA)
    run_cross_release_predict(OchiaiWithoutCA)
    run_cross_release_predict(Op2WithoutCA)
    run_cross_release_predict(TarantulaWithoutCA)

    pass


if __name__ == '__main__':
    run_default()
