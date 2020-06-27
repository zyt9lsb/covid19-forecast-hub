from zoltpy.quantile_io import json_io_dict_from_quantile_csv_file
from zoltpy import util
from zoltpy.connection import ZoltarConnection
from zoltpy.covid19 import VALID_TARGET_NAMES, covid19_row_validator, validate_quantile_csv_file

import os
import sys
import yaml

# Function to read metadata file to get model name
def metadata_dict_for_file(metadata_file):
    with open(metadata_file, encoding="utf8") as metadata_fp:
        metadata_dict = yaml.safe_load(metadata_fp)
    return metadata_dict

# Global variables
project_name = 'COVID-19 Forecasts'
connection = None
repo_forecast_directory = './data-processed/'
try:
    connection = util.authenticate()
except Exception as ex:
    print(ex)
    sys.exit(1)

# Obtaining items related to zoltar project
zoltar_project_obj, zoltar_project_timezeros, zoltar_project_models, \
    zoltar_project_models_abbreviations, zoltar_project_forecasts = None, [], [], [], []
try:
    zoltar_project_obj = [project for project in connection.projects if project.name == project_name][0]
    zoltar_project_timezeros = [timezero.timezero_date for timezero in zoltar_project_obj.timezeros]
    zoltar_project_models = [model for model in zoltar_project_obj.models]
    zoltar_project_models_abbreviations = [model.abbreviation for model in zoltar_project_models]

    # Obtain all forecasts on zoltar
    for zoltar_model in zoltar_project_models:
        existing_zoltar_model_forecasts = [forecast.source for forecast in zoltar_model.forecasts]
        zoltar_project_forecasts.extend(existing_zoltar_model_forecasts)
except Exception as ex:
    print(ex)
    sys.exit(1)

# Obtain all forecasts in GitHub repo
repo_forecasts = []
try:
    repo_model_directories = [model +'/' for model in os.listdir(repo_forecast_directory) if "." not in model]
    for directory in repo_model_directories:
        existing_repo_model_forecasts = \
            [forecast for forecast in os.listdir(repo_forecast_directory+directory+"/") if (directory+".csv") in forecast]
        repo_forecasts.extend(existing_repo_model_forecasts)
except Exception as ex:
    print(ex)
    sys.exit(1)

# try:
#     for forecast in zoltar_forecasts:
#         if forecast not in repo_forecasts:
#             print("This forecast on zoltar but not in GitHub repo "+forecast+", perform deletion from zoltar.")
#             util.delete_forecast(connection, project_name, model_name, forecast.timezero.timezero_date)
# except Exception as ex:
#     print(ex)
#     sys.exit(1)      

### Commented out the functionality to delete models until metadata is finalized
# # Obtain the list of <team_name>-<model_abbr> from metadatas 
# # within the repository. This format was used as abbreviation 
# # for zoltar's models
# repo_models_abbreviations = []
# try:
#     repo_model_directories = [model +'/' for model in os.listdir(repo_forecast_directory) if "." not in model]
#     for directory in model_directories:
#         metadata = metadata_dict_for_file(repo_forecast_directory+repo_model_directories+'metadata-'+dir_name+'.txt')
# except Exception as ex:
#     print(ex)
#     sys.exit(1)
