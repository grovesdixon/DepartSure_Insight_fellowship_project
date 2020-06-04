#explore.R
#core libraries and objects for exploring the EICU dataset
library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(cowplot)
theme_set(theme_cowplot())
eicu_path = '/Users/grovesdixon/projects/Insight/icu_project/eicu-collaborative-research-database-2.0'
mimic_path = '/Users/grovesdixon/projects/Insight/icu_project/mimiciii_database/'
print('data_paths:')
print(paste('eicu_path:', eicu_path))
print(paste('mimic_path:', mimic_path))

#function to load dataframes
load_csv = function(data_path, file_name){
  file_path = paste(data_path, file_name, sep = '/')
  read_csv(file_path)
}

