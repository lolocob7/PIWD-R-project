script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, "data_utils.R"))

data <- load_and_clean_data()


## komenatrz :)