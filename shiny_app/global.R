library(shiny)
library(ggplot2)

# Source utility scripts. 
# We use relative paths assuming the app is run from the shiny_app directory or 
# that the working directory is set correctly. 
# If running from project root, these might need adjustment, but .. usually works if wd is shiny_app.
if (file.exists("data_utils.R")) {
  source("data_utils.R")
  source("plot_utils.R")
} else {
  source("../data_utils.R")
  source("../plot_utils.R")
}

# Load data
datasets <- load_and_clean_data()
unemployment_data <- datasets[["stopa_bezrobocia"]]
