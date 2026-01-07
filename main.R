script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# Source both utility files
source(file.path(script_dir, "data_utils.R"))
source(file.path(script_dir, "plot_utils.R"))

# Load data
datasets <- load_and_clean_data()
unemployment_data <- datasets[["stopa_bezrobocia"]]

# Example: Create a diagram in main.R
# This is the EXACT same function used in the shiny app
print("Generating plot for Austria...")
p <- plot_unemployment_trend(unemployment_data, "Austria")
print(p)

