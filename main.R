read_all_datasets <- function(folder_path = NULL) {
    if (is.null(folder_path)) {
        script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
        folder_path <- file.path(script_dir, "dataset")
    }

    csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

    datasets <- list()

    for (file in csv_files) {
        file_name <- tools::file_path_sans_ext(basename(file))

        datasets[[file_name]] <- read.csv(file, stringsAsFactors = FALSE)

        message(paste("Loaded:", file_name))
    }

    return(datasets)
}

data <- read_all_datasets()


