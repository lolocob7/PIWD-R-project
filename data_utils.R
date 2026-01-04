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

clean_dataset <- function(df) {
    # Remove rows where OBS_VALUE is NA or empty
    df <- df[!is.na(df$OBS_VALUE) & df$OBS_VALUE != "", ]

    # Remove metadata columns that are not useful for analysis
    cols_to_remove <- c("DATAFLOW", "LAST.UPDATE", "CONF_STATUS", "OBS_FLAG", "freq", "unit")
    df <- df[, !(names(df) %in% cols_to_remove)]

    # Convert TIME_PERIOD to numeric (year)
    if ("TIME_PERIOD" %in% names(df)) {
        df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)
    }

    # Ensure OBS_VALUE is numeric
    if ("OBS_VALUE" %in% names(df)) {
        df$OBS_VALUE <- as.numeric(df$OBS_VALUE)
    }

    return(df)
}

clean_all_datasets <- function(datasets) {
    cleaned <- lapply(datasets, clean_dataset)
    return(cleaned)
}

# Function to load and clean all datasets in one step
load_and_clean_data <- function(folder_path = NULL) {
    data <- read_all_datasets(folder_path)
    data <- clean_all_datasets(data)
    return(data)
}
