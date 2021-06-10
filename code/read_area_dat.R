library(dplyr)
library(readr)

# Get a vector of all the filenames
files <- list.files(path = "HMEI-GDrive/data/aohareas_historic", 
                    # Keep the full file path
                    full.names = TRUE,
                    # Limit to CSV files
                    pattern = "\\.csv$")

# Read each one in with lapply & bind all
areadat <- 
    lapply(files, function(file_i){
        suppressMessages(read_csv(file_i))
    }) %>% 
    # Collapse list into dataframe
    bind_rows()
