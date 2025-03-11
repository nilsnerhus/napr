# Make sure renv is active
source(".Rprofile")

# Load the package
library(napunfccc)

# Create directory for cache
cache_dir <- ".github/nap_cache"
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Process fresh NAP data
message("Processing fresh NAP data...")
nap_data <- get_naps_fresh(cache_dir = cache_dir)

# Save as package data
message("Saving processed data...")
usethis::use_data(nap_data, overwrite = TRUE)

# Update documentation for the data
message("Updating documentation...")
timestamp <- format(Sys.time(), "%Y-%m-%d")

# Add timestamp to the data documentation
data_file <- "R/data.R"
data_content <- readLines(data_file)
for (i in seq_along(data_content)) {
  if (grepl("@note Last updated:", data_content[i])) {
    data_content[i] <- paste0("#' @note Last updated: ", timestamp)
    break
  }
}
writeLines(data_content, data_file)

# Document the package
devtools::document()