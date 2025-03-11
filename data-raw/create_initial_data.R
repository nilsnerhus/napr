# This script creates the initial NAP data for the package
# It should be run once to create the data directory and initial data file
# After this, GitHub Actions will handle updates

# Ensure renv is activated
source(".Rprofile")

# Load required packages
library(devtools)
library(usethis)
library(rvest)     # For web scraping
library(tidyverse) # For data manipulation
library(xml2)      # For XML processing
library(pdftools)  # For extracting text from PDFs
library(httr)      # For downloading files
library(polite)    # For ethical web scraping

# Process NAPs
message("Processing initial NAP data...")

# Source the get_naps_fresh function directly (since the package isn't built yet)
source("R/nap_data <- get_naps_fresh(cache_dir = cache_dir)

# Create the data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Save as package data
message("Saving processed data...")
usethis::use_data(nap_data, overwrite = TRUE)

# Add timestamp to the data documentation
timestamp <- format(Sys.time(), "%Y-%m-%d")
data_file <- "R/data.R"
if (file.exists(data_file)) {
  data_content <- readLines(data_file)
  for (i in seq_along(data_content)) {
    if (grepl("@note Last updated:", data_content[i])) {
      data_content[i] <- paste0("#' @note Last updated: ", timestamp)
break
}
}
writeLines(data_content, data_file)
}

# Document the package
devtools::document()

# Print summary of the data
message("\nSummary of initial NAP data:")
message("Number of NAPs: ", nrow(nap_data))
message("Number with text content: ", sum(!is.na(nap_data$pdf_text)))
message("Regions represented: ", paste(unique(nap_data$region), collapse = ", "))

message("\nInitial data creation complete!")
message("This data will be updated automatically by GitHub Actions.")central.R")

# Create directory for cache
cache_dir <- "data-raw/nap_cache"
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Process fresh NAP data
# Note: This might take a while!
nap_