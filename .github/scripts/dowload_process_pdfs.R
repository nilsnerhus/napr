# Script to download and process NAP PDFs
# Make sure renv is active
source(".Rprofile")

# Load required libraries
library(napunfccc)
library(dplyr)
library(httr)
library(pdftools)

# Define cache directories
cache_dir <- ".github/nap_cache"
download_dir <- file.path(cache_dir, "pdfs")

# Create download directory if it doesn't exist
if (!dir.exists(download_dir)) {
  dir.create(download_dir, recursive = TRUE)
  message("Created PDF directory: ", download_dir)
}

# Path to the cached metadata
cache_file <- file.path(cache_dir, "nap_data.rds")

# Check if metadata exists
if (!file.exists(cache_file)) {
  stop("NAP metadata file not found. Please run the scrape_naps.R script first.")
}

# Load the metadata
message("Loading NAP metadata from: ", cache_file)
nap_data <- readRDS(cache_file)
message("Loaded metadata for ", nrow(nap_data), " NAPs")

# Define function to download and process PDFs
download_process_pdfs <- function(
    nap_data,
    download_dir,
    skip_existing = TRUE,
    user_agent = "Educational Research Project on National Adaptation Plans (your.email@example.com)",
    delay = 2
) {
  # Download and process PDFs
  for (i in 1:nrow(nap_data)) {
    # Skip if there's no PDF link
    if (is.na(nap_data$pdf_link[i])) {
      message("No PDF link for row ", i, " (", nap_data$country_name[i], ")")
      next
    }
    
    # Extract country name for filename
    country_name <- nap_data$country_name[i]
    
    # Convert to lowercase and remove any non-alphanumeric characters
    safe_country_name <- tolower(gsub("[^a-zA-Z0-9]", "_", country_name))
    
    # Create a filename with the country name
    filename <- paste0(safe_country_name, ".pdf")
    
    file_path <- file.path(download_dir, filename)
    nap_data$pdf_path[i] <- file_path
    
    # Check if we should skip this file
    if (skip_existing && file.exists(file_path) && nap_data$pdf_download_success[i]) {
      message(paste0("Skipping existing PDF ", i, " of ", nrow(nap_data), ": ", filename))
    } else {
      # Get the complete URL (handle relative URLs)
      pdf_url <- nap_data$pdf_link[i]
      if (!grepl("^http", pdf_url)) {
        # If it's a relative URL, add the base URL
        pdf_url <- paste0("https://napcentral.org", pdf_url)
      }
      
      # Download the PDF
      message(paste0("Downloading PDF ", i, " of ", nrow(nap_data), ": ", filename))
      
      # Try to download the file politely
      download_success <- FALSE
      tryCatch({
        # Honor the delay set
        Sys.sleep(delay)
        
        # Make the request with a proper user agent
        response <- httr::GET(
          pdf_url, 
          httr::write_disk(file_path, overwrite = TRUE),
          httr::user_agent(user_agent)
        )
        
        if (httr::status_code(response) == 200) {
          download_success <- TRUE
          nap_data$pdf_download_success[i] <- TRUE
          message("Download successful.")
        } else {
          message("Download failed with status code: ", httr::status_code(response))
        }
      }, error = function(e) {
        message("Error downloading file: ", e$message)
      })
    }
    
    # Process the PDF if it exists and we haven't already extracted the text
    if (file.exists(file_path) && (is.na(nap_data$pdf_text[i]) || nap_data$pdf_text[i] == "")) {
      message(paste0("Processing PDF ", i, " of ", nrow(nap_data), ": ", filename))
      
      tryCatch({
        # Get number of pages
        pdf_info <- pdftools::pdf_info(file_path)
        nap_data$pdf_pages[i] <- pdf_info$pages
        
        # Extract text from PDF
        text_content <- pdftools::pdf_text(file_path)
        
        # Combine all pages into a single string
        nap_data$pdf_text[i] <- paste(text_content, collapse = "\n\n")
        
        message("Successfully extracted text from PDF (", length(text_content), " pages)")
      }, error = function(e) {
        message("Error processing PDF: ", e$message)
      })
    }
    
    # Save intermediate update
    if (i %% 5 == 0) {  # Save every 5 PDFs
      saveRDS(nap_data, cache_file)
      message("Saved progress after processing ", i, " of ", nrow(nap_data), " PDFs")
    }
  }
  
  return(nap_data)
}

# Execute the download and processing
message("Starting PDF download and processing...")
nap_data <- download_process_pdfs(nap_data, download_dir)

# Save the final data
saveRDS(nap_data, cache_file)
message("Processed NAP data saved to: ", cache_file)

# Create package data
message("Saving processed data to package...")
usethis::use_data(nap_data, overwrite = TRUE)

# Update documentation timestamp
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

message("NAP data update complete!")