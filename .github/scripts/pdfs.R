# Script to download and process NAP PDFs
# Make sure renv is active
source(".Rprofile")

# Load required libraries
library(dplyr)
library(httr)
library(pdftools)

# Start time for tracking script execution
start_time <- Sys.time()
message("Starting NAP PDF processing at ", format(start_time))

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
  stop("NAP metadata file not found. Please run the scrape.R script first.")
}

# Load the metadata
message("Loading NAP metadata from: ", cache_file)
nap_data <- readRDS(cache_file)
message("Loaded metadata for ", nrow(nap_data), " NAPs")

# Count NAPs needing download and processing
need_download <- sum(!nap_data$pdf_download_success & !is.na(nap_data$pdf_link), na.rm = TRUE)
need_processing <- sum(nap_data$pdf_download_success & (is.na(nap_data$pdf_text) | nap_data$pdf_text == ""), na.rm = TRUE)

message("NAPs needing download: ", need_download)
message("NAPs needing text extraction: ", need_processing)

# Define function to retry downloads
download_with_retry <- function(url, destfile, max_attempts = 3, delay_base = 5, user_agent) {
  for (attempt in 1:max_attempts) {
    message("Download attempt ", attempt, " for ", basename(destfile))
    
    tryCatch({
      # Honor the delay set (with exponential backoff on retries)
      if (attempt > 1) {
        backoff_delay <- delay_base * (2^(attempt-1))
        message("Waiting ", backoff_delay, " seconds before retry...")
        Sys.sleep(backoff_delay)
      }
      
      # Make the request with a proper user agent
      response <- httr::GET(
        url, 
        httr::write_disk(destfile, overwrite = TRUE),
        httr::user_agent(user_agent),
        httr::timeout(120) # 2 minute timeout
      )
      
      if (httr::status_code(response) == 200) {
        # Verify file size is greater than 0
        if (file.size(destfile) > 0) {
          message("Download successful.")
          return(TRUE)
        } else {
          message("Downloaded file is empty. Treating as failed download.")
          file.remove(destfile)
        }
      } else {
        message("Download failed with status code: ", httr::status_code(response))
      }
    }, error = function(e) {
      message("Error downloading file: ", e$message)
    })
  }
  
  message("All download attempts failed for ", basename(destfile))
  return(FALSE)
}

# Function to safely extract PDF text with error handling
extract_pdf_text <- function(file_path) {
  tryCatch({
    # Get number of pages
    pdf_info <- pdftools::pdf_info(file_path)
    pages <- pdf_info$pages
    
    message("Extracting text from PDF with ", pages, " pages")
    
    # Extract text from PDF
    text_content <- NULL
    
    # Use timeout to prevent hanging on problematic PDFs
    # This requires the 'R.utils' package, with fallback to basic extraction
    if (requireNamespace("R.utils", quietly = TRUE)) {
      result <- tryCatch({
        R.utils::withTimeout({
          text_content <- pdftools::pdf_text(file_path)
          TRUE
        }, timeout = 300) # 5 minutes
      }, error = function(e) {
        message("Timeout or error extracting text: ", e$message)
        return(FALSE)
      })
      
      if (!result) {
        message("Trying to extract with reduced settings...")
        text_content <- pdftools::pdf_text(file_path, encoding = "UTF-8")
      }
    } else {
      # Fallback if R.utils not available
      text_content <- pdftools::pdf_text(file_path)
    }
    
    # Combine all pages into a single string
    full_text <- paste(text_content, collapse = "\n\n")
    
    return(list(
      success = TRUE,
      pages = pages,
      text = full_text
    ))
  }, error = function(e) {
    message("Error processing PDF: ", e$message)
    return(list(
      success = FALSE,
      pages = NA_integer_,
      text = NA_character_
    ))
  })
}

# Download and process PDFs
user_agent <- "Educational Research Project on National Adaptation Plans (your.email@example.com)"
delay <- 5 # seconds between downloads

# Count for tracking progress
total_naps <- nrow(nap_data)
downloaded_count <- 0
processed_count <- 0

# Process PDFs
for (i in 1:nrow(nap_data)) {
  # Skip if there's no PDF link
  if (is.na(nap_data$pdf_link[i])) {
    message("No PDF link for entry ", i, " (", nap_data$country_name[i], ")")
    next
  }
  
  # Check if we need to download or process this entry
  need_download <- !nap_data$pdf_download_success[i] 
  need_process <- nap_data$pdf_download_success[i] && (is.na(nap_data$pdf_text[i]) || nap_data$pdf_text[i] == "")
  
  # Skip if nothing to do
  if (!need_download && !need_process) {
    if (i %% 10 == 0) {
      message("Entry ", i, " (", nap_data$country_name[i], ") already processed, skipping.")
    }
    next
  }
  
  # Get the complete URL (handle relative URLs)
  pdf_url <- nap_data$pdf_link[i]
  if (!grepl("^http", pdf_url)) {
    # If it's a relative URL, add the base URL
    pdf_url <- paste0("https://napcentral.org", pdf_url)
  }
  
  # Download if needed
  if (need_download) {
    file_path <- nap_data$pdf_path[i]
    message("Downloading PDF ", i, " of ", total_naps, ": ", basename(file_path))
    
    # Add delay between downloads to be polite
    Sys.sleep(delay)
    
    download_success <- download_with_retry(
      pdf_url, 
      file_path, 
      max_attempts = 3, 
      user_agent = user_agent
    )
    
    nap_data$pdf_download_success[i] <- download_success
    
    if (download_success) {
      downloaded_count <- downloaded_count + 1
      
      # Save progress after each successful download
      saveRDS(nap_data, cache_file)
      message("Saved progress after downloading PDF ", i)
    } else {
      # Skip further processing if download failed
      next
    }
  }
  
  # Process PDF text if needed
  if ((need_download && nap_data$pdf_download_success[i]) || need_process) {
    file_path <- nap_data$pdf_path[i]
    
    if (file.exists(file_path)) {
      message("Extracting text from PDF ", i, " of ", total_naps, ": ", basename(file_path))
      
      pdf_result <- extract_pdf_text(file_path)
      
      if (pdf_result$success) {
        nap_data$pdf_pages[i] <- pdf_result$pages
        nap_data$pdf_text[i] <- pdf_result$text
        
        message("Successfully extracted text from PDF (", pdf_result$pages, " pages)")
        processed_count <- processed_count + 1
        
        # Save progress after each successful text extraction
        saveRDS(nap_data, cache_file)
        message("Saved progress after processing PDF ", i)
      }
    } else {
      message("PDF file doesn't exist for processing: ", file_path)
      # Reset download success if file doesn't exist
      nap_data$pdf_download_success[i] <- FALSE
    }
  }
  
  # Print progress update occasionally
  if (i %% 5 == 0 || i == nrow(nap_data)) {
    progress <- round(i / total_naps * 100)
    message("Progress: ", progress, "% (entry ", i, " of ", total_naps, ")")
  }
}

# Save the final data
saveRDS(nap_data, cache_file)
message("Processed NAP data saved to: ", cache_file)

# Save the processed data
message("Saving processed data to package...")
save(nap_data, file = "data/nap_data.rda")

# Complete execution with timing information
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")
message("NAP PDF processing completed in ", round(elapsed, 2), " minutes at ", format(end_time))

# Summary report
message("=== SUMMARY ===")
message("Total NAPs processed: ", total_naps)
message("PDFs downloaded in this run: ", downloaded_count)
message("PDFs text extracted in this run: ", processed_count)
message("Total NAPs with successful downloads: ", sum(nap_data$pdf_download_success, na.rm = TRUE))
message("Total NAPs with text content: ", sum(!is.na(nap_data$pdf_text) & nap_data$pdf_text != "", na.rm = TRUE))