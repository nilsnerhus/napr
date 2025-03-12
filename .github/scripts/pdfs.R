#!/usr/bin/env Rscript
# Optimized NAP PDF processor for GitHub Actions
# Focuses on reliability, parallel processing, and efficient resource usage

# Configure execution environment
options(warn = 1)  # Show warnings immediately
options(stringsAsFactors = FALSE)  # Prevent string/factor conversion 
options(future.fork.enable = TRUE)  # Enable forking for parallel processing

# Load essential libraries without startup messages
suppressPackageStartupMessages({
  library(dplyr, quietly = TRUE)
  library(httr, quietly = TRUE)
  library(pdftools, quietly = TRUE)
  library(future, quietly = TRUE)
  library(future.apply, quietly = TRUE)
})

# Define constants
CACHE_DIR <- ".github/nap_cache"
DOWNLOAD_DIR <- file.path(CACHE_DIR, "pdfs")
METADATA_FILE <- file.path(CACHE_DIR, "nap_data.rds")
LOG_FILE <- file.path(CACHE_DIR, "pdf_processing.log")
USER_AGENT <- "GitHub-Actions-NAP-PDF-Processor (https://github.com/yourusername/napr)"
DOWNLOAD_TIMEOUT <- 300  # 5 minutes
DOWNLOAD_DELAY <- 1      # 1 second
MAX_WORKERS <- min(future::availableCores() - 1, 4)  # Use available cores but limit to 4
CHUNK_SIZE <- 5          # Process in chunks of 5 documents

# Initialize log file
cat(format(Sys.time()), "PDF processing started\n", file = LOG_FILE)

# Ensure directories exist
if (!dir.exists(DOWNLOAD_DIR)) {
  dir.create(DOWNLOAD_DIR, recursive = TRUE)
}

# Check if metadata exists
if (!file.exists(METADATA_FILE)) {
  msg <- "ERROR: NAP metadata file not found. Run scrape.R first."
  message(msg)
  cat(format(Sys.time()), msg, "\n", file = LOG_FILE, append = TRUE)
  if (!interactive()) quit(status = 1, save = "no")
}

# Load metadata
nap_data <- readRDS(METADATA_FILE)
message("Loaded metadata for ", nrow(nap_data), " NAPs")

# Setup parallel processing
plan(multiprocess, workers = MAX_WORKERS)
message("Using parallel processing with ", MAX_WORKERS, " workers")

# Process PDFs efficiently in batches
process_pdfs <- function() {
  # Count work to be done
  need_download <- sum(is.na(nap_data$pdf_download_success) | !nap_data$pdf_download_success, na.rm = TRUE)
  need_processing <- sum((is.na(nap_data$pdf_text) | nap_data$pdf_text == "") & !is.na(nap_data$pdf_path), na.rm = TRUE)
  
  message("Status:")
  message("- PDFs to download: ", need_download)
  message("- PDFs to process: ", need_processing)
  
  # Skip processing if nothing to do
  if (need_download == 0 && need_processing == 0) {
    message("No work to do. All PDFs are downloaded and processed.")
    return(nap_data)
  }
  
  # Prepare for downloads
  download_indices <- which(is.na(nap_data$pdf_download_success) | !nap_data$pdf_download_success)
  download_counts <- list(success = 0, failure = 0)
  
  # Download PDFs sequentially (parallel downloads could trigger rate limiting)
  if (length(download_indices) > 0) {
    message("Downloading ", length(download_indices), " PDFs...")
    
    for (i in download_indices) {
      # Skip if no link
      if (is.na(nap_data$pdf_link[i])) {
        next
      }
      
      # Prepare file path
      country_name <- nap_data$country_name[i]
      safe_country_name <- tolower(gsub("[^a-zA-Z0-9]", "_", country_name))
      filename <- paste0(safe_country_name, ".pdf")
      file_path <- file.path(DOWNLOAD_DIR, filename)
      nap_data$pdf_path[i] <- file_path
      
      # Get complete URL
      pdf_url <- nap_data$pdf_link[i]
      if (!grepl("^http", pdf_url)) {
        pdf_url <- paste0("https://napcentral.org", pdf_url)
      }
      
      # Download with error handling
      message("  Downloading: ", country_name)
      download_result <- tryCatch({
        # Apply rate limiting
        Sys.sleep(DOWNLOAD_DELAY)
        
        # Make the request
        response <- httr::GET(
          pdf_url, 
          httr::write_disk(file_path, overwrite = TRUE),
          httr::user_agent(USER_AGENT),
          httr::timeout(DOWNLOAD_TIMEOUT)
        )
        
        if (httr::status_code(response) == 200) {
          nap_data$pdf_download_success[i] <- TRUE
          download_counts$success <- download_counts$success + 1
          TRUE
        } else {
          log_msg <- paste("Download failed for", country_name, "with status code:", httr::status_code(response))
          cat(format(Sys.time()), log_msg, "\n", file = LOG_FILE, append = TRUE)
          download_counts$failure <- download_counts$failure + 1
          FALSE
        }
      }, error = function(e) {
        log_msg <- paste("Error downloading", country_name, ":", e$message)
        cat(format(Sys.time()), log_msg, "\n", file = LOG_FILE, append = TRUE)
        download_counts$failure <- download_counts$failure + 1
        FALSE
      })
      
      # Save progress after each download
      if (download_result) {
        saveRDS(nap_data, METADATA_FILE)
      }
    }
    
    message("Download summary:")
    message("- Successful: ", download_counts$success)
    message("- Failed: ", download_counts$failure)
    
    # Save after downloads
    saveRDS(nap_data, METADATA_FILE)
  }
  
  # Identify PDFs that need text extraction
  process_indices <- which(
    (is.na(nap_data$pdf_text) | nap_data$pdf_text == "") & 
      !is.na(nap_data$pdf_path) & 
      file.exists(nap_data$pdf_path)
  )
  
  # Process PDFs in parallel using chunks
  if (length(process_indices) > 0) {
    message("Processing ", length(process_indices), " PDFs in parallel...")
    
    # Process in chunks to enable partial progress saving
    chunks <- split(process_indices, ceiling(seq_along(process_indices) / CHUNK_SIZE))
    
    for (chunk_idx in seq_along(chunks)) {
      current_chunk <- chunks[[chunk_idx]]
      message("  Processing chunk ", chunk_idx, "/", length(chunks), 
              " (", length(current_chunk), " PDFs)")
      
      # Process chunk in parallel
      results <- future.apply::future_lapply(current_chunk, function(i) {
        country_name <- nap_data$country_name[i]
        file_path <- nap_data$pdf_path[i]
        
        result <- list(
          success = FALSE,
          pages = NA_integer_,
          text = NA_character_
        )
        
        tryCatch({
          # Extract PDF info
          pdf_info <- pdftools::pdf_info(file_path)
          result$pages <- pdf_info$pages
          
          # Extract text from PDF
          text_content <- pdftools::pdf_text(file_path)
          
          # Combine pages
          result$text <- paste(text_content, collapse = "\n\n")
          result$success <- TRUE
        }, error = function(e) {
          log_msg <- paste("Error processing", country_name, ":", e$message)
          # We can't directly append to log file in parallel workers
          # Will return the message to append in main process
          result$error_message <- log_msg
        })
        
        return(list(
          index = i,
          result = result
        ))
      }, future.seed = TRUE)
      
      # Update data with results
      for (res in results) {
        i <- res$index
        result <- res$result
        
        if (result$success) {
          nap_data$pdf_pages[i] <- result$pages
          nap_data$pdf_text[i] <- result$text
        }
        
        if (!is.null(result$error_message)) {
          cat(format(Sys.time()), result$error_message, "\n", 
              file = LOG_FILE, append = TRUE)
        }
      }
      
      # Save progress after each chunk
      saveRDS(nap_data, METADATA_FILE)
      message("  Saved progress after chunk ", chunk_idx)
    }
    
    # Count processing results
    processed_count <- sum(!is.na(nap_data$pdf_text) & nap_data$pdf_text != "", na.rm = TRUE)
    message("Processing summary:")
    message("- Successfully processed: ", processed_count, "/", length(process_indices))
  }
  
  return(nap_data)
}

# Main execution function
main <- function() {
  start_time <- Sys.time()
  message("PDF processing started at: ", format(start_time))
  
  # Process PDFs
  updated_data <- process_pdfs()
  
  # Save final data
  saveRDS(updated_data, METADATA_FILE)
  message("Processed NAP data saved to: ", METADATA_FILE)
  
  # Create package data
  try({
    usethis::use_data(updated_data, name = "nap_data", overwrite = TRUE)
    
    # Update documentation timestamp
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
      
      # Document package silently
      suppressMessages(devtools::document(quiet = TRUE))
    }
  })
  
  # Print statistics
  total_docs <- nrow(updated_data)
  downloaded <- sum(updated_data$pdf_download_success, na.rm = TRUE)
  processed <- sum(!is.na(updated_data$pdf_text) & updated_data$pdf_text != "", na.rm = TRUE)
  total_pages <- sum(updated_data$pdf_pages, na.rm = TRUE)
  
  message("\nFinal statistics:")
  message("- Total NAPs: ", total_docs)
  message("- Successfully downloaded: ", downloaded, " (", round(downloaded/total_docs*100), "%)")
  message("- Successfully processed: ", processed, " (", round(processed/total_docs*100), "%)")
  message("- Total pages: ", total_pages)
  message("- Average pages per document: ", round(total_pages/processed, 1))
  
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "mins")
  message("PDF processing completed at: ", format(end_time))
  message("Total execution time: ", round(execution_time, 2), " minutes")
  
  # Log completion
  cat(format(Sys.time()), "PDF processing completed\n", 
      file = LOG_FILE, append = TRUE)
  
  return(TRUE)
}

# Run main function with proper exit code
result <- main()
if (!result && !interactive()) {
  quit(status = 1, save = "no")
}