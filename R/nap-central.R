#' Access National Adaptation Plans Data
#'
#' Returns the pre-processed NAP data included with the package. This data
#' is automatically updated through GitHub Actions on a monthly basis.
#'
#' @param refresh Logical. If TRUE, fresh data will be downloaded and processed
#'                from the NAP Central website. Default is FALSE.
#' @param ... Additional arguments passed to get_naps_fresh() when refresh=TRUE.
#'
#' @return A tibble containing NAP data with metadata and text content.
#' @export
#'
#' @examples
#' # Get the pre-processed data included with the package
#' naps <- get_naps()
#' 
#' # Check how many NAPs are available
#' nrow(naps)
#' 
#' \dontrun{
#' # Force a fresh download and processing
#' naps <- get_naps(refresh = TRUE)
#' }
get_naps <- function(refresh = FALSE, ...) {
  if (refresh) {
    return(get_naps_fresh(...))
  } else {
    # Return the package dataset
    return(napunfccc::nap_data)
  }
}

#' Download and Process Fresh NAP Data
#'
#' This function scrapes the NAP Central website, downloads PDFs of National 
#' Adaptation Plans, and extracts their content. It includes caching to avoid 
#' redundant downloads.
#'
#' @param cache_dir Character string. Directory to store cache files. Default is "nap_cache".
#' @param skip_existing Logical. Whether to skip downloading files that already exist. Default is TRUE.
#' @param url Character string. The URL of the NAP Central submitted plans page.
#' @param user_agent Character string. The user agent to use for web requests.
#' @param delay Numeric. The delay between requests in seconds. Default is 2.
#'
#' @return A tibble containing NAP data with extracted PDF content.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download fresh NAP data
#' naps <- get_naps_fresh()
#' }
get_naps_fresh <- function(
    cache_dir = "nap_cache",
    skip_existing = TRUE,
    url = "https://napcentral.org/submitted-naps",
    user_agent = "Educational Research Project on National Adaptation Plans (your.email@example.com)",
    delay = 2
) {
  # Create cache directory structure
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message("Created cache directory: ", cache_dir)
  }
  
  download_dir <- file.path(cache_dir, "pdfs")
  if (!dir.exists(download_dir)) {
    dir.create(download_dir)
    message("Created PDF directory: ", download_dir)
  }
  
  # Scrape the NAP Central website
  message("Scraping NAP Central...")
  
  # Create a polite session
  session <- polite::bow(
    url = url,
    user_agent = user_agent,
    delay = delay
  )
  
  # Politely scrape the HTML content
  nap_html <- polite::scrape(session)
  
  # Extract all rows from the table
  rows <- nap_html %>% rvest::html_nodes("tbody tr")
  
  # Create an empty dataframe to store our results
  nap_data <- tibble::tibble(
    nap_id = character(),
    country_name = character(),
    region = character(),
    ldc_sids_marker = character(),
    nap_lang = character(),
    date_posted = character(),
    pdf_link = character()
  )
  
  # Loop through each row and extract the data
  message(paste("Processing", length(rows), "NAP entries"))
  for (i in 1:length(rows)) {
    # Extract the cells in this row
    cells <- rows[i] %>% rvest::html_nodes("td")
    
    # Extract the text from all six columns
    nap_id <- cells[1] %>% rvest::html_text(trim = TRUE)
    country_name <- cells[2] %>% rvest::html_text(trim = TRUE)
    region <- cells[3] %>% rvest::html_text(trim = TRUE)
    ldc_sids_marker <- cells[4] %>% rvest::html_text(trim = TRUE)
    nap_lang <- cells[5] %>% rvest::html_text(trim = TRUE)
    date_posted <- cells[6] %>% rvest::html_text(trim = TRUE)
    
    # Extract just the first (English) PDF link from the language column
    pdf_elements <- cells[5] %>% rvest::html_nodes("p a")
    
    # If there's at least one PDF link, get the first one (English)
    if (length(pdf_elements) > 0) {
      english_pdf_link <- pdf_elements[1] %>% rvest::html_attr("href")
      
      # Add to our dataframe
      nap_data <- nap_data %>% dplyr::add_row(
        nap_id = nap_id,
        country_name = country_name,
        region = region,
        ldc_sids_marker = ldc_sids_marker,
        nap_lang = nap_lang,
        date_posted = date_posted,
        pdf_link = english_pdf_link
      )
    } else {
      # Handle rows with no PDF links
      nap_data <- nap_data %>% dplyr::add_row(
        nap_id = nap_id,
        country_name = country_name,
        region = region,
        ldc_sids_marker = ldc_sids_marker,
        nap_lang = nap_lang,
        date_posted = date_posted,
        pdf_link = NA_character_
      )
    }
  }
  
  message(paste("Successfully scraped information for", nrow(nap_data), "National Adaptation Plans"))
  
  # Add columns to store PDF-related information
  nap_data <- nap_data %>%
    dplyr::mutate(
      pdf_path = NA_character_,         # Path to the downloaded file
      pdf_download_success = FALSE,     # Whether download was successful
      pdf_pages = NA_integer_,          # Number of pages
      pdf_text = NA_character_          # The extracted text content
    )
  
  # Cache file for NAP data
  cache_file <- file.path(cache_dir, "nap_data.rds")
  
  # Save intermediate results to cache
  saveRDS(nap_data, cache_file)
  message("Initial NAP data saved to cache file: ", cache_file)
  
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
    
    # Save updated data to cache after each PDF for safety
    if (i %% 5 == 0) {  # Save every 5 PDFs
      saveRDS(nap_data, cache_file)
      message("Intermediate update saved to cache file: ", cache_file)
    }
  }
  
  # Save final updated data to cache
  saveRDS(nap_data, cache_file)
  message("Updated NAP data saved to cache file: ", cache_file)
  
  # Return the final data
  return(nap_data)
}

#' Clear NAP Data Cache
#'
#' Removes cache files to force fresh downloads and processing on next run.
#'
#' @param cache_dir Character string. Directory containing cache files.
#' @param remove_pdfs Logical. Whether to remove downloaded PDF files too.
#'
#' @return Invisible NULL. Called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear just the NAP data cache but keep PDF files
#' clear_cache()
#' 
#' # Clear everything including PDF files
#' clear_cache(remove_pdfs = TRUE)
#' }
clear_cache <- function(cache_dir = "nap_cache", remove_pdfs = FALSE) {
  cache_file <- file.path(cache_dir, "nap_data.rds")
  
  if (file.exists(cache_file)) {
    file.remove(cache_file)
    message("NAP data cache cleared.")
  } else {
    message("No NAP data cache file found.")
  }
  
  if (remove_pdfs) {
    pdf_dir <- file.path(cache_dir, "pdfs")
    if (dir.exists(pdf_dir)) {
      pdfs <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)
      if (length(pdfs) > 0) {
        file.remove(pdfs)
        message(paste(length(pdfs), "PDF files removed."))
      } else {
        message("No PDF files found in cache.")
      }
    } else {
      message("PDF directory does not exist.")
    }
  }
  
  invisible(NULL)
}