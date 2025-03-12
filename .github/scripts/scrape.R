#!/usr/bin/env Rscript
# Optimized NAP metadata scraper for GitHub Actions
# Focuses on reliability, minimal dependencies, and efficiency

# Suppress warnings to avoid cluttering logs
options(warn = 1)

# Load only essential libraries
suppressPackageStartupMessages({
  library(dplyr, quietly = TRUE)
  library(rvest, quietly = TRUE)
  library(polite, quietly = TRUE)
  library(tibble, quietly = TRUE)
})

# Define constants
CACHE_DIR <- ".github/nap_cache"
CACHE_FILE <- file.path(CACHE_DIR, "nap_data.rds")
NAP_URL <- "https://napcentral.org/submitted-naps"
USER_AGENT <- "GitHub-Actions-NAP-Scraper (https://github.com/yourusername/napr)"
MAX_RETRIES <- 3
REQUEST_DELAY <- 2
REQUEST_TIMEOUT <- 60

# Ensure cache directory exists
if (!dir.exists(CACHE_DIR)) {
  dir.create(CACHE_DIR, recursive = TRUE)
}

# Function to scrape NAP metadata efficiently
scrape_nap_metadata <- function() {
  # Create empty return structure for consistency
  empty_result <- tibble(
    nap_id = character(),
    country_name = character(),
    region = character(),
    ldc_sids_marker = character(),
    nap_lang = character(),
    date_posted = character(),
    pdf_link = character(),
    pdf_path = NA_character_,
    pdf_download_success = FALSE,
    pdf_pages = NA_integer_,
    pdf_text = NA_character_
  )
  
  # Set timeout for this session
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout))
  options(timeout = REQUEST_TIMEOUT)
  
  # Connect to NAP Central with retries
  message("Connecting to NAP Central...")
  session <- NULL
  for (attempt in 1:MAX_RETRIES) {
    try({
      session <- polite::bow(
        url = NAP_URL,
        user_agent = USER_AGENT,
        delay = REQUEST_DELAY
      )
    }, silent = TRUE)
    
    if (!is.null(session)) break
    
    if (attempt < MAX_RETRIES) {
      message("  Connection attempt ", attempt, " failed. Retrying...")
      Sys.sleep(REQUEST_DELAY * attempt)  # Exponential backoff
    } else {
      message("  All connection attempts failed. Aborting.")
      return(empty_result)
    }
  }
  
  # Scrape HTML content
  message("Scraping NAP data...")
  html_content <- try(polite::scrape(session), silent = TRUE)
  
  if (inherits(html_content, "try-error")) {
    message("  Failed to scrape HTML content.")
    return(empty_result)
  }
  
  # Extract table rows
  rows <- try(html_content %>% html_nodes("tbody tr"), silent = TRUE)
  
  if (inherits(rows, "try-error") || length(rows) == 0) {
    message("  No data table found in HTML content.")
    return(empty_result)
  }
  
  message("Found ", length(rows), " NAP entries. Processing...")
  
  # Pre-allocate lists for better performance
  nap_ids <- character(length(rows))
  countries <- character(length(rows))
  regions <- character(length(rows))
  markers <- character(length(rows))
  languages <- character(length(rows))
  dates <- character(length(rows))
  pdf_links <- character(length(rows))
  valid_rows <- logical(length(rows))
  
  # Process rows efficiently
  for (i in seq_along(rows)) {
    # Extract cells
    cells <- try(rows[i] %>% html_nodes("td"), silent = TRUE)
    
    if (inherits(cells, "try-error") || length(cells) < 6) {
      valid_rows[i] <- FALSE
      next
    }
    
    # Extract text from each cell
    nap_ids[i] <- try(cells[1] %>% html_text(trim = TRUE), silent = TRUE)
    countries[i] <- try(cells[2] %>% html_text(trim = TRUE), silent = TRUE)
    regions[i] <- try(cells[3] %>% html_text(trim = TRUE), silent = TRUE)
    markers[i] <- try(cells[4] %>% html_text(trim = TRUE), silent = TRUE)
    languages[i] <- try(cells[5] %>% html_text(trim = TRUE), silent = TRUE)
    dates[i] <- try(cells[6] %>% html_text(trim = TRUE), silent = TRUE)
    
    # Extract PDF link
    pdf_link <- try({
      pdf_elements <- cells[5] %>% html_nodes("p a")
      if (length(pdf_elements) > 0) {
        pdf_elements[1] %>% html_attr("href")
      } else {
        NA_character_
      }
    }, silent = TRUE)
    
    pdf_links[i] <- if (inherits(pdf_link, "try-error")) NA_character_ else pdf_link
    
    # Mark row as valid if it has essential data
    valid_rows[i] <- !inherits(countries[i], "try-error") && 
      !inherits(nap_ids[i], "try-error") &&
      !is.na(countries[i]) && 
      !is.na(nap_ids[i])
    
    # Progress indicator (only every 10 rows to reduce log size)
    if (i %% 10 == 0) {
      message("  Processed ", i, "/", length(rows), " entries")
    }
  }
  
  # Filter valid rows and create final dataframe
  valid_indices <- which(valid_rows)
  
  if (length(valid_indices) == 0) {
    message("No valid NAP entries found.")
    return(empty_result)
  }
  
  # Create dataframe from filtered data
  nap_data <- tibble(
    nap_id = nap_ids[valid_indices],
    country_name = countries[valid_indices],
    region = regions[valid_indices],
    ldc_sids_marker = markers[valid_indices],
    nap_lang = languages[valid_indices],
    date_posted = dates[valid_indices],
    pdf_link = pdf_links[valid_indices],
    pdf_path = NA_character_,
    pdf_download_success = FALSE,
    pdf_pages = NA_integer_,
    pdf_text = NA_character_
  )
  
  message("Successfully extracted data for ", nrow(nap_data), " NAPs")
  
  return(nap_data)
}

# Main execution function
main <- function() {
  start_time <- Sys.time()
  message("NAP scraping started at: ", format(start_time))
  
  # Scrape NAP metadata
  nap_data <- scrape_nap_metadata()
  
  # Save results
  if (nrow(nap_data) > 0) {
    saveRDS(nap_data, CACHE_FILE)
    message("NAP metadata saved to: ", CACHE_FILE)
    message("Entry count: ", nrow(nap_data))
    message("Unique countries: ", length(unique(nap_data$country_name)))
    message("Entries with PDF links: ", sum(!is.na(nap_data$pdf_link)))
    
    # Summary by region
    region_summary <- nap_data %>%
      group_by(region) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    message("NAPs by region:")
    for (i in 1:nrow(region_summary)) {
      message("  ", region_summary$region[i], ": ", region_summary$count[i])
    }
  } else {
    message("ERROR: No NAP data collected. Process failed.")
    return(FALSE)
  }
  
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "mins")
  message("NAP scraping completed at: ", format(end_time))
  message("Total execution time: ", round(execution_time, 2), " minutes")
  
  return(TRUE)
}

# Run the main function with proper exit code
result <- main()
if (!result && !interactive()) {
  quit(status = 1, save = "no")
}