# Script to scrape NAP metadata from the NAP Central website
# Make sure renv is active
source(".Rprofile")

# Load required libraries
library(napunfccc)
library(dplyr)
library(rvest)
library(polite)
library(tibble)

# Create cache directory
cache_dir <- ".github/nap_cache"
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
  message("Created cache directory: ", cache_dir)
}

# Define the scraping function
scrape_nap_metadata <- function(
    url = "https://napcentral.org/submitted-naps",
    user_agent = "University Research Project on National Adaptation Plans (your.email@example.com)",
    delay = 2
) {
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
  
  return(nap_data)
}

# Execute the scraping
message("Starting NAP metadata scraping...")
nap_data <- scrape_nap_metadata()

# Save the metadata to the cache
cache_file <- file.path(cache_dir, "nap_data.rds")
saveRDS(nap_data, cache_file)
message("NAP metadata saved to: ", cache_file)