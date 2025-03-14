# Script to scrape NAP metadata from the NAP Central website
# Make sure renv is active
source(".Rprofile")

# Load required libraries
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

# Start time for tracking script execution
start_time <- Sys.time()
message("Starting NAP metadata scraping at ", format(start_time))

# Define the scraping function with better error handling
url <- "https://napcentral.org/submitted-naps"
user_agent <- "napr (202302046@post.au.dk)"
delay <- 3  # More polite delay between requests

# Try to create a polite session
message("Connecting to NAP Central...")
tryCatch({
  session <- polite::bow(
    url = url,
    user_agent = user_agent,
    delay = delay
  )
  message("Successfully connected to NAP Central")
}, error = function(e) {
  message("Failed to connect to NAP Central: ", e$message)
  # Exit with error code if connection fails
  stop("Connection to NAP Central failed")
})

# Politely scrape the HTML content
message("Scraping NAP Central...")
tryCatch({
  nap_html <- polite::scrape(session)
}, error = function(e) {
  message("Failed to scrape NAP Central: ", e$message)
  stop("Failed to scrape NAP Central")
})

# Extract all rows from the table
rows <- tryCatch({
  nap_html %>% rvest::html_nodes("tbody tr")
}, error = function(e) {
  message("Table structure not found on NAP Central. Has the website been redesigned?")
  stop("Table structure not found. Website may have changed.")
})

if (length(rows) == 0) {
  message("No NAP entries found. Website may have changed structure.")
  stop("No NAP entries found")
}

message(paste("Found", length(rows), "NAP entries to process"))

# Create an empty dataframe to store our results
nap_data <- tibble::tibble(
  nap_id = character(),
  country_name = character(),
  region = character(),
  ldc_sids_marker = character(),
  nap_lang = character(),
  date_posted = character(),
  pdf_link = character(),
  scrape_date = character()  # Added timestamp for tracking when data was collected
)

# Loop through each row and extract the data
successful_entries <- 0
for (i in 1:length(rows)) {
  # Only print progress occasionally to reduce log noise
  if (i %% 10 == 0 || i == 1 || i == length(rows)) {
    message(paste("Processing entry", i, "of", length(rows)))
  }
  
  tryCatch({
    # Extract the cells in this row
    cells <- rows[i] %>% rvest::html_nodes("td")
    
    if (length(cells) < 6) {
      message(paste("Row", i, "has fewer than expected columns. Skipping."))
      next
    }
    
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
      pdf_link <- pdf_elements[1] %>% rvest::html_attr("href")
      
      # Add to our dataframe
      nap_data <- nap_data %>% dplyr::add_row(
        nap_id = nap_id,
        country_name = country_name,
        region = region,
        ldc_sids_marker = ldc_sids_marker,
        nap_lang = nap_lang,
        date_posted = date_posted,
        pdf_link = pdf_link,
        scrape_date = as.character(Sys.Date())
      )
      
      successful_entries <- successful_entries + 1
    } else {
      # Handle rows with no PDF links
      nap_data <- nap_data %>% dplyr::add_row(
        nap_id = nap_id,
        country_name = country_name,
        region = region,
        ldc_sids_marker = ldc_sids_marker,
        nap_lang = nap_lang,
        date_posted = date_posted,
        pdf_link = NA_character_,
        scrape_date = as.character(Sys.Date())
      )
      
      message(paste("No PDF link found for", country_name))
    }
    
  }, error = function(e) {
    message(paste("Error processing row", i, ":", e$message))
  })
  
  # Add a small delay between processing each row to be polite
  Sys.sleep(0.5)
}

message(paste("Successfully scraped information for", nrow(nap_data), "National Adaptation Plans"))

# Check if we got any data
if (nrow(nap_data) == 0) {
  message("No NAPs were successfully scraped. Check if the website structure has changed.")
  stop("No NAPs successfully scraped")
}

# Add columns to store PDF-related information
nap_data <- nap_data %>%
  dplyr::mutate(
    pdf_path = NA_character_,            # Path to the downloaded file
    pdf_download_success = FALSE,        # Whether download was successful
    pdf_pages = NA_integer_,             # Number of pages
    pdf_text = NA_character_             # The extracted text content
  )

# Check for changes compared to previous run (if it exists)
previous_data_path <- file.path(cache_dir, "nap_data.rds")
if (file.exists(previous_data_path)) {
  previous_data <- readRDS(previous_data_path)
  
  # Basic comparison 
  new_count <- nrow(nap_data)
  old_count <- nrow(previous_data)
  
  message("Previous NAP count: ", old_count)
  message("Current NAP count: ", new_count)
  
  if (new_count > old_count) {
    message("Found ", new_count - old_count, " new NAPs since last run")
  } else if (new_count < old_count) {
    message("WARNING: Current count is less than previous count. Check for scraping issues.")
  } else {
    message("NAP count unchanged since last run")
  }
}

# Save the metadata to the cache
cache_file <- file.path(cache_dir, "nap_data.rds")
saveRDS(nap_data, cache_file)
message("NAP metadata saved to: ", cache_file)

# Complete execution with timing information
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")
message("NAP metadata scraping completed in ", round(elapsed, 2), " minutes at ", format(end_time))