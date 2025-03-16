# NAP scraper with robust link extraction for English NAPs only
source(".Rprofile")

# Load essential libraries
library(dplyr)
library(rvest)
library(polite)
library(httr)
library(pdftools)
library(tibble)

# Create directories
cache_dir <- ".github/cache"
pdf_dir <- file.path(cache_dir, "pdfs")
for (dir in c(cache_dir, pdf_dir)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# Define file paths
cache_file <- file.path(cache_dir, "nap_data.rds")
output_file <- file.path("data", "nap_data.rda")

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
  message("Created 'data' directory")
}

# Scrape NAP metadata
message("Scraping NAP metadata...")
session <- polite::bow(
  url = "https://napcentral.org/submitted-naps",
  user_agent = "napr (nnrorstad@gmail.com)",
  delay = 3
)
nap_html <- polite::scrape(session)

# Get all rows from the table
rows <- nap_html %>% html_nodes("tbody tr")
message("Found ", length(rows), " NAP entries in table")

# Create empty dataframe for results
nap_data <- tibble(
  nap_id = character(),
  country_name = character(),
  region = character(),
  ldc_sids_marker = character(),
  language_options = character(),
  date_posted = character(),
  english_pdf_link = character(),
  pdf_path = character(),
  pdf_download_success = logical(),
  pdf_text = character()
)

# Process each row to extract all needed data
for (i in 1:length(rows)) {
  # Extract cells from this row
  cells <- rows[i] %>% html_nodes("td")
  
  if (length(cells) >= 6) {
    # Extract basic metadata
    nap_id <- cells[1] %>% html_text(trim = TRUE)
    country_name <- cells[2] %>% html_text(trim = TRUE)
    region <- cells[3] %>% html_text(trim = TRUE)
    ldc_sids_marker <- cells[4] %>% html_text(trim = TRUE)
    language_cell <- cells[5]
    date_posted <- cells[6] %>% html_text(trim = TRUE)
    
    # Handle different link structures
    # Try multiple possible selectors for English PDFs
    english_pdf_link <- NULL
    
    # Option 1: Look for links with "English" text
    links <- language_cell %>% html_nodes("a")
    
    if (length(links) > 0) {
      link_texts <- links %>% html_text(trim = TRUE)
      
      for (j in 1:length(links)) {
        # Check if this link text contains "English" (case insensitive)
        if (length(link_texts) >= j && !is.na(link_texts[j]) && 
            grepl("english", link_texts[j], ignore.case = TRUE)) {
          english_pdf_link <- links[j] %>% html_attr("href")
          break
        }
      }
    }
    
    # Option 2: If no English text found, check for links with span containing "English"
    if (is.null(english_pdf_link)) {
      spans <- language_cell %>% html_nodes("a span")
      
      if (length(spans) > 0) {
        span_texts <- spans %>% html_text(trim = TRUE)
        
        for (j in 1:length(spans)) {
          if (length(span_texts) >= j && !is.na(span_texts[j]) && 
              grepl("english", span_texts[j], ignore.case = TRUE)) {
            # Get the parent <a> tag of this span
            parent_link <- spans[j] %>% html_node(xpath = "..") %>% html_attr("href")
            if (!is.na(parent_link)) {
              english_pdf_link <- parent_link
              break
            }
          }
        }
      }
    }
    
    # If we found an English PDF link
    if (!is.null(english_pdf_link)) {
      # Create safe filename
      safe_country <- tolower(gsub("[^a-zA-Z0-9]", "_", country_name))
      pdf_path <- file.path(pdf_dir, paste0(safe_country, ".pdf"))
      
      # Get all language options as text
      language_options <- language_cell %>% html_text(trim = TRUE)
      
      # Add to dataframe
      nap_data <- add_row(nap_data,
                          nap_id = nap_id,
                          country_name = country_name,
                          region = region,
                          ldc_sids_marker = ldc_sids_marker,
                          language_options = language_options,
                          date_posted = date_posted,
                          english_pdf_link = english_pdf_link,
                          pdf_path = pdf_path,
                          pdf_download_success = FALSE,
                          pdf_text = NA_character_
      )
      
      message("Found English NAP for: ", country_name)
    } else {
      message("No English PDF found for: ", country_name)
    }
  }
}

message("Found ", nrow(nap_data), " English NAPs")

# Download and process PDFs
message("Processing English NAPs...")
for (i in 1:nrow(nap_data)) {
  message("Processing ", i, " of ", nrow(nap_data), ": ", nap_data$country_name[i])
  
  # Download PDF
  if (!nap_data$pdf_download_success[i]) {
    pdf_url <- nap_data$english_pdf_link[i]
    if (!grepl("^http", pdf_url)) {
      pdf_url <- paste0("https://napcentral.org", pdf_url)
    }
    
    message("Downloading English PDF for ", nap_data$country_name[i])
    Sys.sleep(3) # Be polite
    
    response <- try(GET(
      pdf_url, 
      write_disk(nap_data$pdf_path[i], overwrite = TRUE),
      user_agent("napr (nnrorstad@gmail.com)"),
      timeout(60)
    ))
    
    if (!inherits(response, "try-error") && status_code(response) == 200 && 
        file.exists(nap_data$pdf_path[i]) && file.size(nap_data$pdf_path[i]) > 0) {
      nap_data$pdf_download_success[i] <- TRUE
      message("Download successful")
    } else {
      message("Download failed")
    }
  }
  
  # Extract text
  if (nap_data$pdf_download_success[i] && is.na(nap_data$pdf_text[i])) {
    message("Extracting text from PDF...")
    text <- try(pdf_text(nap_data$pdf_path[i]))
    if (!inherits(text, "try-error")) {
      nap_data$pdf_text[i] <- paste(text, collapse = "\n\n")
      message("Text extraction successful")
    } else {
      message("Text extraction failed")
    }
  }
  
  # Save progress every 5 records
  if (i %% 5 == 0 || i == nrow(nap_data)) {
    saveRDS(nap_data, cache_file)
    message("Progress saved to cache")
  }
}

# Filter to successful downloads only and keep the standard variable name
nap_data <- nap_data %>% filter(pdf_download_success)

# Save final data
message("Saving final data...")
save(nap_data, file = output_file)

message("Done! Successfully processed ", nrow(nap_data), " English NAPs")