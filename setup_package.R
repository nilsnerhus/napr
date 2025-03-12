# setup_package.R
# Script to programmatically set up package infrastructure

# Load required package
library(usethis)

# Set the working directory to your package root if needed
# setwd("/path/to/your/package")

# Configure package DESCRIPTION
usethis::use_description(
  fields = list(
    Package = "napr",
    Title = "Scraped access to all NAPs published",
    Description = "A package providing access to National Adaptation Plans (NAPs) from the UNFCCC NAP Central website. Contains pre-processed text data from NAP documents, regularly updated via GitHub Actions, with functions to access the latest versions when needed.",
    Version = "0.1.0",
    "Authors@R" = 'person("Nils", "Rørstad", email = "202302046@post.au.dk", role = c("aut", "cre"))',
    License = "MIT + file LICENSE",
    Encoding = "UTF-8",
    Roxygen = "list(markdown = TRUE)",
    RoxygenNote = "7.3.2",
    Depends = "R (>= 3.5.0)",
    LazyData = "true"
  )
)

# Add package dependencies
usethis::use_package("rvest", type = "Imports")
usethis::use_package("dplyr", type = "Imports")
usethis::use_package("xml2", type = "Imports")
usethis::use_package("pdftools", type = "Imports")
usethis::use_package("httr", type =2 "Imports")
usethis::use_package("polite", type = "Imports")
usethis::use_package("tibble", type = "Imports")
usethis::use_package("knitr", type = "Suggests")
usethis::use_package("rmarkdown", type = "Suggests")

# Create LICENSE file
usethis::use_mit_license("Nils Nerhus Rørstad")

# Set up namespace management with roxygen2
usethis::use_namespace()

# Create package-level documentation file
usethis::use_package_doc()

message("Package infrastructure files created. Now add roxygen comments to your R files and run devtools::document().")