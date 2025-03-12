#' @keywords internal
"_PACKAGE"

#' National Adaptation Plans (NAP) Data Access
#'
#' @description 
#' The \code{napr} package provides access to National Adaptation Plans (NAPs) 
#' submitted to the United Nations Framework Convention on Climate Change (UNFCCC).
#' It offers a collection of pre-processed NAP documents with extracted text and metadata,
#' regularly updated through GitHub Actions workflows.
#'
#' @details
#' The package offers the following key functionalities:
#' * Access to NAP data with \code{\link{get_naps}}
#' * Complete text content from NAP PDFs
#' * Metadata including country information, publication dates, and regional classifications
#'
#' Data is automatically updated monthly from the official NAP Central repository.
#'
#' @section Data Structure:
#' The NAP data is provided as a tibble with the following columns:
#' \itemize{
#'   \item \code{nap_id}: Unique identifier for each NAP
#'   \item \code{country_name}: Name of the country that submitted the NAP
#'   \item \code{region}: Geographic region of the country
#'   \item \code{ldc_sids_marker}: Indicator for Least Developed Countries (LDC) or Small Island Developing States (SIDS)
#'   \item \code{nap_lang}: Available languages for the NAP document
#'   \item \code{date_posted}: Date when the NAP was published
#'   \item \code{pdf_link}: URL to the original PDF document
#'   \item \code{pdf_pages}: Number of pages in the PDF
#'   \item \code{pdf_text}: Full text content extracted from the PDF
#' }
#'
#' @seealso
#' \code{\link{get_naps}} for accessing the NAP data
#'
#' @references
#' NAP Central website: \url{https://napcentral.org/submitted-naps}
#'
#' @author Nils Rørstad \email{202302046@post.au.dk}
#'
#' @importFrom dplyr add_row mutate
#' @importFrom httr GET status_code user_agent write_disk
#' @importFrom polite bow scrape
#' @importFrom rvest html_attr html_nodes html_text
#' @importFrom tibble tibble
#' @importFrom pdftools pdf_text pdf_info
#'
#' @name napr
#' @docType _PACKAGE
NULL
