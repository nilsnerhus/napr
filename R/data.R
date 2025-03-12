#' National Adaptation Plans Data
#'
#' A dataset containing information and text content from National Adaptation Plans
#' (NAPs) downloaded from the UNFCCC NAP Central website. This dataset is automatically
#' updated monthly through GitHub Actions.
#'
#' @format A tibble with the following variables:
#' \describe{
#'   \item{nap_id}{Character. The ID of the NAP}
#'   \item{country_name}{Character. The name of the country}
#'   \item{region}{Character. The region of the country}
#'   \item{ldc_sids_marker}{Character. Whether the country is an LDC or SIDS}
#'   \item{nap_lang}{Character. The languages available for the NAP}
#'   \item{date_posted}{Character. The date the NAP was posted}
#'   \item{pdf_link}{Character. The URL to the English PDF}
#'   \item{pdf_path}{Character. The path to the downloaded file (during processing)}
#'   \item{pdf_download_success}{Logical. Whether the download was successful}
#'   \item{pdf_pages}{Integer. The number of pages in the PDF}
#'   \item{pdf_text}{Character. The full text content extracted from the PDF}
#' }
#'
#' @source \url{https://napcentral.org/submitted-naps}
#' @name nap_data
"nap_data"