#' Access National Adaptation Plans Data
#'
#' Returns the pre-processed NAP data included with the package. This data
#' is automatically updated through GitHub Actions on a monthly basis.
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
get_naps <- function() {
  return(napunfccc::nap_data)
}