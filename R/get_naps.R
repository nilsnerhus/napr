#' Access National Adaptation Plans Data
#'
#' Returns the pre-processed National Adaptation Plans (NAPs) data included with 
#' the package. This data is automatically updated through GitHub Actions on a 
#' monthly basis.
#'
#' @return A tibble containing NAP data with metadata and text content.
#' @export
#'
#' @examples
#' # Get the pre-processed data included with the package
#' naps <- get_naps()

get_naps <- function() {
  # Simply return the included dataset
  return(napr::nap_data)
}