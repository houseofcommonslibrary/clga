#' Extract the path from a URL
#'
#' \code{get_path} gets the path component from a URL. This is used to get the
#' unique id of a page within Google Analytics.
#'
#' @param url The url of a page.
#' @return The path component of the URL.
#' @export

get_path <- function(url) {
    url_parts <- httr::parse_url(url)
    stringr::str_to_lower(stringr::str_c("/", url_parts$path))
}
