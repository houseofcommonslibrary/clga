### Utility functions for the package

#' Extract the path from a URL
#'
#' \code{get_path_from_url} gets the path component from a URL. This is used to
#' get the unique id of a page within Google Analytics.
#'
#' @param url The url of a page.
#' @return The path component of the URL.
#' @export

get_path_from_url <- function(url) {
    url_parts <- urltools::url_parse(url)
    stringr::str_to_lower(stringr::str_c("/", url_parts$path))
}

#' Aggregate metrics for all pages that have the same root path
#'
#' \code{merge_paths} takes a tibble of traffic metrics by date and and page
#'  and sums the metrics for all pages that have the same path i.e. for all
#'  pages whose paths differ only by their query string.
#'
#' @param traffic A tibble of traffic metrics by date and page path returned
#'   from one of the fetch_*_traffic functions.
#' @param by_date A boolean indicating whether the results are broken down by
#'   date.
#' @return A tibble with the same column structure as the input tibble where
#'   the metrics have been summed for all pages with the same root path.
#' @keywords internal

merge_paths <- function(traffic, by_date) {

    if (!tibble::is_tibble(traffic)) {
        stop("traffic data is not a tibble")
    }

    expected_colnames <- c(
        "page_path",
        "users",
        "sessions",
        "pageviews",
        "unique_pageviews")

    if (by_date) expected_colnames <- c("date", expected_colnames)

    if (! all(colnames(traffic) == expected_colnames)) {
        stop("traffic data does not have the expected columns")
    }

    traffic$page_path <- stringr::str_extract(traffic$page_path,"[^?]+")

    if (by_date) {
        traffic <- traffic %>% dplyr::group_by(.data$date, .data$page_path)
    } else {
        traffic <- traffic %>% dplyr::group_by(.data$page_path)
    }

    traffic %>%
        dplyr::summarise(
            users = sum(.data$users),
            sessions = sum(.data$sessions),
            pageviews = sum(.data$pageviews),
            unique_pageviews = sum(.data$unique_pageviews)) %>%
        dplyr::ungroup()
}
