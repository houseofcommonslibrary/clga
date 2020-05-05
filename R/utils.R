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
    path <- stringr::str_to_lower(stringr::str_c("/", url_parts$path))
    if(is.na(path)) path <- "/"
    path
}

#' Aggregate traffic metrics for all pages that have the same root path
#'
#' \code{merge_paths} takes a tibble of traffic metrics by property, date and
#' page and sums the metrics for all pages that have the same path i.e. for
#' all pages whose paths differ only by their query string and internal links.
#'
#' @param traffic A tibble of traffic metrics by property, date and page path
#'   returned from one of the fetch_*_traffic functions.
#' @param by_date A boolean indicating whether the results are broken down by
#'   date.
#' @return A tibble with the same column structure as the input tibble where
#'   the metrics have been summed for all pages with the same root path.
#' @keywords internal

merge_traffic_paths <- function(traffic, by_date) {

    if (!tibble::is_tibble(traffic)) {
        stop("traffic data is not a tibble")
    }

    if (ncol(traffic) == 0) return(traffic)

    expected_colnames <- c(
        "page_path",
        "users",
        "sessions",
        "pageviews",
        "unique_pageviews")

    if (by_date) expected_colnames <- c("date", expected_colnames)
    expected_colnames <- c("property", expected_colnames)

    if (! all(expected_colnames == colnames(traffic))) {
        stop("traffic data does not have the expected columns")
    }

    traffic$page_path <- stringr::str_extract(traffic$page_path,"[^?#]+")

    if (by_date) {
        traffic <- traffic %>% dplyr::group_by(
            .data$property, .data$date, .data$page_path)
    } else {
        traffic <- traffic %>% dplyr::group_by(
            .data$property, .data$page_path)
    }

    traffic %>%
        dplyr::summarise(
            users = sum(.data$users),
            sessions = sum(.data$sessions),
            pageviews = sum(.data$pageviews),
            unique_pageviews = sum(.data$unique_pageviews)) %>%
        dplyr::ungroup()
}

#' Aggregate document download metrics for all pages that have the same root
#' path
#'
#' \code{merge_download_paths} takes a tibble of document download metrics by
#' property, date and page and sums the metrics for all pages that have the
#' same path i.e. for all pages whose paths differ only by their query string
#' and internal links.
#'
#' @param downloads A tibble of document download metrics by property, date and
#'   page path returned from one of the download fetching functions.
#' @param by_date A boolean indicating whether the results are broken down by
#'   date.
#' @return A tibble with the same column structure as the input tibble where
#'   the metrics have been summed for all pages with the same root path.
#' @keywords internal

merge_download_paths <- function(downloads, by_date) {

    if (!tibble::is_tibble(downloads)) {
        stop("downloads data is not a tibble")
    }

    if (ncol(downloads) == 0) return(downloads)

    expected_colnames <- c(
        "page_path",
        "total_downloads",
        "unique_downloads")

    if (by_date) expected_colnames <- c("date", expected_colnames)
    expected_colnames <- c("property", expected_colnames)

    if (! all(expected_colnames == colnames(downloads))) {
        stop("downloads data does not have the expected columns")
    }

    downloads$page_path <- stringr::str_extract(downloads$page_path,"[^?#]+")

    if (by_date) {
        downloads <- downloads %>% dplyr::group_by(
            .data$property, .data$date, .data$page_path)
    } else {
        downloads <- downloads %>% dplyr::group_by(
            .data$property, .data$page_path)
    }

    downloads %>%
        dplyr::summarise(
            total_downloads = sum(.data$total_downloads),
            unique_downloads = sum(.data$unique_downloads)) %>%
        dplyr::ungroup()
}

#' Label event metrics with the events they represent
#'
#' \code{label_events} takes a tibble of event metrics and re-labels the event
#' counts to represent the specific events in question.
#'
#' @param events A tibble of event metrics returned from one of the event
#'   fetching functions.
#' @return A tibble whose data is identical to input tibble but whose event
#'   count columns are renamed with the given event name.
#' @keywords internal

label_events <- function(events, event_name) {

    total_label <- stringr::str_glue("total_{event_name}")
    unique_label <- stringr::str_glue("unique_{event_name}")

    events %>% dplyr::rename(
        !! total_label := .data$total_events,
        !! unique_label := .data$unique_events)
}
