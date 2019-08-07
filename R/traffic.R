### Functions for downloading traffic data

# Sets of pages ---------------------------------------------------------------

#' Download traffic data for a given view
#'
#' \code{fetch_traffic} downloads data on traffic metrics for a given view
#' during the given dates and returns the data as a tibble.
#'
#' @param view_id The view id in Google Analytics
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param dimensions A vector of dimensions to return. The default is "date".
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic <- function(
    view_id,
    start_date,
    end_date,
    dimensions = c("date"),
    dim_filters = NULL) {

    googleAnalyticsR::google_analytics(
            view_id,
            date_range = c(start_date, end_date),
            metrics = c("users", "sessions", "pageviews", "uniquePageviews"),
            dimensions = dimensions,
            dim_filters = dim_filters,
            max = -1) %>%
        tibble::as_tibble() %>%
        dplyr::rename(upageviews = .data$uniquePageviews) %>%
        janitor::clean_names(case = "snake")
}
