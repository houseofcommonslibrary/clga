### Functions for downloading data on documents downloaded

#' Download data on document downloads for a given view
#'
#' \code{fetch_downloads} downloads data on document download metrics for a
#' given view during the given dates and returns the data as a tibble.
#'
#' @param view_id The view id in Google Analytics
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param dimensions A vector of dimensions to return. The default is "date".
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @return A tibble of traffic metrics.
#' @keywords internal

fetch_downloads <- function(
    view_id,
    start_date,
    end_date,
    dimensions = c("date"),
    dim_filters = NULL) {

    googleAnalyticsR::google_analytics(
            view_id,
            date_range = c(start_date, end_date),
            metrics = c("totalEvents", "uniqueEvents"),
            dimensions = dimensions,
            dim_filters = dim_filters,
            max = -1) %>%
        tibble::as_tibble() %>%
        janitor::clean_names(case = "snake")
}
