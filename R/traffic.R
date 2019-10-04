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
#' @param dimensions A vector of dimensions to return. The default is NULL.
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @param use_resource_quotas A boolean indicating whether to use the resource
#'   quotas in Parliament's Google Analytics account to prevent sampling.
#'   This is a faster and more effective way to disable sampling than using
#'   \code{anti_sample}, but using resource quotas consumes tokens from a
#'   limited daily quota. Use this when \code{anti_sample} still fails to
#'   prevent sampling or is taking too long. Note that using resource quotas
#'   takes precendence over anti-samping: if \code{use_resource_quotas} is TRUE
#'   \code{anti_sample} is automatically set to FALSE. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic <- function(
    view_id,
    start_date,
    end_date,
    dimensions = NULL,
    dim_filters = NULL,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    # Disable anti_sample if use_resource_quotas is TRUE
    if (use_resource_quotas) anti_sample = FALSE

    googleAnalyticsR::google_analytics(
            view_id,
            date_range = c(start_date, end_date),
            metrics = c("users", "sessions", "pageviews", "uniquePageviews"),
            dimensions = dimensions,
            dim_filters = dim_filters,
            anti_sample = anti_sample,
            anti_sample_batches = 1,
            useResourceQuotas = use_resource_quotas,
            max = -1) %>%
        tibble::as_tibble() %>%
        janitor::clean_names(case = "snake")
}
