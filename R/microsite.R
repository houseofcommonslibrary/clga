### Functions for downloading traffic data for the Commons Library microsite

# Sets of pages: Commons Library microsite ------------------------------------

#' Download traffic data for all pages in the Commons Library microsite view
#' with the given filters
#'
#' \code{fetch_ms_by_filter} downloads data on traffic metrics for all pages on
#' the Commons Library microsite during the given dates, with the given
#' filters, and returns the data as a tibble.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param dimensions A set of dimensions to constrain the results. The
#'   default is NULL.
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by individual page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate metrics for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query string. This parameter is ignored if \code{by_page} is
#'   set to FALSE. Note that while merging paths is necessary for analysis of
#'   individual pages it can introduce small errors in the number of users by
#'   page, as the same user may visit the same page through URLs with different
#'   query strings. The default value is FALSE.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_ms_traffic_by_filter <- function(
    start_date,
    end_date,
    dimensions = NULL,
    dim_filters = NULL,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    if (is.null(dimensions)) dimensions <- c()
    if (by_date) dimensions <- c(dimensions, "date")
    if (by_page) dimensions <- c(dimensions, "pagePath")
    dimensions <- unique(dimensions)

    traffic <- fetch_traffic(
        view_id = VIEW_ID_MS,
        start_date = start_date,
        end_date = end_date,
        dimensions = dimensions,
        dim_filters = dim_filters,
        anti_sample = anti_sample)

    if (by_page && merge_paths) traffic <- merge_paths(traffic, by_date)
    traffic
}

#' Download traffic data for different types of posts on the Commons Library
#' microsite
#'
#' \code{fetch_ms_traffic_by_type} downloads data on traffic metrics for
#' specific types of posts based on their path prefixes during the given dates
#' and returns the data as a tibble. The specific types of posts to return are
#' defined with a regular expression that identifies their prefixes within the
#' page path.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param type_regexp A regular expression that describes the page path for one
#'   or more briefing types. The default is a regexp that matches all pages.
#' @param dimensions A set of dimensions to constrain the results. The
#'   default is NULL.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by individual page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate metrics for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query string. This parameter is ignored if \code{by_page} is
#'   set to FALSE. Note that while merging paths is necessary for analysis of
#'   individual pages it can introduce small errors in the number of users by
#'   page, as the same user may visit the same page through URLs with different
#'   query strings. The default value is FALSE.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_ms_traffic_by_type <- function(
    start_date,
    end_date,
    type_regexp = PATH_REGEXP_ALL,
    dimensions = NULL,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE) {

    type_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "REGEXP",
        type_regexp)

    network_filter <- googleAnalyticsR::dim_filter(
        "networkLocation",
        "REGEXP",
        NETWORK_REGEXP_INTERNAL)

    filters <- list(type_filter)
    if (internal) filters <- list(type_filter, network_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    fetch_ms_traffic_by_filter(
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        dimensions = dimensions,
        dim_filters = dim_filters,
        anti_sample = anti_sample)
}

#' Download traffic data for all pages in the Commons Library microsite
#'
#' \code{fetch_ms_traffic} downloads data on traffic metrics for all posts on
#' the Commons Library microsite during the given dates and returns the data
#' as a tibble.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by individual page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate metrics for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query string. This parameter is ignored if \code{by_page} is
#'   set to FALSE. Note that while merging paths is necessary for analysis of
#'   individual pages it can introduce small errors in the number of users by
#'   page, as the same user may visit the same page through URLs with different
#'   query strings. The default value is FALSE.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_ms_traffic <- function(
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE) {

    fetch_ms_traffic_by_type(
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        anti_sample = anti_sample)
}

#' Download traffic data for all pages in the Commons Library microsite
#'
#' \code{fetch_msi_traffic} downloads data on traffic metrics for all insights
#' on the Commons Library microsite during the given dates and returns the data
#' as a tibble.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by individual page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate metrics for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query string. This parameter is ignored if \code{by_page} is
#'   set to FALSE. Note that while merging paths is necessary for analysis of
#'   individual pages it can introduce small errors in the number of users by
#'   page, as the same user may visit the same page through URLs with different
#'   query strings. The default value is FALSE.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_msi_traffic <- function(
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE) {

    traffic <- fetch_ms_traffic_by_type(
            start_date = start_date,
            end_date = end_date,
            dimensions = "dimension1",
            internal = internal,
            by_date = by_date,
            by_page = by_page,
            anti_sample = anti_sample) %>%
        dplyr::rename(page_category = .data$dimension1)

    if (by_date) {

        if (by_page) {
            traffic <- traffic %>%
                dplyr::group_by(.data$date, .data$page_path)
        } else {
            traffic <- traffic %>%
                dplyr::group_by(.data$date)
        }

    } else {

        if (by_page) {
            traffic <- traffic %>%
                dplyr::group_by(.data$page_path)
        } else {
            traffic <- traffic %>%
                dplyr::mutate(page_category = "insights") %>%
                dplyr::group_by(.data$insights)
        }
    }

    traffic <- traffic %>% dplyr::summarise(
            users = sum(.data$users),
            sessions = sum(.data$sessions),
            pageviews = sum(.data$pageviews),
            unique_pageviews = sum(.data$unique_pageviews)) %>%
        dplyr::ungroup()

    if (by_page && merge_paths) traffic <- merge_paths(traffic, by_date)
    traffic
}

# Individual pages: Commons Library microsite ---------------------------------

#' Download traffic data for a post with the given URL on the Commons Library
#' microsite
#'
#' \code{fetch_traffic_for_ms} downloads data on traffic metrics for a given
#' post url on the Commons Library microsite during the given dates and returns
#' the data as a tibble.
#'
#' @param url The URL of a page for which traffic data is requested.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic_for_ms <- function(
    url,
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    anti_sample = FALSE) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    dimensions <- c()
    if (by_date) dimensions <- c(dimensions, "date")

    page_path <- get_path_from_url(url)

    path_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "BEGINS_WITH",
        page_path)

    network_filter <- googleAnalyticsR::dim_filter(
        "networkLocation",
        "REGEXP",
        NETWORK_REGEXP_INTERNAL)

    filters <- list(path_filter)
    if (internal) filters <- list(path_filter, network_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    traffic <- fetch_traffic(
            view_id = VIEW_ID_MS,
            start_date = start_date,
            end_date = end_date,
            dimensions = dimensions,
            dim_filters = dim_filters,
            anti_sample = anti_sample)

    if (nrow(traffic) == 0) return(traffic)

    traffic <- traffic %>% dplyr::mutate(page_path = page_path)

    if (by_date) {
        traffic <- traffic %>%
            dplyr::select(
                .data$date,
                .data$page_path,
                dplyr::everything())
    } else {
        traffic <- traffic %>%
            dplyr::select(
                .data$page_path,
                dplyr::everything())
    }

    traffic
}
