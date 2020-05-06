### Functions for downloading events data for the Commons Library website

# Sets of pages: Commons Library microsite ------------------------------------

#' Download data on events for all pages in the Commons Library microsite view
#' with the given filters
#'
#' \code{fetch_ms_events_by_filter} downloads data on event metrics for
#' for all pages on the Commons Library microsite during the given dates,
#' with the given filters, and returns the data as a tibble.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
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
#' @return A tibble of events metrics.
#' @keywords internal

fetch_ms_events_by_filter <- function(
    start_date,
    end_date,
    by_date = FALSE,
    by_page = FALSE,
    dim_filters = NULL,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    dimensions <- c()
    if (by_date) dimensions <- c(dimensions, "date")
    if (by_page) dimensions <- c(dimensions, "pagePath")

    events <- fetch_events(
        view_id = VIEW_ID_MS,
        start_date = start_date,
        end_date = end_date,
        dimensions = dimensions,
        dim_filters = dim_filters,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas) %>%
        dplyr::mutate(property = LABEL_MS) %>%
        dplyr::select(.data$property, dplyr::everything())

    events
}

# Types -----------------------------------------------------------------------

#' Download data on document downloads for different types of posts on the
#' Commons Library microsite
#'
#' \code{fetch_ms_downloads_by_type} downloads data on document download
#' metrics for specific types of pages based on their path prefixes during the
#' given dates and returns the data as a tibble. The specific types of pages to
#' return are defined with a regular expression that identifies their prefixes within the
#' page path.
#'
#' Download figures can be requested by page. Google Analytics treats webpages
#' requested with different query strings and section anchors as different
#' pages in its downloads data. You can use the \code{merge_paths} argument to
#' optionally sum the figures for pages with the same base path. This makes it
#' easier to calculcate the total number of downloads from each distinct page.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param type_regexp A regular expression that describes the page path for one
#'   or more briefing types. The default is a regexp that matches all pages.
#' @param internal A boolean indicating whether to return only the results for
#'   downloads from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate figures for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query strings or internal anchors. This parameter is ignored
#'   if \code{by_page} is set to FALSE. Note that while merging paths is
#'   necessary for analysis of individual pages it can introduce errors in the
#'   number of users by page, as the same user may visit the same page through
#'   URLs with different query strings and anchors. The default value is FALSE.
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
#' @return A tibble of document download metrics.
#' @export

fetch_ms_downloads_by_type <- function(
    start_date,
    end_date,
    type_regexp = PATH_REGEXP_ALL,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    type_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "REGEXP",
        type_regexp)

    category_filter <- googleAnalyticsR::dim_filter(
        "eventCategory",
        "IN_LIST",
        CATEGORY_DOWNLOADS)

    network_filter <- googleAnalyticsR::dim_filter(
        "networkLocation",
        "REGEXP",
        NETWORK_REGEXP_INTERNAL)

    filters <- list(type_filter, category_filter)
    if (internal) filters <- list(type_filter, category_filter, network_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    downloads <- fetch_ms_events_by_filter(
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        by_page = by_page,
        dim_filters = dim_filters,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)

    if (nrow(downloads) == 0) return(tibble::tibble())

    downloads <- label_events(downloads, "downloads")

    if (by_page && merge_paths) {
        downloads <- merge_download_paths(downloads, by_date)
    }

    downloads
}

#' Download data on document downloads for all pages on the Commons Library
#' microsite
#'
#' \code{fetch_ms_downloads} downloads data on document download metrics for
#' all posts on the Commons Library microsite during the given dates and
#' returns the data as a tibble.
#'
#' Download figures can be requested by page. Google Analytics treats webpages
#' requested with different query strings and section anchors as different
#' pages in its downloads data. You can use the \code{merge_paths} argument to
#' optionally sum the figures for pages with the same base path.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   downloads from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate figures for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query strings or internal anchors. This parameter is ignored
#'   if \code{by_page} is set to FALSE. The default value is FALSE.
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
#' @return A tibble of document download metrics.
#' @export

fetch_ms_downloads <- function(
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    fetch_ms_downloads_by_type(
        start_date = start_date,
        end_date = end_date,
        type_regexp = PATH_REGEXP_ALL,
        internal = internal,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)
}

# Individual pages: Commons Library microsite ---------------------------------

#' Download data on document downloads for a page with the given URL on the
#' Commons Library microsite
#'
#' \code{fetch_downloads_for_ms} downloads data on document download metrics
#' for a given url on the Commons Library microsite during the given dates and
#' returns the data as a tibble.
#'
#' @param url The URL of a page for which downloads data is requested.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param match_exact A boolean indicating whether to match only pages with the
#'   exact URL or pages which start with the URL. In general, if you need
#'   traffic data for individual insights and research briefings you don't want
#'   to use an exact match, as it will exclude requests for the page that
#'   contained query strings, which are sometimes used for tracking marketing
#'   campaigns. However, when fetching traffic data for category pages an
#'   exact match is necessary to exclude other pages with same URL stem. The
#'   default is FALSE.
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
#' @param use_resource_quotas A boolean indicating whether to use the resource
#'   quotas in Parliament's Google Analytics account to prevent sampling.
#'   This is a faster and more effective way to disable sampling than using
#'   \code{anti_sample}, but using resource quotas consumes tokens from a
#'   limited daily quota. Use this when \code{anti_sample} still fails to
#'   prevent sampling or is taking too long. Note that using resource quotas
#'   takes precendence over anti-samping: if \code{use_resource_quotas} is TRUE
#'   \code{anti_sample} is automatically set to FALSE. The default is FALSE.
#' @return A tibble of document download metrics.
#' @export

fetch_downloads_for_ms <- function(
    url,
    start_date,
    end_date,
    match_exact = FALSE,
    internal = FALSE,
    by_date = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    dimensions <- c()
    if (by_date) dimensions <- c(dimensions, "date")

    page_path <- get_path_from_url(url)

    match_type <- ifelse(match_exact, "EXACT", "BEGINS_WITH")

    path_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        match_type,
        page_path)

    category_filter <- googleAnalyticsR::dim_filter(
        "eventCategory",
        "IN_LIST",
        CATEGORY_DOWNLOADS)

    network_filter <- googleAnalyticsR::dim_filter(
        "networkLocation",
        "REGEXP",
        NETWORK_REGEXP_INTERNAL)

    filters <- list(path_filter, category_filter)
    if (internal) filters <- list(path_filter, category_filter, network_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    downloads <- fetch_events(
        view_id = VIEW_ID_MS,
        start_date = start_date,
        end_date = end_date,
        dimensions = dimensions,
        dim_filters = dim_filters,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas) %>%
        dplyr::mutate(property = LABEL_MS) %>%
        dplyr::select(.data$property, dplyr::everything())

    if (nrow(downloads) == 0) return(tibble::tibble())

    downloads <- downloads %>% dplyr::mutate(page_path = page_path)

    if (by_date) {
        downloads <- downloads %>%
            dplyr::select(
                .data$property,
                .data$date,
                .data$page_path,
                dplyr::everything())
    } else {
        downloads <- downloads %>%
            dplyr::select(
                .data$property,
                .data$page_path,
                dplyr::everything())
    }

    label_events(downloads, "downloads")
}
