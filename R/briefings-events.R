### Functions for downloading events data for research briefings

# Sets of pages: Parliament website -------------------------------------------

#' Download data on document downloads for all pages in the research briefings
#' view with the given filters
#'
#' \code{fetch_rb_downloads_public_by_filter} downloads data on document
#' download metrics for all research briefings on the public Parliament website
#' during the given dates, with the given filters, and returns the data as a
#' tibble.
#'
#' Note that this is not all research briefings document downloads, as it does
#' not include downloads from the research briefings pages on the Parliamentary
#' intranet. Use \code{fetch_rb_downloads_intranet_by_filter} to retrieve
#' equivalent data for the Parliamentary intranet.
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
#' @return A tibble of document download metrics.
#' @keywords internal

fetch_rb_downloads_public_by_filter <- function(
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

    if (end < as.Date(DATE_START_RB_NEW)) {

        downloads <- fetch_events(
                view_id = VIEW_ID_RB_OLD,
                start_date = start_date,
                end_date = end_date,
                dimensions = dimensions,
                dim_filters = dim_filters,
                anti_sample = anti_sample,
                use_resource_quotas = use_resource_quotas) %>%
            dplyr::mutate(property = LABEL_RB_PUBLIC_OLD) %>%
            dplyr::select(.data$property, dplyr::everything())

    } else if (start > as.Date(DATE_END_RB_OLD)) {

        downloads <- fetch_events(
                view_id = VIEW_ID_RB_NEW,
                start_date = start_date,
                end_date = end_date,
                dimensions = dimensions,
                dim_filters = dim_filters,
                anti_sample = anti_sample,
                use_resource_quotas = use_resource_quotas) %>%
            dplyr::mutate(property = LABEL_RB_PUBLIC_NEW) %>%
            dplyr::select(.data$property, dplyr::everything())

    } else {

        downloads <- dplyr::bind_rows(
            fetch_events(
                    view_id = VIEW_ID_RB_OLD,
                    start_date = start_date,
                    end_date = DATE_END_RB_OLD,
                    dimensions = dimensions,
                    dim_filters = dim_filters,
                    anti_sample = anti_sample,
                    use_resource_quotas = use_resource_quotas) %>%
                dplyr::mutate(property = LABEL_RB_PUBLIC_OLD) %>%
                dplyr::select(.data$property, dplyr::everything()),
            fetch_event_categorie(
                    view_id = VIEW_ID_RB_NEW,
                    start_date = DATE_START_RB_NEW,
                    end_date = end_date,
                    dimensions = dimensions,
                    dim_filters = dim_filters,
                    anti_sample = anti_sample,
                    use_resource_quotas = use_resource_quotas) %>%
                dplyr::mutate(property = LABEL_RB_PUBLIC_NEW) %>%
                dplyr::select(.data$property, dplyr::everything()))
    }

    downloads
}

#' Download data on document downloads for different types of Commons research
#' briefings on the main Parliamentary website
#'
#' \code{fetch_rb_downloads_public_by_type} downloads data on document download
#' metrics for specific types of research breifing based on their id prefixes
#' during the given dates and returns the data as a tibble. The specific types
#' of briefings to return are defined with a regular expression that identifies
#' their id prefixes within the page path.
#'
#' Note that this function only returns data for the main Parliament website.
#' Use \code{fetch_rb_downloads_intranet_by_type} to retrieve equivalent data
#' for the Parliamentary intranet.
#'
#' By default, downloads figures are reported separately for each website
#' property in Google Analytics that contains some of the requested data. You
#' can use the \code{combine} argument to optionally combine downloads figures
#' so that each result appears only once with figures totalled across all
#' relevant properties.
#'
#' Download figures can be requested by page. Google Analytics treats webpages
#' requested with different query strings and section anchors as different
#' pages in its downloads data. You can use the \code{merge_paths} argument to
#' optionally sum the figures for pages with the same base path. This makes it
#' easier to calculcate the total number of downloads from each distinct page.
#'
#' If \code{combine} and \code{merge_paths} are both set to TRUE, rows for
#' different properties are combined before paths are merged.
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
#' @param combine A boolean indicating whether to combine the totals from
#'   different properties or to report them separately. The default is FALSE.
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

fetch_rb_downloads_public_by_type <- function(
    start_date,
    end_date,
    type_regexp = PATH_REGEXP_ALL,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    combine = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    category_filter <- googleAnalyticsR::dim_filter(
        "eventCategory",
        "IN_LIST",
        CATEGORY_DOWNLOADS)

    type_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "REGEXP",
        type_regexp)

    network_filter <- googleAnalyticsR::dim_filter(
        "networkLocation",
        "REGEXP",
        NETWORK_REGEXP_INTERNAL)

    filters <- list(category_filter, type_filter)
    if (internal) filters <- list(category_filter, type_filter, network_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    downloads <- fetch_rb_downloads_public_by_filter(
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        by_page = by_page,
        dim_filters = dim_filters,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)

    if (nrow(downloads) == 0) return(tibble::tibble())

    if (combine) {

        downloads <- downloads %>% dplyr::mutate(property = LABEL_RB_PUBLIC)

        if (by_date) {

            if (by_page) {
                downloads <- downloads %>%
                    dplyr::group_by(.data$property, .data$date, .data$page_path)
            } else {
                downloads <- downloads %>%
                    dplyr::group_by(.data$property, .data$date)
            }

        } else {

            if (by_page) {
                downloads <- downloads %>%
                    dplyr::group_by(.data$property, .data$page_path)
            } else {
                downloads <- downloads %>%
                    dplyr::group_by(.data$property)
            }
        }

        downloads <- downloads %>% dplyr::summarise(
                users = sum(.data$users),
                sessions = sum(.data$sessions),
                pageviews = sum(.data$pageviews),
                unique_pageviews = sum(.data$unique_pageviews)) %>%
            dplyr::ungroup()

    }

    if (by_page && merge_paths) {
        downloads <- merge_event_paths(downloads, by_date)
    }

    downloads
}
