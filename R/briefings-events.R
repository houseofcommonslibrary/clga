### Functions for downloading events data for research briefings

# Sets of pages: Parliament website -------------------------------------------

#' Download data on events for all pages in the research briefings view with
#' the given filters
#'
#' \code{fetch_rb_events_public_by_filter} downloads data on event metrics for
#' all research briefings on the public Parliament website during the given
#' dates, with the given filters, and returns the data as a tibble.
#'
#' Note that this is not all research briefings events, as it does not include
#' events on the research briefings pages on the Parliamentary intranet. Use
#' \code{fetch_rb_events_intranet_by_filter} to retrieve equivalent data for
#' the Parliamentary intranet.
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

fetch_rb_events_public_by_filter <- function(
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

        events <- fetch_events(
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

        events <- fetch_events(
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

        events <- dplyr::bind_rows(
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
            fetch_events(
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

    events
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

    downloads <- fetch_rb_events_public_by_filter(
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

    downloads <- label_events(downloads, "downloads")

    if (by_page && merge_paths) {
        downloads <- merge_download_paths(downloads, by_date)
    }

    downloads
}

#' Download data on document downloads for all pages in the research briefings
#' view
#'
#' \code{fetch_rb_downloads_public} downloads data on document download
#' metrics for all research briefings on the public Parliament website during
#' the given dates and returns the data as a tibble.
#'
#' Note that this is not all research briefing downloads, as it does not
#' include downloads from the research briefings pages on the Parliamentary
#' intranet. Use \code{fetch_rb_downloads_intranet} to retrieve equivalent data
#' for the Parliamentary intranet.
#'
#' By default, download figures are reported separately for each website
#' property in Google Analytics that contains some of the requested data. You
#' can use the \code{combine} argument to optionally combine download figures
#' so that each result appears only once with figures totalled across all
#' relevant properties.
#'
#' Download figures can be requested by page. Google Analytics treats webpages
#' requested with different query strings and section anchors as different
#' pages in its downloads data. You can use the \code{merge_paths} argument to
#' optionally sum the figures for pages with the same base path.
#'
#' If \code{combine} and \code{merge_paths} are both set to TRUE, rows for
#' different properties are combined before paths are merged.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
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

fetch_rb_downloads_public <- function(
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    combine = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    fetch_rb_downloads_public_by_type(
        start_date = start_date,
        end_date = end_date,
        type_regexp = PATH_REGEXP_ALL,
        internal = internal,
        by_date = by_date,
        by_page = by_page,
        combine = combine,
        merge_paths = merge_paths,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)
}

# Sets of pages: Intranet -----------------------------------------------------

#' Download data on events for all pages in the research briefings intranet
#' view with the given filters
#'
#' \code{fetch_rb_events_intranet_by_filter} downloads data on event metrics
#' for all research briefings on the Parliamentary intranet during the given
#' dates, with the given filters, and returns the data as a tibble.
#'
#' Note that this is not all research briefing events, as it does not include
#' events on the research briefings pages on the main Parliament website. Use
#' \code{fetch_rb_events_public_by_filter} to retrieve equivalent data for the
#' main Parliament website.
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

fetch_rb_events_intranet_by_filter <- function(
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
            view_id = VIEW_ID_RB_INTRANET,
            start_date = start_date,
            end_date = end_date,
            dimensions = dimensions,
            dim_filters = dim_filters,
            anti_sample = anti_sample,
            use_resource_quotas = use_resource_quotas) %>%
        dplyr::mutate(property = LABEL_RB_INTRANET) %>%
        dplyr::select(.data$property, dplyr::everything())

    events
}

#' Download data on document downloads for different types of Commons research
#' briefings on the Parliamentary intranet
#'
#' \code{fetch_rb_downloads_intranet_by_type} downloads data on document
#' download metrics for specific types of research breifing based on their id
#' prefixes during the given dates and returns the data as a tibble. The
#' specific types of briefings to return are defined with a regular expression
#' that identifies their id prefixes within the page path.
#'
#' Note that this function only returns data for the Parliamentary intranet.
#' Use \code{fetch_rb_downloads_public_by_type} to retrieve equivalent data
#' for the main Parliamentary website.
#'
#' Download figures can be requested by page. Google Analytics treats webpages
#' requested with different query strings and section anchors as different
#' pages in its downloads data. You can use the \code{merge_paths} argument to
#' optionally sum the figures for pages with the same base path.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param type_regexp A regular expression that describes the page path for one
#'   or more briefing types. The default is a regexp that matches all pages.
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

fetch_rb_downloads_intranet_by_type <- function(
    start_date,
    end_date,
    type_regexp = PATH_REGEXP_ALL,
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

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        list(type_filter, category_filter), operator = "AND")

    downloads <- fetch_rb_events_intranet_by_filter(
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

#' Download data on document downloads for all pages in the research briefings
#' intranet view
#'
#' \code{fetch_rb_downloads_intranet} downloads data on document download
#' metrics for all research briefings on the Parliamentary intranet during the
#' given dates, with the given filters, and returns the data as a tibble.
#'
#' Note that this is not all research briefings downloads, as it does not
#' include downloads on the research briefings pages on the main Parliament
#' website. Use \code{fetch_rb_downloads_public} to retrieve equivalent data
#' for the main Parliament website.
#'
#' Download figures can be requested by page. Google Analytics treats webpages
#' requested with different query strings and section anchors as different
#' pages in its download data. You can use the \code{merge_paths} argument to
#' optionally sum the figures for pages with the same base path.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
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

fetch_rb_downloads_intranet <- function(
    start_date,
    end_date,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    fetch_rb_downloads_intranet_by_type(
        start_date = start_date,
        end_date = end_date,
        type_regexp = PATH_REGEXP_ALL,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)
}

# Sets of pages: All research briefings ---------------------------------------

#' Download data on document downloads for pages in both the research briefings
#' view and the research briefings intranet view using the given functions for
#' each view
#'
#' \code{fetch_rb_downloads_all_properties} downloads data on document download
#' metrics for the same set of research briefings from all properties covering
#' the public Parliament website and the Parliamentary intranet during the
#' given dates and returns the data as a tibble. You must provide matching
#' fetch_* functions for each property.
#'
#' By default, download figures are reported separately for each website
#' property in Google Analytics that contains some of the requested data. You
#' can use the \code{combine} argument to optionally combine download figures
#' so that each result appears only once with figures totalled across all
#' relevant properties.
#'
#' Download figures can be requested by page. Google Analytics treats webpages
#' requested with different query strings and section anchors as different
#' pages in its download data. You can use the \code{merge_paths} argument to
#' optionally sum the figures for pages with the same base path.
#'
#' If \code{combine} and \code{merge_paths} are both set to TRUE, rows for
#' different properties are combined before paths are merged.
#'
#' @param fetch_public A \code{fetch_*_public} function for a set of briefings.
#' @param fetch_intranet A \code{fetch_*_intranet} function for a set of
#'   briefings. For this function to behave as expected \code{fetch_public} and
#'   \code{fetch_intranet} should target the same set of briefings in each view.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
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

fetch_rb_downloads_all_properties <- function(
    fetch_public,
    fetch_intranet,
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    combine = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    public <- fetch_public(
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        by_date = by_date,
        by_page = by_page,
        merge_paths = FALSE,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)

    intranet <- fetch_intranet(
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        by_page = by_page,
        merge_paths = FALSE,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)

    if (nrow(public) == 0 && nrow(intranet) == 0) return(tibble::tibble())

    if (combine) {

        downloads <- dplyr::bind_rows(public, intranet) %>%
            dplyr::mutate(property = LABEL_RB_ALL)

        if (by_date) {

            if (by_page) {
                all <- downloads %>%
                    dplyr::group_by(.data$property, .data$date, .data$page_path)
            } else {
                all <- downloads %>%
                    dplyr::group_by(.data$property, .data$date)
            }

        } else {

            if (by_page) {
                all <- downloads %>%
                    dplyr::group_by(.data$property, .data$page_path)
            } else {
                all <- downloads %>%
                    dplyr::group_by(.data$property)
            }
        }

        downloads <- all %>% dplyr::summarise(
                total_downloads = sum(.data$total_downloads),
                unique_downloads = sum(.data$unique_downloads)) %>%
            dplyr::ungroup()

    } else {

        downloads <- dplyr::bind_rows(public, intranet)
    }

    if (by_page && merge_paths) {
        downloads <- merge_download_paths(downloads, by_date)
    }

    downloads
}

#' Download data on document downloads for all pages in both the research
#' briefings view and the research briefings intranet view
#'
#' \code{fetch_rb_downloads} downloads data on document download metrics for
#' all research briefings on both the public Parliament website and the
#' Parliamentary intranet during the given dates and returns the data as a tibble.
#'
#' By default, download figures are reported separately for each website
#' property in Google Analytics that contains some of the requested data. You
#' can use the \code{combine} argument to optionally combine download figures
#' so that each result appears only once with figures totalled across all
#' relevant properties.
#'
#' Download figures can be requested by page. Google Analytics treats webpages
#' requested with different query strings and section anchors as different
#' pages in its downloads data. You can use the \code{merge_paths} argument to
#' optionally sum the figures for pages with the same base path.
#'
#' If \code{combine} and \code{merge_paths} are both set to TRUE, rows for
#' different properties are combined before paths are merged.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
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

fetch_rb_downloads <- function(
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    combine = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    fetch_rb_downloads_all_properties(
        fetch_rb_downloads_public,
        fetch_rb_downloads_intranet,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        by_date = by_date,
        by_page = by_page,
        combine = combine,
        merge_paths = merge_paths,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)
}

# Individual pages: Parliament website ----------------------------------------

#' Download data on document downloads for a research briefing with the given
#' URL on the main Parliament website
#'
#' \code{fetch_downloads_for_rb_public} downloads data on document download
#' metrics for a given research briefing url on the main Parliament website
#' during the given dates and returns the data as a tibble.
#'
#' By default, download figures are reported separately for each website
#' property in Google Analytics that contains some of the requested data. You
#' can use the \code{combine} argument to optionally combine download figures
#' so that each result appears only once with figures totalled across all
#' relevant properties.
#'
#' @param url The URL of a page for which downloads data is requested.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   downloads from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param combine A boolean indicating whether to combine the totals from
#'   different properties or to report them separately.The default is FALSE.
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

fetch_downloads_for_rb_public <- function(
    url,
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    combine = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

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
            fetch_events(
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

    if (combine) {

        downloads <- downloads %>% dplyr::mutate(property = LABEL_RB_PUBLIC)

        if (by_date) {
            downloads <- downloads %>%
                dplyr::group_by(.data$property, .data$date, .data$page_path)
        } else {
            downloads <- downloads %>%
                dplyr::group_by(.data$property, .data$page_path)
        }

        downloads <- downloads %>%
            dplyr::summarise(
                users = sum(.data$users),
                sessions = sum(.data$sessions),
                pageviews = sum(.data$pageviews),
                unique_pageviews = sum(.data$unique_pageviews)) %>%
            dplyr::ungroup()

    }

    label_events(downloads, "downloads")
}

# Individual pages: Intranet --------------------------------------------------

#' Download data on document downloads for a research briefing with the given
#' URL on the Parliamentary intranet
#'
#' \code{fetch_downloads_for_rb_intranet} downloads data on document download
#' metrics for a given research briefing url on the Parliamentary intranet
#' during the given dates and returns the data as a tibble.
#'
#' @param url The URL of a page for which downloads data is requested.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
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

fetch_downloads_for_rb_intranet <- function(
    url,
    start_date,
    end_date,
    by_date = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

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

    category_filter <- googleAnalyticsR::dim_filter(
        "eventCategory",
        "IN_LIST",
        CATEGORY_DOWNLOADS)

    filters <- list(path_filter, category_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    downloads <- fetch_events(
                view_id = VIEW_ID_RB_INTRANET,
                start_date = start_date,
                end_date = end_date,
                dimensions = dimensions,
                dim_filters = dim_filters,
                anti_sample = anti_sample,
                use_resource_quotas = use_resource_quotas) %>%
            dplyr::mutate(property = LABEL_RB_INTRANET) %>%
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

# Individual pages: Parliament website and intranet ---------------------------

#' Download data on document downloads for a research briefing with the given
#' URL on the main Parliament website and the Parliamentary intranet
#'
#' \code{fetch_downloads_for_rb} downloads data on document download metrics
#' for a given research briefing url on both the main Parliament website and
#' the Parliamentary intranet during the given dates and returns the data as a
#' tibble.
#'
#' By default, download figures are reported separately for each website
#' property in Google Analytics that contains some of the requested data. You
#' can use the \code{combine} argument to optionally combine download figures
#' so that each result appears only once with figures totalled across all
#' relevant properties.
#'
#' @param url The URL of a page for which downloads data is requested.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   downloads from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param combine A boolean indicating whether to combine the totals from
#'   different properties or to report them separately. The default is FALSE.
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

fetch_downloads_for_rb <- function(
    url,
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    combine = FALSE,
    anti_sample = FALSE,
    use_resource_quotas = FALSE) {

    public <- fetch_downloads_for_rb_public(
        url = url,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        by_date = by_date,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)

    intranet <- fetch_downloads_for_rb_intranet(
        url = url,
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        anti_sample = anti_sample,
        use_resource_quotas = use_resource_quotas)

    if (nrow(public) == 0 && nrow(intranet) == 0) return(tibble::tibble())

    if (combine) {

        downloads <- dplyr::bind_rows(public, intranet) %>%
            dplyr::mutate(property = LABEL_RB_ALL)

        if (by_date) {
            downloads <- downloads %>%
                dplyr::group_by(.data$property, .data$date, .data$page_path)
        } else {
            downloads <- downloads %>%
                dplyr::group_by(.data$property, .data$page_path)
        }

        downloads <- downloads %>%
            dplyr::summarise(
                users = sum(.data$users),
                sessions = sum(.data$sessions),
                pageviews = sum(.data$pageviews),
                unique_pageviews = sum(.data$unique_pageviews)) %>%
            dplyr::ungroup()

    } else {

        downloads <- dplyr::bind_rows(public, intranet)
    }

    downloads
}


