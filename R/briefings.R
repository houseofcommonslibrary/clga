### Functions for downloading traffic data for research briefings

# Sets of pages: Parliament website -------------------------------------------

#' Download traffic data for all pages in the research briefings view with the
#' given filters
#'
#' \code{fetch_rb_traffic_public_by_filter} downloads data on traffic metrics
#' for all research briefings on the public Parliament website during the given
#' dates, with the given filters, and returns the data as a tibble.
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the Parliamentary intranet. Use
#' \code{fetch_rb_traffic_intranet_by_filter} to retrieve equivalent data for
#' the Parliamentary intranet.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate metrics for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query string. This parameter is ignored if \code{by_page} is
#'   set to FALSE. Note that while merging paths is necessary for analysis of
#'   individual pages it can introduce small errors in the number of users by
#'   page, as the same user may visit the same page through URLs with different
#'   query strings. The default value is FALSE.
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_public_by_filter <- function(
    start_date,
    end_date,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    dim_filters = NULL,
    anti_sample = FALSE) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    dimensions <- c()
    if (by_date) dimensions <- c(dimensions, "date")
    if (by_page) dimensions <- c(dimensions, "pagePath")

    if (end < as.Date(DATE_START_RB_NEW)) {

        traffic <- fetch_traffic(
            view_id = VIEW_ID_RB_OLD,
            start_date = start_date,
            end_date = end_date,
            dimensions = dimensions,
            dim_filters = dim_filters,
            anti_sample = anti_sample)

    } else if (start > as.Date(DATE_END_RB_OLD)) {

        traffic <- fetch_traffic(
            view_id = VIEW_ID_RB_NEW,
            start_date = start_date,
            end_date = end_date,
            dimensions = dimensions,
            dim_filters = dim_filters,
            anti_sample = anti_sample)

    } else {

        traffic <- dplyr::bind_rows(
            fetch_traffic(
                view_id = VIEW_ID_RB_OLD,
                start_date = start_date,
                end_date = DATE_END_RB_OLD,
                dimensions = dimensions,
                dim_filters = dim_filters,
                anti_sample = anti_sample),
            fetch_traffic(
                view_id = VIEW_ID_RB_NEW,
                start_date = DATE_START_RB_NEW,
                end_date = end_date,
                dimensions = dimensions,
                dim_filters = dim_filters,
                anti_sample = anti_sample))
    }

    if (by_page && merge_paths) traffic <- merge_paths(traffic, by_date)
    traffic
}

#' Download traffic data for different types of Commons research briefings on
#' the main Parliamentary website
#'
#' \code{fetch_rb_traffic_public_by_type} downloads data on traffic metrics for
#' specific types of research breifing based on their id prefixes during the
#' given dates and returns the data as a tibble. The specific types of
#' briefings to return are defined with a regular expression that identifies
#' their id prefixes within the page path.
#'
#' Note that this function only returns data for the main Parliament website.
#' Use \code{fetch_rb_traffic_intranet_by_type} to retrieve equivalent data
#' for the Parliamentary intranet.
#'
#' @param type_regexp That regular expression that describes the page path for
#'   one or more briefing types.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
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

fetch_rb_traffic_public_by_type <- function(
    type_regexp,
    start_date,
    end_date,
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

    fetch_rb_traffic_public_by_filter(
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        dim_filters = dim_filters,
        anti_sample = anti_sample)
}

#' Download traffic data for all pages in the research briefings view
#'
#' \code{fetch_rb_traffic_public} downloads data on traffic metrics for all
#' research briefings on the public Parliament website during the given dates
#' and returns the data as a tibble.
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the Parliamentary intranet. Use
#' \code{fetch_rb_traffic_intranet} to retrieve equivalent data for the
#' Parliamentary intranet.

#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
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

fetch_rb_traffic_public <- function(
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE) {

    fetch_rb_traffic_public_by_type(
        type_regexp = PATH_REGEXP_ALL,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        anti_sample = anti_sample)
}

# Sets of pages: Intranet -----------------------------------------------------

#' Download traffic data for all pages in the research briefings intranet view
#' with the given filters
#'
#' \code{fetch_rb_traffic_intranet_by_filter} downloads data on traffic metrics
#' for all research briefings on the Parliamentary intranet during the given
#' dates, with the given filters, and returns the data as a tibble.
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the main Parliament website. Use
#' \code{fetch_rb_traffic_public_by_filter} to retrieve equivalent data for
#' the main Parliament website.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate metrics for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query string. This parameter is ignored if \code{by_page} is
#'   set to FALSE. Note that while merging paths is necessary for analysis of
#'   individual pages it can introduce small errors in the number of users by
#'   page, as the same user may visit the same page through URLs with different
#'   query strings. The default value is FALSE.
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_intranet_by_filter <- function(
    start_date,
    end_date,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    dim_filters = NULL,
    anti_sample = FALSE) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    dimensions <- c()
    if (by_date) dimensions <- c(dimensions, "date")
    if (by_page) dimensions <- c(dimensions, "pagePath")

    traffic <- fetch_traffic(
        view_id = VIEW_ID_RB_INTRANET,
        start_date = start_date,
        end_date = end_date,
        dimensions = dimensions,
        dim_filters = dim_filters,
        anti_sample = anti_sample)

    if (by_page && merge_paths) traffic <- merge_paths(traffic, by_date)
    traffic
}

#' Download traffic data for different types of Commons research briefings on
#' the Parliamentary intranet
#'
#' \code{fetch_rb_traffic_intranet_by_type} downloads data on traffic metrics
#' for specific types of research breifing based on their id prefixes during
#' the given dates and returns the data as a tibble. The specific types of
#' briefings to return are defined with a regular expression that identifies
#' their id prefixes within the page path.
#'
#' Note that this function only returns data for the Parliamentary intranet.
#' Use \code{fetch_rb_traffic_public_by_type} to retrieve equivalent data
#' for the main Parliamentary website.
#'
#' @param type_regexp That regular expression that describes the page path for
#'   one or more briefing types.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
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

fetch_rb_traffic_intranet_by_type <- function(
    type_regexp,
    start_date,
    end_date,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE) {

    type_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "REGEXP",
        type_regexp)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        list(type_filter), operator = "AND")

    fetch_rb_traffic_intranet_by_filter(
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        dim_filters = dim_filters,
        anti_sample = anti_sample)
}

#' Download traffic data for all pages in the research briefings intranet view
#'
#' \code{fetch_rb_traffic_intranet_by_filter} downloads data on traffic metrics
#' for all research briefings on the Parliamentary intranet during the given
#' dates, with the given filters, and returns the data as a tibble.
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the main Parliament website. Use
#' \code{fetch_rb_traffic_public} to retrieve equivalent data for the main
#' Parliament website.

#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
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

fetch_rb_traffic_intranet <- function(
    start_date,
    end_date,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    anti_sample = FALSE) {

    fetch_rb_traffic_intranet_by_type(
        type_regexp = PATH_REGEXP_ALL,
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        anti_sample = anti_sample)
}

# Sets of pages: All research briefings ---------------------------------------

#' Download traffic data for pages in both the research briefings view and the
#' research briefings intranet view using the given functions for each view
#'
#' \code{fetch_rb_traffic_all_sources} downloads data on traffic metrics for
#' the same set of research briefings on both the public Parliament website and
#' the Parliamentary intranet during the given dates and returns the data as a
#' tibble. You must provide matching fetch_* functions for each view.
#'
#' The data can either be combined so that each result appears once with
#' totals across both the Parliament website and the intranet, or reported
#' separately with separate rows for the website and the intranet.
#'
#' @param fetch_public A \code{fetch_*_public} function for a set of briefings.
#' @param fetch_intranet A \code{fetch_*_intranet} function for a set of
#'   briefings. For this function to behave as expected \code{fetch_public} and
#'   \code{fetch_intranet} should target the same set of briefings in each view.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate metrics for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query string. This parameter is ignored if \code{by_page} is
#'   set to FALSE. Note that while merging paths is necessary for analysis of
#'   individual pages it can introduce small errors in the number of users by
#'   page, as the same user may visit the same page through URLs with different
#'   query strings. The default value is FALSE.
#' @param combined A boolean indicating whether to combine the totals from
#'   the website and the intranet or to report them separately. Note that
#'   combining the traffic across both properties can introduce errors in
#'   the number of users, as the same user may visit pages on both properties.
#'   The default is FALSE.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_all_sources <- function(
    fetch_public,
    fetch_intranet,
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    combined = FALSE,
    anti_sample = FALSE) {

    public <- fetch_public(
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        anti_sample = anti_sample)

    intranet <- fetch_intranet(
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        anti_sample = anti_sample)

    if (combined) {

        if (by_date) {

            if (by_page) {
                all <- dplyr::bind_rows(public, intranet) %>%
                    dplyr::group_by(.data$date, .data$page_path)
            } else {
                all <- dplyr::bind_rows(public, intranet) %>%
                    dplyr::group_by(.data$date)
            }

        } else {

            if (by_page) {
                all <- dplyr::bind_rows(public, intranet) %>%
                    dplyr::group_by(.data$page_path)
            } else {
                all <- dplyr::bind_rows(public, intranet) %>%
                    dplyr::mutate(website = "combined") %>%
                    dplyr::group_by(.data$website)
            }
        }

        all %>% dplyr::summarise(
                users = sum(.data$users),
                sessions = sum(.data$sessions),
                pageviews = sum(.data$pageviews),
                unique_pageviews = sum(.data$unique_pageviews)) %>%
            dplyr::ungroup()

    } else {

        public$website <- LABEL_PUBLIC
        intranet$website <- LABEL_INTRANET

        if (by_date) {

            if (by_page) {
                dplyr::bind_rows(public, intranet) %>%
                    dplyr::select(
                        .data$date,
                        .data$page_path,
                        .data$website,
                        dplyr::everything())
            } else {
                dplyr::bind_rows(public, intranet) %>%
                    dplyr::select(
                        .data$date,
                        .data$website,
                        dplyr::everything())
            }

        } else {

            if (by_page) {
                dplyr::bind_rows(public, intranet) %>%
                    dplyr::select(
                        .data$page_path,
                        .data$website,
                        dplyr::everything())
            } else {
                dplyr::bind_rows(public, intranet) %>%
                    dplyr::select(
                        .data$website,
                        dplyr::everything())
            }
        }
    }
}

#' Download traffic data for all pages in both the research briefings view
#' and the research briefings intranet view
#'
#' \code{fetch_rb_traffic} downloads data on traffic metrics for all research
#' briefings on both the public Parliament website and the Parliamentary
#' intranet during the given dates and returns the data as a tibble.
#'
#' The data can either be combined so that each result appears once with
#' totals across both the Parliament website and the intranet, or reported
#' separately with separate rows for the website and the intranet.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param by_page A boolean indicating whether to return the results broken
#'   down by page. The default is FALSE.
#' @param merge_paths A boolean indicating whether to aggregate metrics for all
#'   pages that have the same root path i.e. for all pages whose paths differ
#'   only by their query string. This parameter is ignored if \code{by_page} is
#'   set to FALSE. Note that while merging paths is necessary for analysis of
#'   individual pages it can introduce small errors in the number of users by
#'   page, as the same user may visit the same page through URLs with different
#'   query strings. The default value is FALSE.
#' @param combined A boolean indicating whether to combine the totals from
#'   the website and the intranet or to report them separately. Note that
#'   combining the traffic across both properties can introduce errors in
#'   the number of users, as the same user may visit pages on both properties.
#'   The default is FALSE.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic <- function(
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    by_page = FALSE,
    merge_paths = FALSE,
    combined = FALSE,
    anti_sample = FALSE) {

    fetch_rb_traffic_all_sources(
        fetch_rb_traffic_public,
        fetch_rb_traffic_intranet,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        by_date = by_date,
        by_page = by_page,
        merge_paths = merge_paths,
        combined = combined,
        anti_sample = anti_sample)
}

# Individual pages: Parliament website ----------------------------------------

#' Download traffic data for a research briefing with the given URL on the main
#' Parliament website
#'
#' \code{fetch_traffic_for_rb_public} downloads data on traffic metrics for a
#' given research briefing url on the main Parliament website during the given
#' dates and returns the data as a tibble.
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

fetch_traffic_for_rb_public <- function(
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

    if (end < as.Date(DATE_START_RB_NEW)) {

        traffic <- fetch_traffic(
            view_id = VIEW_ID_RB_OLD,
            start_date = start_date,
            end_date = end_date,
            dimensions = dimensions,
            dim_filters = dim_filters,
            anti_sample = anti_sample)

    } else if (start > as.Date(DATE_END_RB_OLD)) {

        traffic <- fetch_traffic(
            view_id = VIEW_ID_RB_NEW,
            start_date = start_date,
            end_date = end_date,
            dimensions = dimensions,
            dim_filters = dim_filters,
            anti_sample = anti_sample)

    } else {

        traffic <- dplyr::bind_rows(
            fetch_traffic(
                view_id = VIEW_ID_RB_OLD,
                start_date = start_date,
                end_date = DATE_END_RB_OLD,
                dimensions = dimensions,
                dim_filters = dim_filters,
                anti_sample = anti_sample),
            fetch_traffic(
                view_id = VIEW_ID_RB_NEW,
                start_date = DATE_START_RB_NEW,
                end_date = end_date,
                dimensions = dimensions,
                dim_filters = dim_filters,
                anti_sample = anti_sample))
    }

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

# Individual pages: Intranet --------------------------------------------------

#' Download traffic data for a research briefing with the given URL on the
#' Parliamentary intranet
#'
#' \code{fetch_traffic_for_rb_intranet} downloads data on traffic metrics for a
#' given research briefing url on the main Parliament website during the
#' given dates and returns the data as a tibble.
#'
#' @param url The URL of a page for which traffic data is requested.
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
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic_for_rb_intranet <- function(
    url,
    start_date,
    end_date,
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

    filters <- list(path_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    traffic <- fetch_traffic(
            view_id = VIEW_ID_RB_INTRANET,
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

# Individual pages: Parliament website and intranet ---------------------------

#' Download traffic data for a research briefing with the given URL on the main
#' Parliament website and the Parliamentary intranet
#'
#' \code{fetch_traffic_for_rb} downloads data on traffic metrics for a given
#' research briefing url on both the main Parliament website and the
#' Parliamentary intranet during the given dates and returns the data as a
#' tibble.
#'
#' The data can either be combined so that each result appears once with
#' totals across both the Parliament website and the intranet, or reported
#' separately with separate rows for the website and the intranet.
#'
#' @param url The URL of a page for which traffic data is requested.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param by_date A boolean indicating whether to return the results broken
#'   down by date. The default is FALSE.
#' @param combined A boolean indicating whether to combine the totals from
#'   the website and the intranet or to report them separately. Note that
#'   combining the traffic across both properties can introduce errors in
#'   the number of users, as the same user may visit pages on both properties.
#'   The default is FALSE.
#' @param anti_sample A boolean indicating whether to use googleAnalyticsR's
#'   anti-sample feature, which chunks API calls to keep the number of records
#'   requested under the API limits that trigger sampling. This makes the
#'   download process slower but ensures that all records are returned. Only
#'   use this feature if you see that an API request triggers sampling without
#'   it. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic_for_rb <- function(
    url,
    start_date,
    end_date,
    internal = FALSE,
    by_date = FALSE,
    combined = FALSE,
    anti_sample = FALSE) {

    public <- fetch_traffic_for_rb_public(
        url = url,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        by_date = by_date,
        anti_sample = anti_sample)

    intranet <- fetch_traffic_for_rb_intranet(
        url = url,
        start_date = start_date,
        end_date = end_date,
        by_date = by_date,
        anti_sample = anti_sample)

    if (nrow(public) == 0 && nrow(intranet) == 0) return(tibble::tibble())

    if (combined) {

        traffic <- dplyr::bind_rows(public, intranet)

        if (by_date) {
            traffic <- traffic %>% dplyr::group_by(.data$date, .data$page_path)
        } else {
            traffic <- traffic %>%  dplyr::group_by(.data$page_path)
        }

        traffic <- traffic %>%
            dplyr::summarise(
                users = sum(.data$users),
                sessions = sum(.data$sessions),
                pageviews = sum(.data$pageviews),
                unique_pageviews = sum(.data$unique_pageviews)) %>%
            dplyr::ungroup()

    } else {

        if (nrow(public) > 0) public$website <- LABEL_PUBLIC
        if (nrow(intranet) > 0) intranet$website <- LABEL_INTRANET
        traffic <- dplyr::bind_rows(public, intranet)

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
    }

    traffic
}
