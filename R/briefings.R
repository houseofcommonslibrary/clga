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
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by individual page. The default is FALSE.
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_public_by_filter <- function(
    start_date,
    end_date,
    detailed = FALSE,
    dim_filters = NULL) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    dimensions <- c("date")
    if(detailed) dimensions <- c("date", "pagePath")

    if (end < as.Date(DATE_START_RB_NEW)) {
        return(fetch_traffic(
            view_id = VIEW_ID_RB_OLD,
            start_date = start_date,
            end_date = end_date,
            dimensions = dimensions,
            dim_filters = dim_filters))
    }

    if (start > as.Date(DATE_END_RB_OLD)) {
        return(fetch_traffic(
            view_id = VIEW_ID_RB_NEW,
            start_date = start_date,
            end_date = end_date,
            dimensions = dimensions,
            dim_filters = dim_filters))
    }

    dplyr::bind_rows(
        fetch_traffic(
            view_id = VIEW_ID_RB_OLD,
            start_date = start_date,
            end_date = DATE_END_RB_OLD,
            dimensions = dimensions,
            dim_filters = dim_filters),
        fetch_traffic(
            view_id = VIEW_ID_RB_NEW,
            start_date = DATE_START_RB_NEW,
            end_date = end_date,
            dimensions = dimensions,
            dim_filters = dim_filters))
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
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_public_by_type <- function(
    type_regexp,
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE) {

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
        detailed = detailed,
        dim_filters = dim_filters)
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
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_public <- function(
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE) {

    fetch_rb_traffic_public_by_type(
        type_regexp = PATH_REGEXP_RB_ALL,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        detailed = detailed)
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
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by individual page. The default is FALSE.
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_intranet_by_filter <- function(
    start_date,
    end_date,
    detailed = FALSE,
    dim_filters = NULL) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    dimensions <- c("date")
    if(detailed) dimensions <- c("date", "pagePath")

    return(fetch_traffic(
        view_id = VIEW_ID_RB_INTRANET,
        start_date = start_date,
        end_date = end_date,
        dimensions = dimensions,
        dim_filters = dim_filters))
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
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_intranet_by_type <- function(
    type_regexp,
    start_date,
    end_date,
    detailed = FALSE) {

    type_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "REGEXP",
        type_regexp)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        list(type_filter), operator = "AND")

    fetch_rb_traffic_intranet_by_filter(
        start_date = start_date,
        end_date = end_date,
        detailed = detailed,
        dim_filters = dim_filters)
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
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_intranet <- function(
    start_date,
    end_date,
    detailed = FALSE) {

    fetch_rb_traffic_intranet_by_type(
        type_regexp = PATH_REGEXP_RB_ALL,
        start_date = start_date,
        end_date = end_date,
        detailed = detailed)
}

# Sets of pages: All research briefings (Parliament website and Intranet) -----

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
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by individual page. The default is FALSE.
#' @param combined A boolean indicating whether to combine the totals from
#'   the website and the intranet or to report them separately. The default is
#'   TRUE.
#' @return A tibble of traffic metrics.
#' @keywords internal

fetch_rb_traffic_all_sources <- function(
    fetch_public,
    fetch_intranet,
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE,
    combined = TRUE) {

    public <- fetch_public(
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        detailed = detailed)

    intranet <- fetch_intranet(
        start_date = start_date,
        end_date = end_date,
        detailed = detailed)

    if (combined) {

        if (detailed) {
            all <- dplyr::bind_rows(public, intranet) %>%
                dplyr::group_by(.data$date, .data$page_path)
        } else {
            all <- dplyr::bind_rows(public, intranet) %>%
                dplyr::group_by(.data$date)
        }

        all %>% dplyr::summarise(
            users = sum(.data$users),
            sessions = sum(.data$sessions),
            pageviews = sum(.data$pageviews),
            upageviews = sum(.data$upageviews))

    } else {

        public$website <- LABEL_PUBLIC
        intranet$website <- LABEL_INTRANET

        if (detailed) {
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
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by individual page. The default is FALSE.
#' @param combined A boolean indicating whether to combine the totals from
#'   the website and the intranet or to report them separately. The default is
#'   TRUE.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic <- function(
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE,
    combined = TRUE) {

    fetch_rb_traffic_all_sources(
        fetch_rb_traffic_public,
        fetch_rb_traffic_intranet,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        detailed = detailed,
        combined = combined)
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
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic_for_rb_public <- function(
    url,
    start_date,
    end_date,
    internal = FALSE) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    page_path <- get_path(url)

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
            dim_filters = dim_filters)

    } else if (start > as.Date(DATE_END_RB_OLD)) {

        traffic <- fetch_traffic(
            view_id = VIEW_ID_RB_NEW,
            start_date = start_date,
            end_date = end_date,
            dim_filters = dim_filters)

    } else {

        traffic <- dplyr::bind_rows(
            fetch_traffic(
                view_id = VIEW_ID_RB_OLD,
                start_date = start_date,
                end_date = DATE_END_RB_OLD,
                dim_filters = dim_filters),
            fetch_traffic(
                view_id = VIEW_ID_RB_NEW,
                start_date = DATE_START_RB_NEW,
                end_date = end_date,
                dim_filters = dim_filters))
    }

    traffic %>%
        dplyr::mutate(page_path = page_path) %>%
        dplyr::select(
            .data$date,
            .data$page_path,
            dplyr::everything())
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
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic_for_rb_intranet <- function(
    url,
    start_date,
    end_date) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    page_path <- get_path(url)

    path_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "BEGINS_WITH",
        page_path)

    filters <- list(path_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    fetch_traffic(
            view_id = VIEW_ID_RB_INTRANET,
            start_date = start_date,
            end_date = end_date,
            dim_filters = dim_filters) %>%
        dplyr::mutate(page_path = page_path) %>%
        dplyr::select(
            .data$date,
            .data$page_path,
            dplyr::everything())
}

# Individual pages: All research briefings (Parliament website and Intranet) ---

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
#' @param combined A boolean indicating whether to combine the totals from
#'   the website and the intranet or to report them separately. The default is
#'   TRUE.
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic_for_rb <- function(
    url,
    start_date,
    end_date,
    internal = FALSE,
    combined = TRUE) {

    public <- fetch_traffic_for_rb_public(
        url = url,
        start_date = start_date,
        end_date = end_date,
        internal = internal)

    intranet <- fetch_traffic_for_rb_intranet(
        url = url,
        start_date = start_date,
        end_date = end_date)

    if (combined) {

        dplyr::bind_rows(public, intranet) %>%
            dplyr::group_by(.data$date, .data$page_path) %>%
            dplyr::summarise(
                users = sum(.data$users),
                sessions = sum(.data$sessions),
                pageviews = sum(.data$pageviews),
                upageviews = sum(.data$upageviews))

    } else {

        public$website <- LABEL_PUBLIC
        intranet$website <- LABEL_INTRANET

        dplyr::bind_rows(public, intranet) %>%
            dplyr::select(
                .data$date,
                .data$page_path,
                .data$website,
                dplyr::everything())
    }

}
