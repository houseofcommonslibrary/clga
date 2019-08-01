### Functions for downloading traffic data

#' Download traffic data for a given view
#'
#' \code{fetch_traffic} downloads data on traffic metrics for a given view
#' during the given dates and returns the data as a tibble.
#'
#' @param view_id The view id in Google Analytics
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param dimensions A vector of dimensions to return. The default is "date".
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @return A tibble of traffic metrics.
#' @keywords internal

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

# Parliament website ----------------------------------------------------------

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
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by individual page. The default is FALSE.
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_public <- function(
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

#' Download traffic data for all Commons briefing paper landing pages on the
#' main Parliamentary website
#'
#' \code{fetch_cbp_traffic_public} downloads data on traffic metrics for all
#' Commons Library briefing papers during the given dates and returns the data
#' as a tibble. These are the landing pages for briefings whose codes begin
#' with "SN" or "CBP".
#'
#' Note that this is not all Commons briefing paper traffic, as it does not
#' include traffic to the briefing paper pages on the Parliamentary intranet.
#' Use \code{fetch_cbp_traffic_intranet} to retrieve equivalent data for
#' the Parliamentary intranet.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_cbp_traffic_public <- function(
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE) {

    cbp_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "REGEXP",
        PATH_REGEXP_RB_CBP)

    network_filter <- googleAnalyticsR::dim_filter(
        "networkLocation",
        "REGEXP",
        NETWORK_REGEXP_INTERNAL)

    filters <- list(cbp_filter)
    if (internal) filters <- list(cbp_filter, network_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    fetch_rb_traffic_public(
        start_date = start_date,
        end_date = end_date,
        detailed = detailed,
        dim_filters = dim_filters)
}

#' Download traffic data for all Commons debate pack landing pages on the main
#' Parliamentary website
#'
#' \code{fetch_cdp_traffic_public} downloads data on traffic metrics for all
#' Commons Library debate packs during the given dates and returns the data as
#' a tibble. These are the landing pages for briefings whose codes begin with
#' "CDP".
#'
#' Note that this is not all Commons debate pack traffic, as it does not include
#' traffic to the debate pack pages on the Parliamentary intranet. Use
#' \code{fetch_cdp_traffic_intranet} to retrieve equivalent data for the
#' Parliamentary intranet.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_cdp_traffic_public <- function(
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE) {

    cdp_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "REGEXP",
        PATH_REGEXP_RB_CDP)

    network_filter <- googleAnalyticsR::dim_filter(
        "networkLocation",
        "REGEXP",
        NETWORK_REGEXP_INTERNAL)

    filters <- list(cdp_filter)
    if (internal) filters <- list(cdp_filter, network_filter)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        filters, operator = "AND")

    fetch_rb_traffic_public(
        start_date = start_date,
        end_date = end_date,
        detailed = detailed,
        dim_filters = dim_filters)
}

# Intranet --------------------------------------------------------------------

#' Download traffic data for all pages in the research briefings intranet view
#'
#' \code{fetch_rb_traffic_intranet} downloads data on traffic metrics for all
#' research briefings on the Parliamentary intranet during the given dates and
#' returns the data as a tibble.
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the main Parliament website. Use
#' \code{fetch_rb_traffic_public} to retrieve equivalent data for the main
#' Parliament website.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by individual page. The default is FALSE.
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @return A tibble of traffic metrics.
#' @export

fetch_rb_traffic_intranet <- function(
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

#' Download traffic data for all Commons briefing paper landing pages on
#' the Parliamentary intranet
#'
#' \code{fetch_cbp_traffic_intranet} downloads data on traffic metrics for all
#' Commons Library briefing papers on the intranet during the given dates and
#' returns the data as a tibble. These are the landing pages for briefings
#' whose codes begin with "SN" or "CBP".
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the main Parliament website. Use
#' \code{fetch_cbp_traffic_public} to retrieve equivalent data for the main
#' Parliament website.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_cbp_traffic_intranet <- function(
    start_date,
    end_date,
    detailed = FALSE) {

    cbp_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "REGEXP",
        PATH_REGEXP_RB_CBP)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        list(cbp_filter), operator = "AND")

    fetch_rb_traffic_intranet(
        start_date = start_date,
        end_date = end_date,
        detailed = detailed,
        dim_filters = dim_filters)
}

#' Download traffic data for all Commons debate pack landing pages on the
#' Parliamentary intranet
#'
#' \code{fetch_cdp_traffic_intranet} downloads data on traffic metrics for all
#' Commons Library debate packs on the intranet during the given dates and
#' returns the data as a tibble. These are the landing pages for briefings
#' whose codes begin with "CDP".
#'
#' Note that this is not all debate pack traffic, as it does not include
#' traffic to the debate pack pages on the main Parliament website. Use
#' \code{fetch_cdp_traffic_public} to retrieve equivalent data for the main
#' Parliament website.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_cdp_traffic_intranet <- function(
    start_date,
    end_date,
    detailed = FALSE) {

    cdp_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "REGEXP",
        PATH_REGEXP_RB_CDP)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        list(cdp_filter), operator = "AND")

    fetch_rb_traffic_intranet(
        start_date = start_date,
        end_date = end_date,
        detailed = detailed,
        dim_filters = dim_filters)
}

# Individual pages ------------------------------------------------------------

#' Download traffic data for a page with the given URL and view id
#'
#' \code{fetch_traffic_for_page} downloads data on traffic metrics for a given
#' url during the given dates and returns the data as a tibble.
#'
#' @param url The URL of a page.
#' @param view_id The view id in Google Analytics for the requested page.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @return A tibble of traffic metrics.
#' @keywords internal

fetch_traffic_for_page <- function(url, view_id, start_date, end_date) {

    page_path <- get_path(url)

    path_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "BEGINS_WITH",
        page_path)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(list(path_filter))

    fetch_traffic(
            view_id = view_id,
            start_date = start_date,
            end_date = end_date,
            dim_filters = dim_filters) %>%
        dplyr::mutate(page_path = page_path) %>%
        dplyr::select(.data$page_path, dplyr::everything())
}

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
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic_for_rb_public <- function(url, start_date, end_date) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    if (end < as.Date(DATE_START_RB_NEW)) {
        return(fetch_traffic_for_page(
            url,
            VIEW_ID_RB_OLD,
            start_date,
            end_date))
    }

    if (start > as.Date(DATE_END_RB_OLD)) {
        return(fetch_traffic_for_page(
            url,
            VIEW_ID_RB_NEW,
            start_date,
            end_date))
    }

    dplyr::bind_rows(
        fetch_traffic_for_page(
            url,
            VIEW_ID_RB_OLD,
            start_date,
            DATE_END_RB_OLD),
        fetch_traffic_for_page(
            url,
            VIEW_ID_RB_NEW,
            DATE_START_RB_NEW,
            end_date))
}

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

fetch_traffic_for_rb_intranet <- function(url, start_date, end_date) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)
    if (end < start) stop("The start_date is later than the end_date")

    return(fetch_traffic_for_page(
        url,
        VIEW_ID_RB_INTRANET,
        start_date,
        end_date))
}


