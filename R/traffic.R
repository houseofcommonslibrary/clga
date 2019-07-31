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

#' Download traffic data for all pages in the research briefings view
#'
#' \code{fetch_briefings_traffic} downloads data on traffic metrics for all
#' research briefings under the research briefings view during the given dates
#' and returns the data as a tibble.
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the Parliamentary intranet.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by individual page. The default is FALSE.
#' @param dim_filters A set of dimension filters to constrain the results. The
#'   default is NULL.
#' @return A tibble of traffic metrics.
#' @export

fetch_briefings_traffic <- function(
    start_date,
    end_date,
    detailed = FALSE,
    dim_filters = NULL) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)

    dimensions <- c("date")
    if(detailed) dimensions <- c("date", "pagePath")

    if (end < start) {
        stop("The start_date is later than the end_date")
    }

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

#' Download traffic data for all Commons research briefings landing pages
#'
#' \code{fetch_commons_research_briefings_traffic} downloads data on traffic
#' metrics for all Commons Library research briefings during the given dates
#' and returns the data as a tibble. These are the landing pages for briefings
#' whose codes begin with "SN" or "CBP".
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the Parliamentary intranet.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by individual page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_commons_research_briefings_traffic <- function(
    start_date,
    end_date,
    detailed = FALSE) {

    sn_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "BEGINS_WITH",
        PATH_PREFIX_RB_SN)

    cbp_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "BEGINS_WITH",
        PATH_PREFIX_RB_CBP)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(
        list(sn_filter, cbp_filter), operator = "OR")

    fetch_briefings_traffic(
        start_date = start_date,
        end_date = end_date,
        detailed = detailed,
        dim_filters = dim_filters)
}

#' Download traffic data for all Commons debate pacj landing pages
#'
#' \code{fetch_commons_debate_pack_traffic} downloads data on traffic
#' metrics for all Commons Library debate packs during the given dates
#' and returns the data as a tibble. These are the landing pages for briefings
#' whose codes begin with "CDP".
#'
#' Note that this is not all debate pack traffic, as it does not include
#' traffic to the debate pack pages on the Parliamentary intranet.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by individual page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_commons_debate_pack_traffic <- function(
    start_date,
    end_date,
    detailed = FALSE) {

    cdp_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "BEGINS_WITH",
        PATH_PREFIX_RB_CDP)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(list(cdp_filter))

    fetch_briefings_traffic(
        start_date = start_date,
        end_date = end_date,
        detailed = detailed,
        dim_filters = dim_filters)
}

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

#' Download traffic data for a research briefing with the given URL
#'
#' \code{fetch_traffic_for_briefing} downloads data on traffic metrics for a
#' given research briefing url during the given dates and returns the data as a
#' tibble.
#'
#' @param url The URL of a page for which traffic data is requested.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @return A tibble of traffic metrics.
#' @export

fetch_traffic_for_briefing <- function(url, start_date, end_date) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)

    if (end < start) {
        stop("The start_date is later than the end_date")
    }

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


