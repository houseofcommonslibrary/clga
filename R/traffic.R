### Functions for downloading traffic data

#' Download traffic data for a given view
#'
#' \code{fetch_traffic} downloads data showing the number of users,
#' sessions, and pageviews for a given view during the given dates and returns
#' the data as a tibble.
#'
#' @param view_id The view id in Google Analytics
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param dim_filters A set of dimension filters to constrain the results.
#' @return A tibble of users, sessions, and pageviews by date.
#' @keywords internal

fetch_traffic <- function(
    view_id,
    start_date,
    end_date,
    dim_filters = NULL) {

    googleAnalyticsR::google_analytics(
            view_id,
            date_range = c(start_date, end_date),
            metrics = c("users", "sessions", "pageviews"),
            dimensions = "date",
            dim_filters = dim_filters,
            max = -1) %>%
        tibble::as_tibble()
}

#' Download traffic data for all pages in the research briefings view
#'
#' \code{fetch_briefings_traffic} downloads data showing the number of users,
#' sessions, and pageviews for all research briefings under the research
#' briefings view during the given dates and returns the data as a tibble.
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the Parliamentary intranet.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param dim_filters A set of dimension filters to constrain the results.
#' @return A tibble of users, sessions, and pageviews by date.
#' @export

fetch_briefings_traffic <- function(
    start_date,
    end_date,
    dim_filters = NULL) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)

    if (end < start) {
        stop("The start_date is later than the end_date")
    }

    if (end < as.Date(DATE_START_RB_NEW)) {
        return(fetch_traffic(
            VIEW_ID_RB_OLD,
            start_date,
            end_date,
            dim_filters))
    }

    if (start > as.Date(DATE_END_RB_OLD)) {
        return(fetch_traffic(
            VIEW_ID_RB_NEW,
            start_date,
            end_date,
            dim_filters))
    }

    dplyr::bind_rows(
        fetch_traffic(
            VIEW_ID_RB_OLD,
            start_date,
            DATE_END_RB_OLD,
            dim_filters),
        fetch_traffic(
            VIEW_ID_RB_NEW,
            DATE_START_RB_NEW,
            end_date,
            dim_filters))
}

fetch_commons_research_briefings_traffic <- function(start_date, end_date) {

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

    fetch_briefings_traffic(start_date, end_date, dim_filters)
}

#' Download traffic data for a page with the given URL and view id
#'
#' \code{fetch_page_traffic} downloads data showing the number of users,
#' sessions, and pageviews for a given url during the given dates and returns
#' the data as a tibble.
#'
#' @param url The URL of a page.
#' @param view_id The view id in Google Analytics for the requested page.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @return A tibble of users, sessions, and pageviews by date.
#' @keywords internal

fetch_page_traffic <- function(url, view_id, start_date, end_date) {

    page_path <- get_path(url)

    path_filter <- googleAnalyticsR::dim_filter(
        "pagePath",
        "EXACT",
        page_path)

    dim_filters <- googleAnalyticsR::filter_clause_ga4(list(path_filter))

    fetch_traffic(view_id, start_date, end_date, dim_filters) %>%
        dplyr::mutate(page_path = page_path) %>%
        dplyr::select(.data$page_path, dplyr::everything())
}

#' Download traffic data for a research briefing with the given URL
#'
#' \code{fetch_briefing_traffic} downloads data showing the number of users,
#' sessions, and pageviews for a given research briefing url during the given
#' dates and returns the data as a tibble.
#'
#' @param url The URL of a page for which traffic data is requested.
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @return A tibble of users, sessions, and pageviews by date.
#' @export

fetch_briefing_traffic <- function(url, start_date, end_date) {

    start <- as.Date(start_date)
    end <- as.Date(end_date)

    if (end < start) {
        stop("The start_date is later than the end_date")
    }

    if (end < as.Date(DATE_START_RB_NEW)) {
        return(fetch_page_traffic(url, VIEW_ID_RB_OLD, start_date, end_date))
    }

    if (start > as.Date(DATE_END_RB_OLD)) {
        return(fetch_page_traffic(url, VIEW_ID_RB_NEW, start_date, end_date))
    }

    dplyr::bind_rows(
        fetch_page_traffic(url, VIEW_ID_RB_OLD, start_date, DATE_END_RB_OLD),
        fetch_page_traffic(url, VIEW_ID_RB_NEW, DATE_START_RB_NEW, end_date))
}


