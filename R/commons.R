### Functions for downloading traffic data for Commons research briefings

# Sets of pages: Parliament website -------------------------------------------

#' Download traffic data for all Commons research briefing landing pages on the
#' main Parliamentary website
#'
#' \code{fetch_crb_traffic_public} downloads data on traffic metrics for all
#' Commons Library research briefings during the given dates and returns the
#' data as a tibble. These are the landing pages for briefings whose codes
#' begin with "SN", "CBP" or "CDP".
#'
#' Note that this is not all Commons research briefing traffic, as it does not
#' include traffic to the research briefing pages on the Parliamentary
#' intranet. Use \code{fetch_crb_traffic_intranet} to retrieve equivalent data
#' for the Parliamentary intranet.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param internal A boolean indicating whether to return only the results for
#'   traffic from internal parliamentary networks. The default is FALSE.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_crb_traffic_public <- function(
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE) {

    fetch_rb_traffic_public_by_type(
        type_regexp = PATH_REGEXP_RB_CRB,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        detailed = detailed)
}

#' Download traffic data for all Commons briefing paper landing pages on the
#' main Parliamentary website
#'
#' \code{fetch_cbp_traffic_public} downloads data on traffic metrics for all
#' Commons Library briefing papers during the given dates and returns the data
#' as a tibble. These are the landing pages for briefings whose ids begin
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

    fetch_rb_traffic_public_by_type(
        type_regexp = PATH_REGEXP_RB_CBP,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        detailed = detailed)
}

#' Download traffic data for all Commons debate pack landing pages on the main
#' Parliamentary website
#'
#' \code{fetch_cdp_traffic_public} downloads data on traffic metrics for all
#' Commons Library debate packs during the given dates and returns the data as
#' a tibble. These are the landing pages for briefings whose ids begin with
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

    fetch_rb_traffic_public_by_type(
        type_regexp = PATH_REGEXP_RB_CDP,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        detailed = detailed)
}

# Sets of pages: Intranet -----------------------------------------------------

#' Download traffic data for all Commons research briefing landing pages on
#' the Parliamentary intranet
#'
#' \code{fetch_crb_traffic_intranet} downloads data on traffic metrics for all
#' Commons Library research briefings on the intranet during the given dates
#' and returns the data as a tibble. These are the landing pages for briefings
#' whose ids begin with "SN", "CBP", or "CDP".
#'
#' Note that this is not all research briefings traffic, as it does not include
#' traffic to the research briefings pages on the main Parliament website. Use
#' \code{fetch_crb_traffic_public} to retrieve equivalent data for the main
#' Parliament website.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_crb_traffic_intranet <- function(
    start_date,
    end_date,
    detailed = FALSE) {

    fetch_rb_traffic_intranet_by_type(
        type_regexp = PATH_REGEXP_RB_CRB,
        start_date = start_date,
        end_date = end_date,
        detailed = detailed)
}

#' Download traffic data for all Commons briefing paper landing pages on
#' the Parliamentary intranet
#'
#' \code{fetch_cbp_traffic_intranet} downloads data on traffic metrics for all
#' Commons Library briefing papers on the intranet during the given dates and
#' returns the data as a tibble. These are the landing pages for briefings
#' whose ids begin with "SN" or "CBP".
#'
#' Note that this is not all Commons briefing paper traffic, as it does not
#' include traffic to the briefing papers on the main Parliament website. Use
#' \code{fetch_cbp_traffic_public} to retrieve equivalent data for the main
#' Parliament website.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_cbp_traffic_intranet <- function(
    start_date,
    end_date,
    detailed = FALSE) {

    fetch_rb_traffic_intranet_by_type(
        type_regexp = PATH_REGEXP_RB_CBP,
        start_date = start_date,
        end_date = end_date,
        detailed = detailed)
}

#' Download traffic data for all Commons debate pack landing pages on the
#' Parliamentary intranet
#'
#' \code{fetch_cdp_traffic_intranet} downloads data on traffic metrics for all
#' Commons Library debate packs on the intranet during the given dates and
#' returns the data as a tibble. These are the landing pages for briefings
#' whose ids begin with "CDP".
#'
#' Note that this is not all debate pack traffic, as it does not include
#' traffic to the debate pack pages on the main Parliament website. Use
#' \code{fetch_cdp_traffic_public} to retrieve equivalent data for the main
#' Parliament website.
#'
#' @param start_date The start date as an ISO 8601 string.
#' @param end_date The end date as an ISO 8601 string.
#' @param detailed A boolean indicating whether to return the results by date
#'   in total, or broken down by date and page. The default is FALSE.
#' @return A tibble of traffic metrics.
#' @export

fetch_cdp_traffic_intranet <- function(
    start_date,
    end_date,
    detailed = FALSE) {

    fetch_rb_traffic_intranet_by_type(
        type_regexp = PATH_REGEXP_RB_CDP,
        start_date = start_date,
        end_date = end_date,
        detailed = detailed)
}

# Sets of pages: All research briefings (Parliament website and Intranet) -----

#' Download traffic data for all Commons research briefings in both the
#' research briefings view and the research briefings intranet view
#'
#' \code{fetch_crb_traffic} downloads data on traffic metrics for all Commons
#' research briefings on both the public Parliament website and the
#' Parliamentary intranet during the given dates and returns the data as a
#' tibble.
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

fetch_crb_traffic <- function(
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE,
    combined = TRUE) {

    fetch_rb_traffic_all_sources(
        fetch_crb_traffic_public,
        fetch_crb_traffic_intranet,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        detailed = detailed,
        combined = combined)
}

#' Download traffic data for all Commons briefing papers in both the
#' research briefings view and the research briefings intranet view
#'
#' \code{fetch_cbp_traffic} downloads data on traffic metrics for all Commons
#' briefing papers on both the public Parliament website and the Parliamentary
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

fetch_cbp_traffic <- function(
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE,
    combined = TRUE) {

    fetch_rb_traffic_all_sources(
        fetch_cbp_traffic_public,
        fetch_cbp_traffic_intranet,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        detailed = detailed,
        combined = combined)
}

#' Download traffic data for all Commons debate packs in both the research
#' briefings view and the research briefings intranet view
#'
#' \code{fetch_cdp_traffic} downloads data on traffic metrics for all Commons
#' debate packs on both the public Parliament website and the Parliamentary
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

fetch_cdp_traffic <- function(
    start_date,
    end_date,
    internal = FALSE,
    detailed = FALSE,
    combined = TRUE) {

    fetch_rb_traffic_all_sources(
        fetch_cdp_traffic_public,
        fetch_cdp_traffic_intranet,
        start_date = start_date,
        end_date = end_date,
        internal = internal,
        detailed = detailed,
        combined = combined)
}
