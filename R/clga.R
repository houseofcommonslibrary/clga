#' clga: Download Traffic Data for Commons Library content from Google Analytics
#'
#' This package contains a suite of functions for downloading data on traffic
#' to research briefing landing pages and to the Commons Library website for
#' research products produced by the House of Commons Library.
#'
#' @docType package
#' @name clga
#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

# Tell R CMD check about new operators
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", ":="))
}
