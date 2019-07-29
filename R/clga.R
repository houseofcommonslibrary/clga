#' clga: Download Traffic Data for Commons Library content from Google Analytics
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
