.onLoad <- function(libname, pkgname) {
    if (file.exists(".httr-oauth")) googleAnalyticsR::ga_auth(".httr-oauth")
}
