.onLoad <- function(libname, pkgname) {
    if (file.exists(".httr-oauth")) googleAnalyticsR::ga_auth(".httr-oauth")
    if (file.exists(file.path("auth", "auth.json"))) googleAuthR::gar_auth_service(file.path("auth", "auth.json"))
}
