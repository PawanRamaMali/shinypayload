#' Get URL query parameters in Shiny
#' @param session Shiny session
#' @param keys Optional character vector of keys to pull; if NULL return all
#' @return Named list
#' @export
params_get <- function(session, keys = NULL) {
  stopifnot(!missing(session))
  raw <- tryCatch(shiny::getQueryString(session), error = function(e) list())
  if (is.null(keys)) return(raw)
  raw[keys]
}