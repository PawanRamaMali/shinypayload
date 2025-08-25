#' Get URL query parameters in Shiny
#' @param session Shiny session
#' @param keys Optional character vector of keys to pull; if NULL return all
#' @return A named list containing the URL query parameters. If \code{keys} is 
#'   specified, only those parameters are returned. If no parameters exist or 
#'   the specified keys are not found, returns an empty list or list with 
#'   \code{NULL} values respectively.
#' @export
#' @examples
#' if (interactive()) {
#' server <- function(input, output, session) {
#'   # Get all query parameters
#'   all_params <- params_get(session)
#'
#'   # Get specific parameters
#'   user_params <- params_get(session, keys = c("user_id", "token"))
#'
#'   # Use in outputs
#'   output$params_display <- renderText({
#'     params <- params_get(session)
#'     if (length(params) > 0) {
#'       paste("Parameters:", jsonlite::toJSON(params))
#'     } else {
#'       "No parameters provided"
#'     }
#'   })
#' }
#' }
params_get <- function(session, keys = NULL) {
  stopifnot(!missing(session))
  raw <- tryCatch(shiny::getQueryString(session), error = function(e) list())
  if (is.null(keys)) {
    return(raw)
  }
  raw[keys]
}
