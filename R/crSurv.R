#' Competing Risks Outcome Definition
#'
#' I think this fn should also somehow define the event of interest and the
#' censoring code somehow....? as an attribute?
#' @param time vector of failure/censoring times
#' @param event vector with a unique code for each failure type and a separate
#' code for censored observations
#'
#' @return
#' @export
#'
#' @examples
#' # add example
crSurv <- function(time, event) {
  cbind(time, event)
}
