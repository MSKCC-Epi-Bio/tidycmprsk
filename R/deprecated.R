#' Deprecated
#'
#' @description
#' **DEPRECATED** functions.
#'
#' @param ... not used
#' @keywords internal
#'
#' @name deprecated
#' @return Misc.
#'
#' @examples
#' # DO NOT USE DEPRECATED FUNCTIONS
NULL

#' @rdname deprecated
#' @export
autoplot.tidycuminc <- function(...) {
  # function was deprecated on 2022-09-17
  cli::cli_abort(c("!" = "The {.code autoplot.tidycuminc()} function is deprecated.",
                   "i" = "Use {.code ggsurvfit::ggcuminc()} instead."))
}
