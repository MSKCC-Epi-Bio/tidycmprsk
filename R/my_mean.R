#' My Mean
#'
#' @param x numeric vector
#'
#' @return numeric scalar
#' @export
#'
#' @examples
#' my_mean(1:5)
my_mean <- function(x) {
  if (rlang::is_string(x)) {
    stop("`x=` cannot be a string.", call. = FALSE)
  }

  mean(x)
}
