#' @keywords internal
#' @importFrom rlang .data .env expr
#' @importFrom dplyr select everything
#' @importFrom purrr %||%
#' @importFrom ggplot2 ggplot aes geom_step geom_ribbon
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(c(".", "where"))

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
