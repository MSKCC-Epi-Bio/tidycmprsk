#' Print crr object
#'
#' @param x tidycrr obejct
#' @param ... not used
#'
#' @export
print.tidycrr <- function(x, ...){
  cli::cli_h1("tidycrr()")
  cli::cli_li("Call {.field {deparse(x$formula)}}")
  cli::cli_li("Failure type of interest {.val {names(x$failcode)}}")

  cat("\nFine and Gray's model fit: \n")
  print(x$tidy)
  invisible()

  # cat("Call: \n")
  # print(x$formula)
  # cat(paste("Failure type of interest:",x$failcode,"\n"))
  # cat("Fine and Gray's model fit: \n")
  # print(x$tidy)
  # invisible(x)
}
