#' Broom methods for tidycmprsk objects
#'
#' @inheritParams broom::tidy.crr
#' @param new_data placeholder
#' @param x placeholder
#' @param quantiles placeholder
#' @param ... not used
#'
#' @name broom_methods
#' @return a tibble
NULL

# tidy
#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
tidy.tidycrr <- function(x,
                         exponentiate = FALSE,
                         conf.int = FALSE,
                         conf.level = 0.95, ...) {
  df_tidy <-
    broom::tidy(
    x$original_fit,
    exponentiate = exponentiate,
    conf.int = conf.int,
    conf.level = conf.level, ...
  )

  if (isTRUE(conf.int)) {
    df_tidy <-
      df_tidy %>%
      dplyr::relocate(.data$conf.low, .data$conf.high, .before = .data$p.value)
  }

  df_tidy
}

#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
glance.tidycrr <- function(x, ...) {
  broom::glance(x$original_fit, ...)
}

#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
augment.tidycrr <- function(x, quantiles = seq(0, 1, 0.25), new_data, ...) {
  pred <- predict.tidycrr(x, new_data = x$model, quantiles = quantiles)
  out <- cbind(
    pred$newdata,
    pred$qout,
    pred$lpout
  )
  tibble::as_tibble(out)
}
