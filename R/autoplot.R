#' Plot cuminc Estimates
#'
#' Function uses the result from `tidy(object)` to create figure.
#'
#' @param object object of class 'cuminc'
#' @param ... not used
#'
#' @export
#' @examples
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   autoplot()
autoplot.tidycuminc <- function(object, ...) {
  df_tidy <-
    object$tidy %>%
    dplyr::bind_rows(
      dplyr::select(., dplyr::any_of(c("outcome", "strata", "time", "estimate"))) %>%
        dplyr::mutate(dplyr::across(c(.data$time, .data$estimate), ~0)) %>%
        dplyr::distinct()
    )

  if ("strata" %in% names(df_tidy)) {
    gg <-
      ggplot2::ggplot(df_tidy,
                      ggplot2::aes(x = .data$time,
                                   y = .data$estimate,
                                   color = .data$strata,
                                   linetype = .data$outcome))
  }
  else {
    gg <-
      ggplot2::ggplot(df_tidy,
                      ggplot2::aes(x = .data$time,
                                   y = .data$estimate,
                                   linetype = .data$outcome))
  }

  gg + ggplot2::geom_step()
}

