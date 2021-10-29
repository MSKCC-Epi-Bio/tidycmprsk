#' Plot cuminc Estimates
#'
#' Function uses the result from `tidy(object)` to create figure.
#'
#' @param object object of class 'cuminc'
#' @param outcomes character vector of outcomes to include in plot
#' @inheritParams tidy.tidycuminc
#' @param ... not used
#'
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   autoplot()
#'
#' # Example 2 ----------------------------------
#' cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>%
#'   autoplot(outcomes = "death from cancer", conf.int = TRUE) +
#'   ggplot2::labs(
#'     x = "Months from Treatment",
#'     y = "Risk of Death"
#'   )

autoplot.tidycuminc <- function(object, outcomes = names(object$failcode),
                                conf.int = FALSE, conf.level = 0.95, ...) {
  # checking inputs ------------------------------------------------------------
  outcomes <- match.arg(outcomes, names(object$failcode), several.ok = TRUE)

  # tidying --------------------------------------------------------------------
  df_tidy <-
    tidy(object, conf.int = conf.int, conf.level = conf.level) %>%
    dplyr::filter(.data$outcome %in% .env$outcomes) %>%
    # adding time = 0 into the data set
    dplyr::bind_rows(
      dplyr::select(., dplyr::any_of(c("outcome", "strata", "time", "estimate"))) %>%
        dplyr::mutate(dplyr::across(c(.data$time, .data$estimate), ~0)) %>%
        dplyr::distinct()
    )

  # construct ggplot call ------------------------------------------------------
  # aes()
  aes_args <- list(x = expr(.data$time), y = expr(.data$estimate))
  if ("strata" %in% names(df_tidy))
    aes_args <- c(aes_args, list(colour = expr(.data$strata), fill = expr(.data$strata)))
  if (length(unique(df_tidy$outcome)) > 1)
    aes_args <- c(aes_args, list(linetype = expr(.data$outcome)))
  if (isTRUE(conf.int))
    aes_args <- c(aes_args, list(ymin = expr(.data$conf.low), ymax = expr(.data$conf.high)))

  # ggplot call
  gg <-
    rlang::inject(ggplot(data = df_tidy, aes(!!!aes_args))) +
    geom_step()

  if (isTRUE(conf.int))
    gg <- gg + geom_ribbon(alpha = 0.2, colour = NA)

  gg
}

