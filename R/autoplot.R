#' Plot Cumulative Incidence Estimates
#'
#' @description
#' **DEPRECATED** Use `ggsurvfit::ggcuminc()` instead.
#'
#' Function uses the result from `tidy(object)` to create figure.
#'
#' @param object object of class 'cuminc'
#' @param outcomes character vector of outcomes to include in plot. Default
#' is to include the first competing events.
#' @param aes List of arguments that will be added or replace the existing
#' arguments in `ggplot2::aes()`. Details below.
#' @inheritParams tidy.tidycuminc
#' @param ... not used
#'
#' @section aesthetics:
#' The `aes=` argument accepts a named list of arguments that will be added to
#' or replace existing arguments in the `ggplot2::aes()` call.
#' The tibble used to create the figure is the output from `tidy()`.
#' The default call to `ggplot2::aes()` includes, at most, the following:
#' `ggplot2::aes(x = time, y = estimate, colour = strata, fill = strata, linetype = outcome, ymin = conf.low, ymax = conf.high`
#' Not all arguments appear in every plot, however.
#'
#' @keywords internal
#' @return a ggplot object
#' @family cuminc() functions
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
autoplot.tidycuminc <- function(object, outcomes = NULL,
                                conf.int = FALSE, conf.level = 0.95,
                                aes = NULL, ...) {
  # added this message on 2022-09-17
  cli::cli_inform(c(
    "!" = "The {.code autoplot.tidycuminc()} function is deprecated and no longer maintained.",
    "i" = "Use {.code ggsurvfit::ggcuminc()} instead."
  ))
  # checking inputs ------------------------------------------------------------
  outcomes <- outcomes %||% names(object$failcode)[1]
  outcomes <- match.arg(outcomes, names(object$failcode), several.ok = TRUE)

  # tidying --------------------------------------------------------------------
  df_tidy <-
    tidy(object, conf.int = conf.int, conf.level = conf.level) %>%
    filter(.data$outcome %in% .env$outcomes)

  # construct ggplot call ------------------------------------------------------
  # aes()
  aes_args <- list(x = expr(.data$time), y = expr(.data$estimate))
  if ("strata" %in% names(df_tidy)) {
    aes_args <- c(aes_args, list(colour = expr(.data$strata), fill = expr(.data$strata)))
  }
  if (length(unique(df_tidy$outcome)) > 1) {
    aes_args <- c(aes_args, list(linetype = expr(.data$outcome)))
  }
  if (isTRUE(conf.int)) {
    aes_args <- c(aes_args, list(ymin = expr(.data$conf.low), ymax = expr(.data$conf.high)))
  }

  aes <- as.list(rlang::enexpr(aes))[-1]
  aes_args <- aes_args %>% purrr::list_modify(!!!aes)

  # ggplot call
  gg <-
    rlang::inject(ggplot(data = df_tidy, aes(!!!aes_args))) +
    geom_step()

  if (isTRUE(conf.int)) {
    gg <- gg + geom_ribbon(alpha = 0.2, colour = NA)
  }

  gg
}
