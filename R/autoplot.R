#' Plot Cumulative Incidence Estimates
#'
#' @description
#' **DEPRECATED** Use [`ggsurvfit::ggcuminc()`] instead.
#'
#' @param object,outcomes,aes,conf.int,conf.level,... **DEPRECATED**
#' @keywords internal
#' @export

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
