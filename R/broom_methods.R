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

#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
tidy.tidycuminc <- function(x, conf.int = FALSE, conf.level = 0.95,  ...) {
  # create df of each outcome level with an ID column as well
  df_outcomes <-
    rlang::f_lhs(x$formula) %>%
    rlang::eval_tidy(data = x$data) %>%
    attr("states") %>%
    stats::setNames(seq_len(length(.))) %>%
    tibble::enframe("outcome_id", "outcome")

  # will calculate risk estimates at all observed followup times
  times <-
    stats::model.frame(x$formula, data = x$data)[[1]][, 1] %>% unique() %>% sort()

  # convert estimates into tibble
  df_est <-
    x$original_fit %>% cmprsk::timepoints(times = times) %>%
    purrr::pluck("est") %>%
    cuminc_matrix_to_df(name = "estimate")

  # convert variances into tibble
  df_se <-
    x$original_fit %>% cmprsk::timepoints(times = times) %>%
    purrr::pluck("var") %>%
    sqrt() %>%
    cuminc_matrix_to_df(name = "std.error")

  # combine estimates and variances into single tibble
  df_tidy <-
    dplyr::full_join(
      df_est, df_se,
      by = c("strata_id", "outcome_id", "time")
    ) %>%
    dplyr::full_join(
      df_outcomes,
      by = "outcome_id"
    ) %>%
    dplyr::select(.data$outcome, dplyr::everything(), -.data$outcome_id)

  # if only one group, then remove the column from
  if (length(unique(df_tidy$strata_id)) == 1L) {
    df_tidy$strata_id <- NULL
  }
  #otherwise, link the strata_id to the stratifying variables
  else {
    processed <- hardhat::mold(x$formula, x$data)
    df_strata <-
      dplyr::distinct(processed$predictors)
      names(processed$predictors) %>%
      stats::setNames(seq_len(length(.)) - 1L) %>%
      tibble::enframe("strata_id", "strata")

    df_tidy <-
      df_tidy %>%
      dplyr::full_join(
        df_strata,
        by = "strata_id"
      ) %>%
      dplyr::select(.data$strata, dplyr::everything(), -.data$strata_id)
  }

  # if user requested conf.int, add to tibble
  if (isTRUE(conf.int)) {
    # Use CI formula so that CI is bounded [0,100%]
    # (Competing Risks: A Practical Perspective by Melania Pintilie)
    df_tidy <-
      df_tidy %>%
      dplyr::mutate(
        conf.low =
          .data$estimate ^ exp(stats::qnorm((1 - .env$conf.level) / 2) * .data$std.error /
                                 (.data$estimate * log(.data$estimate))),
        conf.high =
          .data$estimate ^ exp(-stats::qnorm((1 - .env$conf.level) / 2) * .data$std.error /
                                 (.data$estimate * log(.data$estimate))),
        dplyr::across(c(.data$conf.low, .data$conf.high), ~ifelse(is.nan(.), NA, .))
      )
  }

  # return tidied tibble of results
  df_tidy
}

cuminc_matrix_to_df <- function(x, name) {
    as.data.frame(x) %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      strata_id = stringr::word(.data$rowname),
      outcome_id = stringr::word(.data$rowname, 2L),
      .before = .data$rowname
    ) %>%
    dplyr::select(-.data$rowname) %>%
    tidyr::pivot_longer(
      cols = -c(.data$strata_id, .data$outcome_id),
      names_to = "time",
      values_to = name
    ) %>%
    dplyr::mutate(time = as.numeric(.data$time))
}
