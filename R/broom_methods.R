#' Broom methods for tidycmprsk objects
#'
#' @inheritParams broom::tidy.crr
#' @inheritParams base_methods
#' @param x placeholder
#' @param times vector of numeric time points where risk estimates will be shown.
#' Default it to use all observed times.
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
      x$cmprsk,
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
  broom::glance(x$cmprsk, ...)
}

#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
augment.tidycrr <- function(x, times = NULL, probs = NULL, newdata = NULL, ...) {
  pred <-
    predict.tidycrr(x, times = times, probs = probs, newdata = newdata) %>%
    dplyr::bind_cols()

  dplyr::bind_cols(
    newdata %||% x$data,
    pred
  ) %>%
    tibble::as_tibble()
}

#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
tidy.tidycuminc <- function(x, conf.int = FALSE, conf.level = 0.95,
                            times = NULL, ...) {
  # create df of each outcome level with an ID column as well
  df_outcomes <-
    rlang::f_lhs(x$formula) %>%
    rlang::eval_tidy(data = x$data) %>%
    attr("states") %>%
    stats::setNames(seq_len(length(.))) %>%
    tibble::enframe("outcome_id", "outcome")

  # will calculate risk estimates at all observed followup times
  times <-
    times %||%
    stats::model.frame(x$formula, data = x$data)[[1]][, 1] %>%
    unique() %>%
    sort()

  # convert estimates into tibble
  df_est <-
    x$cmprsk %>%
    cmprsk::timepoints(times = times) %>%
    purrr::pluck("est") %>%
    cuminc_matrix_to_df(name = "estimate")

  # convert variances into tibble
  df_se <-
    x$cmprsk %>%
    cmprsk::timepoints(times = times) %>%
    purrr::pluck("var") %>%
    sqrt() %>%
    cuminc_matrix_to_df(name = "std.error")

  # combine estimates and variances into single tibble
  df_tidy <-
    dplyr::full_join(
      df_est, df_se,
      by = c("strata", "outcome_id", "time")
    ) %>%
    dplyr::full_join(
      df_outcomes,
      by = "outcome_id"
    ) %>%
    dplyr::select(.data$outcome, dplyr::everything(), -.data$outcome_id)

  # if only one group, then remove the column from
  if (length(unique(df_tidy$strata)) == 1L) {
    df_tidy$strata <- NULL
  }

  # if user requested conf.int, add to tibble
  if (isTRUE(conf.int)) {
    # Use CI formula so that CI is bounded [0,100%]
    # (Competing Risks: A Practical Perspective by Melania Pintilie)
    df_tidy <-
      df_tidy %>%
      dplyr::mutate(
        conf.low =
          .data$estimate^exp(stats::qnorm((1 - .env$conf.level) / 2) * .data$std.error /
            (.data$estimate * log(.data$estimate))),
        conf.high =
          .data$estimate^exp(-stats::qnorm((1 - .env$conf.level) / 2) * .data$std.error /
            (.data$estimate * log(.data$estimate))),
        dplyr::across(c(.data$conf.low, .data$conf.high), ~ ifelse(is.nan(.), NA, .))
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
      strata = stringr::word(.data$rowname, 1, -2),
      outcome_id = stringr::word(.data$rowname, -1L),
      .before = .data$rowname
    ) %>%
    dplyr::select(-.data$rowname) %>%
    tidyr::pivot_longer(
      cols = -c(.data$strata, .data$outcome_id),
      names_to = "time",
      values_to = name
    ) %>%
    dplyr::mutate(time = as.numeric(.data$time))
}

#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
glance.tidycuminc <- function(x, ...) {
  if (is.null(x$cmprsk$Tests)) {
    return(tibble::tibble())
  }

  # create select input for re-ordering variables at the end
  select_expr <-
    stringr::str_glue("ends_with('_{x$failcode}')") %>%
    purrr::map(rlang::parse_expr)

  x$cmprsk$Tests %>%
    as.data.frame() %>%
    tibble::rownames_to_column("failcode_id") %>%
    tibble::as_tibble() %>%
    dplyr::left_join(
      x$failcode %>%
        tibble::enframe("outcome", "failcode_id") %>%
        dplyr::mutate(failcode_id = as.character(.data$failcode_id)),
      by = "failcode_id"
    ) %>%
    select(.data$outcome, .data$failcode_id, statistic = .data$stat,
           .data$df, p.value = .data$pv) %>%
    tidyr::pivot_wider(
      values_from = c(.data$outcome, .data$statistic, .data$df, .data$p.value),
      names_from = .data$failcode_id,
      names_glue = "{.value}_{failcode_id}"
    ) %>%
    select(!!!select_expr)
}
