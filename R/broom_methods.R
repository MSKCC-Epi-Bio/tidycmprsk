#' Broom methods for tidycrr objects
#'
#' @param x a tidycrr object
#' @param exponentiate Logical indicating whether or not to exponentiate the
#' coefficient estimates. Defaults to `FALSE`.
#' @param conf.level Level of the confidence interval. Default matches that in
#' `crr(conf.level=)` (typically, 0.95)
#' @inheritParams base_methods_crr
#' @inheritParams broom::tidy.crr
#' @inheritParams predict.tidycrr
#'
#' @name broom_methods_crr
#' @return a tibble
#' @family crr() functions
#' @examples
#' crr <- crr(Surv(ttdeath, death_cr) ~ age + grade, trial)
#'
#' tidy(crr)
#'
#' glance(crr)
#'
#' augment(crr, times = 12)
NULL

# tidy
#' @rdname broom_methods_crr
#' @export
#' @family tidycrr tidiers
tidy.tidycrr <- function(x,
                         exponentiate = FALSE,
                         conf.int = FALSE,
                         conf.level = x$conf.level, ...) {
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
      dplyr::relocate(dplyr::all_of(c("conf.low", "conf.high")), .before = dplyr::all_of("p.value"))
  }

  df_tidy
}

#' @rdname broom_methods_crr
#' @export
#' @family tidycrr tidiers
glance.tidycrr <- function(x, ...) {
  broom::glance(x$cmprsk, ...)
}

#' @rdname broom_methods_crr
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

#' Broom methods for tidy cuminc objects
#'
#' @param x object of class 'tidycuminc'
#' @param conf.level Level of the confidence interval. Default matches that in
#' `cuminc(conf.level=)` (typically, 0.95)
#' @inheritParams base_methods_cuminc
#' @inheritParams broom_methods_cuminc
#' @inheritParams base_methods_crr
#' @inheritParams predict.tidycrr
#' @inheritParams broom::tidy.crr
#'
#' @name broom_methods_cuminc
#' @return a tibble
#' @family cuminc() functions
#'
#' @section `tidy()` data frame:
#'
#' The returned `tidy()` data frame returns the following columns:
#'
#'  ```{r, echo = FALSE}
#' tibble::tribble(
#'   ~`**Column Name**`, ~`**Description**`,
#'   "`outcome`", "Competing Event Outcome",
#'   "`time`", "Numeric follow-up time",
#'   "`estimate`", "Risk estimate",
#'   "`std.error`", "Standard Error",
#'   "`n.risk`", "Number at risk at the specified time",
#'   "`n.event`", "If the `times=` argument is missing, then the number of events that occurred at time `t`. Otherwise, it is the cumulative number of events that have occurred since the last time listed.",
#'   "`n.censor`", "If the `times=` argument is missing, then the number of censored obs at time `t`. Otherwise, it is the cumulative number of censored obs that have occurred since the last time listed.",
#'   "`cum.event`", "Cumulative number of events at specified time",
#'   "`cum.censor`", "Cumulative number of censored observations at specified time"
#' ) %>%
#' knitr::kable()
#' ```
#'
#' If `tidy(time=)` is specified, then `n.event` and `n.censor` are the
#' cumulative number of events/censored in the interval. For example, if
#' `tidy(time = c(0, 12, 18))` is passed, `n.event` and `n.censor` at `time = 18`
#' are the cumulative number of events/censored in the interval `(12, 18]`.
#'
#' @inheritSection cuminc p-values
#' @inheritSection cuminc Confidence intervals
#'
#' @examples
#' cuminc <- cuminc(Surv(ttdeath, death_cr) ~ trt, trial)
#'
#' tidy(cuminc)
#'
#' glance(cuminc)
#'
#' # restructure glance to one line per outcome
#' glance(cuminc) %>%
#'   tidyr::pivot_longer(
#'     everything(),
#'     names_to = c(".value", "outcome_id"),
#'     names_pattern = "(.*)_(.*)"
#'   )
NULL

#' @rdname broom_methods_cuminc
#' @export
#' @family cuminc tidiers
tidy.tidycuminc <- function(x,
                            times = NULL,
                            conf.int = TRUE,
                            conf.level = x$conf.level, ...) {
  # check inputs ---------------------------------------------------------------
  if (!is.numeric(conf.level) || !dplyr::between(conf.level, 0, 1)) {
    stop("`conf.level=` must be between 0 and 1")
  }

  # if user requested the default tidier, return the version in x$tidy ---------
  if (is.null(times) && isTRUE(conf.int) && identical(conf.level, x$conf.level)) {
    return(x$tidy)
  }

  times <- times %||% unique(x$tidy$time) %>% sort()
  if (!is.null(times) && any(times < 0)) {
    message("`times=` cannot be negative. Negative values have been omitted.")
    times <- times[times >= 0]
  }

  # if user requested default tidier without CI, return w/o CI -----------------
  if (is.null(times) && !isTRUE(conf.int)) {
    return(select(x$tidy, -dplyr::all_of(c("conf.low", "conf.high"))))
  }

  # if times are specified, add them (and associated stats) to the data frame
  df_tidy <- .add_tidy_times(x$tidy, times = times)

  # adding cumulative events and censor
  df_tidy <- .add_cumulative_stats(df_tidy)

  # if times are specified, remove the un-selected times and correct the
  # n.censor and n.event stats (these are cumulative over the time
  # interval, between specified times)
  df_tidy <- .keep_selected_times(df_tidy, times = times)

  # return tidied tibble
  df_tidy %>%
    dplyr::select(-dplyr::all_of("time_max")) %>%
    dplyr::mutate(
      conf.level = x$conf.int
    )

  # delete/update CI if needed -------------------------------------------------
  if (!isTRUE(conf.int)) {
    df_tidy <-
      df_tidy %>%
      select(-dplyr::all_of(c("conf.low", "conf.high")))
  }
  else if (!identical(conf.level, x$conf.level)) {
    df_tidy <- add_conf.int(df_tidy, conf.level = conf.level)
  }

  # return tidied df -----------------------------------------------------------
  df_tidy
}

first_cuminc_tidy <- function(x, conf.level) {
  # create df of each outcome level with an ID column as well ------------------
  df_outcomes <-
    rlang::f_lhs(x$formula) %>%
    rlang::eval_tidy(data = x$data) %>%
    attr("states") %>%
    stats::setNames(seq_len(length(.))) %>%
    tibble::enframe("outcome_id", "outcome")

  # will calculate risk estimates at all observed followup times ---------------
  times <-
    union(0, stats::model.frame(x$formula, data = x$data)[[1]][, 1]) %>%
    unique() %>%
    sort()

  # convert estimates into tibble ----------------------------------------------
  df_est <-
    x$cmprsk %>%
    cmprsk::timepoints(times = times) %>%
    purrr::pluck("est") %>%
    cuminc_matrix_to_df(name = "estimate", times = times)

  # convert variances into tibble ----------------------------------------------
  df_se <-
    x$cmprsk %>%
    cmprsk::timepoints(times = times) %>%
    purrr::pluck("var") %>%
    sqrt() %>%
    cuminc_matrix_to_df(name = "std.error", times = times)

  # combine estimates and variances into single tibble -------------------------
  df_tidy <-
    dplyr::full_join(
      df_est, df_se,
      by = intersect(names(df_est), names(df_se))
    ) %>%
    dplyr::full_join(
      df_outcomes,
      by = "outcome_id"
    ) %>%
    select(dplyr::all_of("outcome"), everything(), -dplyr::all_of("outcome_id"))

  # if only one group, then remove the column from -----------------------------
  if (length(unique(df_tidy$strata)) == 1L) {
    df_tidy$strata <- NULL
  }

  # if there is a strata, then make it a factor --------------------------------
  if ("strata" %in% names(df_tidy)) {
    lvls <-
      hardhat::mold(
        x$formula, x$data,
        blueprint = hardhat::default_formula_blueprint(indicators = "none")
      ) %>%
      purrr::pluck("predictors") %>%
      interaction(sep = ", ") %>%
      levels()

    df_tidy$strata <- factor(df_tidy$strata, levels = lvls)
  }

  # add conf.int to tibble -----------------------------------------------------
  df_tidy <- add_conf.int(df_tidy, conf.level = conf.level)

  # adding the number at risk, censored, events --------------------------------
  df_tidy <- add_n_stats(df_tidy, x)

  # return tidied tibble of results --------------------------------------------
  df_tidy
}

add_conf.int <- function(df_tidy, conf.level) {
  # Use CI formula so that CI is bounded [0,100%]
  # (Competing Risks: A Practical Perspective by Melania Pintilie)
  df_tidy %>%
    mutate(
      conf.low =
        .data$estimate^exp(stats::qnorm((1 - .env$conf.level) / 2) * .data$std.error /
                             (.data$estimate * log(.data$estimate))),
      conf.high =
        .data$estimate^exp(-stats::qnorm((1 - .env$conf.level) / 2) * .data$std.error /
                             (.data$estimate * log(.data$estimate))),
      across(dplyr::all_of(c("conf.low", "conf.high")), ~ ifelse(is.nan(.), NA, .))
    )
}

add_n_stats <- function(df_tidy, x) {
  df_Surv <-
    rlang::f_lhs(x$formula) %>%
    rlang::eval_tidy(data = x$data) %>%
    unclass() %>%
    tibble::as_tibble()

  is_strata <- "strata" %in% names(df_tidy)
  if (is_strata) {
    df_Surv <-
      df_Surv %>%
      mutate(
        # this code MUST match that in cuminc.formula()...perhaps should functionalize it so a future update doesn't break anything
        strata =
          hardhat::mold(
            x$formula, x$data,
            blueprint = hardhat::default_formula_blueprint(indicators = "none")
          ) %>%
          purrr::pluck("predictors") %>%
          interaction(sep = ", ") %>%
          as.character()
      )
  }

  ## Determine n at risk (t = 0) --------
  df_n_risk0 <- df_Surv %>%
    dplyr::group_by(across(any_of(c("strata")))) %>%
    dplyr::summarize(n.risk = dplyr::n(),
                     .groups = "drop")

  ## Determine censored & events each t --------
  df_n_cens_event <- df_Surv %>%
    dplyr::group_by(across(any_of(c("time", "strata", "status")))) %>%
    dplyr::summarize(n = dplyr::n(),
                     .groups = "drop")

  ## Determine censored overall each t ---------
  df_n_event_overall <- df_Surv %>%
    filter(.data$status != 0) %>%
    dplyr::group_by(across(any_of(c("time", "strata")))) %>%
    dplyr::summarise(n.event.overall = dplyr::n(),
                     .groups = "drop")

  ## Joining the data --------
  if(is_strata) {
    df_result <- df_tidy %>%
      dplyr::left_join(df_n_risk0,
                       by = "strata") %>%
      dplyr::left_join(df_n_cens_event %>%
                         filter(.data$status != 0) %>%
                         mutate(outcome = factor(.data$status,
                                                 x$failcode,
                                                 names(x$failcode))) %>%
                         select(-dplyr::all_of(c("status"))) %>%
                         dplyr::rename(n.event = dplyr::all_of("n")),
                       by = c("strata", "time", "outcome")) %>%
      dplyr::left_join(df_n_cens_event %>%
                         filter(.data$status == 0) %>%
                         select(-dplyr::all_of("status")) %>%
                         dplyr::rename(n.censor = dplyr::all_of("n")),
                       by = c("strata", "time"))   %>%
      dplyr::left_join(df_n_event_overall,
                       by = c("strata", "time"),
                       relationship = "many-to-many")
  } else {
    df_result <- df_tidy %>%
      dplyr::cross_join(df_n_risk0) %>%
      dplyr::left_join(df_n_cens_event %>%
                         filter(.data$status != 0) %>%
                         mutate(outcome = factor(.data$status,
                                                 x$failcode,
                                                 names(x$failcode))) %>%
                         select(-dplyr::all_of(c("status"))) %>%
                         dplyr::rename(n.event = dplyr::all_of("n")),
                       by = c("time", "outcome")) %>%
      dplyr::left_join(df_n_cens_event %>%
                         filter(.data$status == 0) %>%
                         select(-dplyr::all_of("status")) %>%
                         dplyr::rename(n.censor = dplyr::all_of("n")),
                       by = c("time")) %>%
      dplyr::left_join(df_n_event_overall,
                       by = c("time"))
  }

  # Fill missing values and build cum. sum ------
  df_result <- df_result %>%
    mutate(n.event  = dplyr::coalesce(.data$n.event, 0L),
           n.censor = dplyr::coalesce(.data$n.censor, 0L),
           n.event.overall = dplyr::coalesce(.data$n.event.overall, 0L)) %>%
    group_by(across(any_of(c("strata", "outcome")))) %>%
    arrange("time") %>%
    mutate(cum.event  = cumsum(.data$n.event),
           cum.censor = cumsum(.data$n.censor),
           cum.event.overall = cumsum(.data$n.event.overall),
           n.risk = .data$n.risk - .data$cum.event.overall - .data$cum.censor +
             .data$n.event.overall + .data$n.censor) %>%
    dplyr::ungroup() %>%
    arrange(across(any_of(c("strata", "outcome", "time", "n.risk")))) %>%
    select(any_of(c("time", "outcome", "strata")),
           everything()) %>%
    select(-any_of(c("cum.event.overall",
                     "n.event.overall")))

  if(is_strata)
    df_result <- df_result %>%
      mutate(strata = factor(.data$strata,
                             levels(df_tidy$strata)))

  return(df_result)
}

cuminc_matrix_to_df <- function(x, name, times) {
  df <-
    as.data.frame(x) %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    mutate(
      strata = stringr::word(.data$rowname, 1, -2),
      outcome_id = stringr::word(.data$rowname, -1L),
      .before = dplyr::all_of("rowname")
    ) %>%
    select(-dplyr::all_of("rowname")) %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(c("strata", "outcome_id")),
      names_to = "time_chr",
      values_to = name
    ) %>%
    group_by(.data$strata, .data$outcome_id) %>%
    mutate(time = times, .after = dplyr::all_of("time_chr")) %>%
    dplyr::ungroup()

  # checking for issues mapping the numeric times back onto the estimates
  if (any(is.na(df$time)) || any(is.na(df$time_chr)) ||
      max(abs(df$time - as.numeric(df$time_chr))) > 10e-5) {
    paste(
      "There was an error mapping observed times to cumulative",
      "incidence estimates. Please report this bug at",
      "'https://github.com/MSKCC-Epi-Bio/tidycmprsk/issues'"
    ) %>%
      stringr::str_wrap() %>%
      stop(call. = FALSE)
  }

  df %>% select(-dplyr::all_of("time_chr"))
}

#' @rdname broom_methods_cuminc
#' @export
#' @family tidycuminc tidiers
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
        mutate(failcode_id = as.character(.data$failcode_id)),
      by = "failcode_id"
    ) %>%
    select(dplyr::all_of(c("outcome", "failcode_id")),
           statistic = dplyr::all_of("stat"),
           dplyr::all_of("df"),
           p.value = dplyr::all_of("pv")
    ) %>%
    tidyr::pivot_wider(
      values_from = dplyr::all_of(c("outcome", "statistic", "df", "p.value")),
      names_from = dplyr::all_of("failcode_id"),
      names_glue = "{.value}_{failcode_id}"
    ) %>%
    select(!!!select_expr)
}




.keep_selected_times <- function(x, times) {
  if (is.null(times)) {
    return(x)
  }

  times0 <- union(0, times) %>% sort()

  x %>%
    dplyr::mutate(
      time_group = cut(.data$time, breaks = union(.env$times0, max(.env$x$time)))
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("time_group", "strata", "outcome")))) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("n.censor", "n.event")),
        ~ sum(.)
      )
    ) %>%
    dplyr::filter(.data$time %in% .env$times) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::all_of("time_group"))
}

.add_cumulative_stats <- function(x) {
  x %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata", "outcome")))) %>%
    dplyr::mutate(
      cum.event = cumsum(.data$n.event),
      cum.censor = cumsum(.data$n.censor),
      .after = dplyr::all_of("n.censor")
    ) %>%
    dplyr::ungroup()
}

.add_tidy_times <- function(x, times) {
  x <-
    x %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata", "outcome")))) %>%
    dplyr::mutate(
      time_max = max(.data$time)
    ) %>%
    dplyr::ungroup()

  if (is.null(times)) {
    return(x)
  }

  # create tibble of times
  df_times <-
    do.call(
      what = expand.grid,
      args =
        # remove NULL elements before passing to exapnd grid
        Filter(
          Negate(is.null),
          list(
            time = times,
            strata = suppressWarnings(unique(x$strata)),
            outcome = suppressWarnings(unique(x$outcome)),
            stringsAsFactors = FALSE
          )
        )
    )

  # merge tibble of times with tidy df
  df_result <-
    dplyr::full_join(
      x,
      df_times,
      by = intersect(c("time", "strata", "outcome"), names(x))
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c("outcome", "strata", "time"))))

  # fill in missing stats
  df_result <-
    df_result %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata", "outcome")))) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c("n.event", "n.censor")), ~ ifelse(is.na(.), 0, .))
    ) %>%
    tidyr::fill(
      -dplyr::all_of(c("n.risk", "n.event", "n.censor")),
      .direction = "down"
    ) %>%
    tidyr::fill(
      dplyr::all_of("n.risk"),
      .direction = "up"
    ) %>%
    dplyr::mutate(
      n.risk = ifelse(.data$time > .data$time_max, 0, .data$n.risk)
    ) %>%
    dplyr::ungroup()

  # any times above the max observed time are set to NA
  df_result %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("estimate", "std.error", "conf.low", "conf.high")),
        ~ ifelse(.data$time > .data$time_max, NA, .)
      )
    )
}
