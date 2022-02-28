#' Broom methods for tidycrr objects
#'
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
      dplyr::relocate(.data$conf.low, .data$conf.high, .before = .data$p.value)
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
    return(select(x$tidy, -.data$conf.low, -.data$conf.high))
  }

  # tidy df with requested time points -----------------------------------------
  df_tidy <-
    x$tidy %>%
    dplyr::full_join(
      list(
        outcome = unique(x$tidy$outcome),
        strata = switch("strata" %in% names(x$tidy),
                        unique(x$tidy$strata)
        ),
        time = times
      ) %>%
        purrr::compact() %>%
        purrr::cross_df(),
      by = intersect(c("outcome", "strata", "time"), names(.))
    ) %>%
    arrange(across(any_of(c("strata", "outcome", "time")))) %>%
    group_by(across(any_of(c("strata", "outcome")))) %>%
    # replace unobserved timepoints with 0 counts for events and censored
    mutate(
      across(c(.data$n.event, .data$n.censor), ~ tidyr::replace_na(., 0L)),
      ..max_time.. = max(.data$time[!is.na(.data$estimate)]),
      ..min_time.. = min(.data$time[!is.na(.data$estimate) & .data$time > 0])
    ) %>%
    # fill down the estimates
    tidyr::fill(
      .data$estimate, .data$std.error, .data$conf.low, .data$conf.high,
      .data$cum.event, .data$cum.censor,
      .direction = "down"
    ) %>%
    tidyr::fill(
      .data$n.risk,
      .direction = "up"
    ) %>%
    # correcting values larger than largest observed timepoint
    mutate(
      across(
        c(.data$estimate, .data$std.error, .data$conf.low, .data$conf.high),
        ~ ifelse(.data$time > .data$..max_time.., NA, .)
      ),
      n.risk = ifelse(.data$time > .data$..max_time.., 0L, .data$n.risk),
      across(
        c(.data$conf.low, .data$conf.high),
        ~ ifelse(.data$time < .data$..min_time.., NA, .)
      ),
      across(
        c(.data$estimate, .data$std.error),
        ~ ifelse(.data$time < .data$..min_time.., 0L, .)
      )
    ) %>%
    # select(-.data$..max_time..,-.data$..min_time..) %>%
    dplyr::ungroup() %>%
    filter(.data$time %in% .env$times) %>%
    group_by(across(any_of(c("strata", "outcome")))) %>%
    mutate(
      n.event = c(.data$cum.event[1],diff(.data$cum.event)),
      n.censor = c(.data$cum.censor[1],diff(.data$cum.censor))
    )  %>%
    # correcting values larger than largest observed timepoint
    mutate(
      across(
        c(.data$n.event, .data$n.censor),
        ~ ifelse(.data$time > .data$..max_time.. | .data$time < .data$..min_time.., 0L, .)
      )
    ) %>%
    select(-.data$..max_time..,-.data$..min_time..) %>%
    dplyr::ungroup()

  # delete/update CI if needed -------------------------------------------------
  if (!isTRUE(conf.int)) {
    df_tidy <-
      df_tidy %>%
      select(-.data$conf.low, -.data$conf.high)
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
    select(.data$outcome, everything(), -.data$outcome_id)

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
      across(c(.data$conf.low, .data$conf.high), ~ ifelse(is.nan(.), NA, .))
    )
}

add_n_stats <- function(df_tidy, x) {
  df_Surv <-
    rlang::f_lhs(x$formula) %>%
    rlang::eval_tidy(data = x$data) %>%
    unclass() %>%
    tibble::as_tibble()

  if ("strata" %in% names(df_tidy)) {
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

  df_time_zero <-
    df_Surv %>%
    select(any_of(c("strata", "status"))) %>%
    group_by(across(any_of(c("strata")))) %>%
    mutate(
      time = 0,
      n.event = 0L,
      n.risk = dplyr::n(),
      n.censor = 0L,
      outcome =
        dplyr::recode(
          .data$status,
          !!!(as.list(names(x$failcode)) %>% stats::setNames(unlist(x$failcode))),
          .default = NA_character_
        )
    ) %>%
    filter(.data$status != 0) %>%
    select(-.data$status) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  df_Surv <-
    df_Surv %>%
    filter(stats::complete.cases(.)) %>%
    arrange(across(any_of(c("strata", "time", "status")))) %>%
    group_by(across(any_of(c("strata")))) %>%
    mutate(
      n.risk = dplyr::n() - dplyr::row_number() + 1L,
      n.event = as.integer(.data$status != 0),
      n.censor = as.integer(.data$status == 0)
    ) %>%
    group_by(across(any_of(c("strata", "time")))) %>%
    mutate(
      outcome = ifelse(.data$status != 0,
                       dplyr::recode(.data$status, !!!(as.list(names(x$failcode)) %>% stats::setNames(unlist(x$failcode)))),
                       "censored"
      )
    ) %>%
    select(-.data$status) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  df_n_censor <-
    df_Surv %>%
    filter(.data$outcome == "censored") %>%
    group_by(across(any_of(c("strata", "time")))) %>%
    dplyr::slice(rep(1:dplyr::n(), each = length(x$failcode))) %>%
    mutate(
      status = rep(1:length(x$failcode), dplyr::n() / length(x$failcode))
    ) %>%
    mutate(
      outcome =
        dplyr::recode(.data$status, !!!(as.list(names(x$failcode)) %>% stats::setNames(unlist(x$failcode))))
    ) %>%
    select(-.data$status) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  df_Surv <- df_Surv %>%
    filter(.data$outcome != "censored") %>%
    group_by(across(any_of(c("strata", "outcome", "time")))) %>%
    dplyr::slice(rep(1:dplyr::n(), each = length(x$failcode))) %>%
    mutate(
      status = rep(1:length(x$failcode), dplyr::n() / length(x$failcode)),
      outcome2 =
        dplyr::recode(.data$status, !!!(as.list(names(x$failcode)) %>% stats::setNames(unlist(x$failcode)))),
      n.event = as.integer(.data$outcome == .data$outcome2),
      outcome = .data$outcome2
    ) %>%
    select(-.data$status, -.data$outcome2)

  df_Surv <- merge(df_Surv, df_n_censor, all = TRUE)
  df_Surv <- merge(df_Surv, df_time_zero, all = TRUE)
  df_Surv <- df_Surv %>%
    arrange(across(any_of(c("strata", "outcome", "time")))) %>%
    mutate(
      ties = ifelse(.data$time == dplyr::lag(.data$time, default = -1), 1, 0)
    )

  df_Surv$keep <- 1
  for (ii in 1:nrow(df_Surv)) {
    if (df_Surv$ties[ii] == 1) {
      df_Surv$n.event[ii] <- df_Surv$n.event[ii] + df_Surv$n.event[ii - 1]
      df_Surv$n.censor[ii] <- df_Surv$n.censor[ii] + df_Surv$n.censor[ii - 1]
      df_Surv$keep[ii - 1] <- 0
    }
  }

  df_Surv <- df_Surv %>%
    filter(.data$keep == 1) %>%
    select(-.data$ties, -.data$keep)

  df_Surv <- df_Surv %>%
    group_by(across(any_of(c("strata", "outcome")))) %>%
    arrange(across(any_of(c("strata", "outcome", "time")))) %>%
    mutate(
      cum.event = as.integer(cumsum(.data$n.event)),
      cum.censor = as.integer(cumsum(.data$n.censor))
    )

  if ("strata" %in% names(df_tidy)) {
    output <- merge(df_tidy, df_Surv, by = c("time", "outcome", "strata"), all.y = TRUE)
  } else {
    output <- merge(df_tidy, df_Surv, by = c("time", "outcome"), all.y = TRUE)
  }

  output %>%
    arrange(across(any_of(c("strata", "outcome", "time", "n.risk")))) %>%
    group_by(across(any_of(c("strata", "outcome")))) %>%
    tidyr::fill(.data$n.risk, .data$estimate, .data$std.error,
                .data$conf.low, .data$conf.high,
                .data$n.event, .data$n.censor, .data$cum.event,
                .data$cum.censor,
                .direction = "down"
    ) %>%
    dplyr::ungroup() %>%
    filter(!is.na(.data$outcome)) %>%
    dplyr::distinct()
}

cuminc_matrix_to_df <- function(x, name, times) {
  df <-
    as.data.frame(x) %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    mutate(
      strata = stringr::word(.data$rowname, 1, -2),
      outcome_id = stringr::word(.data$rowname, -1L),
      .before = .data$rowname
    ) %>%
    select(-.data$rowname) %>%
    tidyr::pivot_longer(
      cols = -c(.data$strata, .data$outcome_id),
      names_to = "time_chr",
      values_to = name
    ) %>%
    group_by(.data$strata, .data$outcome_id) %>%
    mutate(time = times, .after = .data$time_chr) %>%
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

  df %>% select(-.data$time_chr)
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
    select(.data$outcome, .data$failcode_id,
           statistic = .data$stat,
           .data$df, p.value = .data$pv
    ) %>%
    tidyr::pivot_wider(
      values_from = c(.data$outcome, .data$statistic, .data$df, .data$p.value),
      names_from = .data$failcode_id,
      names_glue = "{.value}_{failcode_id}"
    ) %>%
    select(!!!select_expr)
}
