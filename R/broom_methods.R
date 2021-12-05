#' Broom methods for tidycrr objects
#'
#' @param exponentiate Logical indicating whether or not to exponentiate the
#' coefficient estimates. Defaults to `FALSE`.
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
#'   "`n.event`", "Cumulative number of events at specified time",
#'   "`n.censor`", "Cumulative number of censored observations"
#' ) %>%
#' knitr::kable()
#' ```
#'
#' @section `tidy()` confidence intervals:
#'
#' The confidence intervals in `tidy()` use the recommended method in
#' *Competing Risks: A Practical Perspective* by Melania Pintilie.
#'
#' `   x^exp(-z * se / (x * log(x))), x^exp(z * se / (x * log(x))) `
#'
#' @examples
#' cuminc <- cuminc(Surv(ttdeath, death_cr) ~ trt, trial)
#'
#' tidy(cuminc)
#'
#' glance(cuminc)
NULL


#' @rdname broom_methods_cuminc
#' @export
#' @family cuminc tidiers
tidy.tidycuminc <- function(x, times = NULL,
                            conf.int = TRUE, conf.level = 0.95, ...) {
  # check inputs ---------------------------------------------------------------
  if (!is.numeric(conf.level)  || !dplyr::between(conf.level, 0, 1)) {
    stop("`conf.level=` must be between 0 and 1")
  }

  # if user requested the default tidier, return the version in x$tidy ---------
  if (is.null(times) && isTRUE(conf.int) && identical(conf.level, 0.95)) {
    return(x$tidy)
  }

  times <- times %||% unique(x$tidy$time) %>% sort()
  if (!is.null(times) && any(times < 0 | times > max(x$tidy$time))) {
    stringr::str_glue(
      "`times=` must be in [0, {max(x$tidy$time)}]. Values outside this range",
      "have been omitted.") %>%
      message()
    times <- times[times >= 0 | times <= max(x$tidy$time)]
  }

  # if user requested default tidier without CI, return w/o CI -----------------
  if (is.null(times) && !isTRUE(conf.int)) {
    return(dplyr::select(x$tidy, -.data$conf.low, -.data$conf.high))
  }

  # tidy df with requested time points -----------------------------------------
  df_tidy <-
    x$tidy %>%
    dplyr::full_join(
      list(outcome = unique(x$tidy$outcome),
           strata = switch("strata" %in% names(x$tidy), unique(x$tidy$strata)),
           time = times) %>%
        purrr::compact() %>%
        purrr::cross_df(),
      by = intersect(c("outcome", "strata", "time"), names(.))
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c("strata", "outcome", "time")))) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata", "outcome")))) %>%
    tidyr::fill(.data$estimate, .data$std.error, .data$conf.low, .data$conf.high,
                .data$n.risk, .data$n.event, .data$n.censor) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$time %in% .env$times)

  # delete/update CI if needed -------------------------------------------------
  if (!isTRUE(conf.int)) {
    df_tidy <-
      df_tidy %>%
      dplyr::select(-.data$conf.low, -.data$conf.high)
  }
  else if (!identical(conf.level, 0.95)) {
    df_tidy <- add_conf.int(df_tidy, conf.level)
  }

  # return tidied df -----------------------------------------------------------
  df_tidy
}

first_cuminc_tidy <- function(x) {
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
    cuminc_matrix_to_df(name = "estimate")

  # convert variances into tibble ----------------------------------------------
  df_se <-
    x$cmprsk %>%
    cmprsk::timepoints(times = times) %>%
    purrr::pluck("var") %>%
    sqrt() %>%
    cuminc_matrix_to_df(name = "std.error")

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
    dplyr::select(.data$outcome, dplyr::everything(), -.data$outcome_id)

  # if only one group, then remove the column from -----------------------------
  if (length(unique(df_tidy$strata)) == 1L) {
    df_tidy$strata <- NULL
  }

  # add conf.int to tibble -----------------------------------------------------
  df_tidy <- add_conf.int(df_tidy, conf.level = 0.95)

  # adding the number at risk, censored, events --------------------------------
  df_tidy <- add_n_stats(df_tidy, x)

  # return tidied tibble of results --------------------------------------------
  df_tidy
}

add_conf.int <- function(df_tidy, conf.level) {
  # Use CI formula so that CI is bounded [0,100%]
  # (Competing Risks: A Practical Perspective by Melania Pintilie)
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

add_n_stats <- function(df_tidy, x) {
  df_Surv <-
    rlang::f_lhs(x$formula) %>%
    rlang::eval_tidy(data = x$data) %>%
    unclass() %>%
    tibble::as_tibble()

  if ("strata" %in% names(df_tidy)) {
    df_Surv <-
      df_Surv %>%
      dplyr::mutate(
        # this code MUST match that in cuminc.formula()...perhaps should functionalize it so a future update doesn't break anything
        strata =
          hardhat::mold(
            x$formula, x$data,
            blueprint = hardhat::default_formula_blueprint(indicators = "none")
          ) %>%
          purrr::pluck("predictors") %>%
          interaction() %>%
          as.character()
      )
  }

  df_Surv <-
    df_Surv %>%
    dplyr::filter(stats::complete.cases(.))

  df_n_risk <-
    df_Surv %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c("strata", "time", "status"))))%>%
    dplyr::group_by(dplyr::across(dplyr::any_of("strata"))) %>%
    dplyr::mutate(

      ### Could you please clarify if the risk set contains subjects alive prior
      ### to the time of interest?

      n.risk = dplyr::n() - cumsum(.data$status != 0) - cumsum(.data$status == 0) #+ (.data$status != 0)
    ) %>%
    dplyr::select(dplyr::any_of(c("strata", "time", "n.risk"))) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata", "time")))) %>%
    dplyr::mutate(
      n.risk = min(.data$n.risk)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  df_n_event <-
    df_Surv %>%
    dplyr::filter(.data$status != 0) %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c("strata", "time", "status"))))%>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata", "status")))) %>%
    dplyr::mutate(
      n.event = dplyr::row_number()
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata", "time")))) %>%
    dplyr::mutate(
      n.event = max(.data$n.event),
      outcome =
        dplyr::recode(.data$status, !!!(as.list(names(x$failcode)) %>% stats::setNames(unlist(x$failcode))))
    ) %>%
    dplyr::select(-.data$status) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  df_n_censor <-
    df_Surv %>%
    dplyr::filter(.data$status == 0) %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c("strata", "time"))))%>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata")))) %>%
    dplyr::mutate(
      n.censor = dplyr::row_number()
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata", "time")))) %>%
    dplyr::mutate(
      n.censor = max(.data$n.censor)
    ) %>%
    dplyr::slice(rep(1:n(), each = length(x$failcode))) %>%
    dplyr::mutate(
      status = rep(1:length(x$failcode),each = n()/length(x$failcode))
    ) %>%
    dplyr::mutate(
      outcome =
        dplyr::recode(.data$status, !!!(as.list(names(x$failcode)) %>% stats::setNames(unlist(x$failcode))))
    ) %>%
    dplyr::select(-.data$status) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  df_time_zero <-
    df_Surv %>%
    dplyr::select(dplyr::any_of(c("strata", "status"))) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata")))) %>%
    dplyr::mutate(
      time = 0,
      n.event = 0,
      n.risk = dplyr::n(),
      n.censor = 0,
      outcome =
        dplyr::recode(
          .data$status,
          !!!(as.list(names(x$failcode)) %>% stats::setNames(unlist(x$failcode))),
          .default = NA_character_)
    ) %>%
    dplyr::filter(.data$status != 0) %>%
    dplyr::select(-.data$status) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()


  # some attempt to merge matrices more effectively (not successful yet)

  # event_mat <- merge(df_tidy,df_n_event)
  censor_mat <- merge(df_n_event,df_n_censor,all=TRUE)
  full_mat <- merge(censor_mat,df_n_risk,all=TRUE)
  full_mat <- merge(full_mat,df_time_zero,all=TRUE)
  full_mat <- full_mat %>%
    tidyr::fill(n.censor)

  # standardize decimal points of time between df_tidy and other tables.
  full_mat$time <- round(full_mat$time,8)
  df_tidy$time <- round(df_tidy$time,8)

  output_mat <- merge(df_tidy,full_mat,all=TRUE)

  list(df_tidy, full_mat) %>%
    purrr::reduce(
      ~suppressMessages(dplyr::full_join(.x, .y))
    ) %>%
    dplyr::rows_update(
      df_time_zero,
      by = intersect(c("outcome", "strata", "time"), names(df_time_zero))
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c("strata", "outcome", "time")))) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("strata", "outcome")))) %>%
    tidyr::fill(.data$n.risk, .data$estimate, .data$std.error,
                .data$conf.low, .data$conf.high,
                .data$n.event, .data$n.censor, .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$outcome)) %>%
    dplyr::distinct()
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
