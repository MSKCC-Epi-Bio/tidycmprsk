#' Tabular Summary of Cumulative Incidence
#'
#' @param x a 'tidycuminc' object created with `cuminc()`
#' @param outcomes character vector of outcomes to include. Default
#' is to include the first outcome.
#' @param statistic string of statistic to report. Default is
#' `"{estimate}% ({conf.low}%, {conf.high}%)"`
#' @param estimate_fun function that styles and formats the statistics.
#' Default is `~gtsummary::style_sigfig(.x, scale = 100)`
#' @param label string indicating the variable label
#' @param label_header string for the header labels; uses glue syntax.
#' Default is `"**Time {time}**"`
#' @param missing string to replace missing values with. Default is an
#' em-dash, `"\U2014"`
#' @inheritParams tidy.tidycuminc
#' @inheritParams rlang::args_dots_empty
#'
#' @name tbl_cuminc
#' @family tbl_cuminc tools
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' tbl_cuminc_ex1 <-
#'   cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>%
#'   tbl_cuminc(times = c(12, 24), label_header = "**Month {time}**")
#'
#' # Example 2 ----------------------------------
#' tbl_cuminc_ex2 <-
#'   cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   tbl_cuminc(times = c(12, 24),
#'              outcomes = c("death from cancer", "death other causes"),
#'              label_header = "**Month {time}**")
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_cuminc_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_cuminc_ex2.png}{options: width=60\%}}
NULL

#' @export
#' @rdname tbl_cuminc
tbl_cuminc.tidycuminc <- function(x,
                                  times = NULL,
                                  outcomes = NULL,
                                  statistic = "{estimate}% ({conf.low}%, {conf.high}%)",
                                  label = NULL,
                                  label_header = "**Time {time}**",
                                  estimate_fun = NULL,
                                  conf.level = x$conf.level,
                                  missing = NULL,
                                  ...) {
  # check inputs ---------------------------------------------------------------
  rlang::check_dots_empty()
  if (!rlang::is_string(statistic)) {
    cli::cli_abort("Argument {.arg statistic} must be a string.")
  }
  if (!rlang::is_string(label_header)) {
    cli::cli_abort("Argument {.arg label_header} must be a string.")
  }
  if (!is.null(label) && !rlang::is_string(label)) {
    cli::cli_abort("Argument {.arg label} must be a string.")
  }
  if (any(!outcomes %in% names(x$failcode))) {
    cli::cli_abort(c(
      "!" = "Error in {.arg outcomes} argument specification.",
      "i" = "Must be one or more of {.val {names(x$failcode)}}."
    ))
  }
  func_inputs <- as.list(environment())
  missing <- missing %||% "\U2014"

  # setting defaults -----------------------------------------------------------
  outcomes <- outcomes %||% names(x$failcode)[1]
  times <- times %||% (setdiff(pretty(x$tidy$time, n = 2), 0) %>% purrr::discard(~ . > max(x$tidy$time)))
  estimate_fun <-
    estimate_fun %||%
    (function(x) gtsummary::style_sigfig(x, scale = 100)) %>%
    purrr::as_mapper()

  # creating a default label
  rhs_variables <- rlang::f_rhs(x$formula) %>% all.vars()
  if (is.null(label)) {
    label <-
      ifelse(
        rlang::is_empty(rhs_variables),
        "Overall",
        purrr::map(rhs_variables, ~attr(x$data[[.x]], "label") %||% .x) %>%
          paste(collapse = ".")
      )
  }

  # calculate estimates --------------------------------------------------------
  df_tidy <-
    tidy(x, times = times, conf.level = conf.level) %>%
    filter(.data$outcome %in% .env$outcomes) %>%
    select(any_of(c("time", "outcome", "strata", "estimate", "conf.low", "conf.high"))) %>%
    mutate(
      across(any_of(c("estimate", "conf.low", "conf.high")),
             ~ifelse(is.na(.), missing, estimate_fun(.))),
      label_header = stringr::str_glue(label_header),
      statistic = stringr::str_glue(statistic)
    ) %>%
    select(any_of(c("label_header", "time", "outcome", "strata", "statistic"))) %>%
    dplyr::group_by(across(any_of(c("outcome", "strata")))) %>%
    mutate(column_name = paste("stat", dplyr::row_number(), sep = "_")) %>%
    dplyr::ungroup()

  # calculate Ns ---------------------------------------------------------------
  df_n <-
    suppressMessages(
      dplyr::full_join(
        tidy(x, times = 0) %>%
          select(any_of(c("outcome", "strata")),
                 n = "n.risk"),
        tidy(x, times = max(x$tidy$time) + 1) %>%
          select(any_of(c("outcome", "strata")),
                 n.event = "cum.event")
      )
    ) %>%
    dplyr::group_by(.data$outcome) %>%
    mutate(
      N = sum(.data$n),
      N.event = sum(.data$n.event),
      .before = dplyr::all_of("n")
    ) %>%
    dplyr::ungroup()

  # combine results ------------------------------------------------------------
  table_body <-
    df_tidy %>%
    select(dplyr::any_of(c("outcome", "strata", "column_name", "statistic"))) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::any_of(c("outcome", "strata")),
      names_from = "column_name",
      values_from = "statistic"
    ) %>%
    mutate(row_type = "level")

  if ("strata" %in% names(table_body)) {
    # add a header row for stratified tables and the label column
    table_body <- table_body %>%
      mutate(label = .data$strata, .before = 1) %>%
      tibble::add_row(
        outcome = unique(.$outcome),
        label = .env$label,
        row_type = "label",
        .before = 1L
      )
  } else {
    # otherwise, just add the label column
    table_body <- table_body %>%
      mutate(row_type = "label", label = .env$label, .before = 1)
  }

  if (length(unique(table_body$outcome)) > 1) {
    table_body <- table_body %>%
      mutate(groupname_col = .data$outcome, .before = 1)
  }

  table_body <- table_body %>%
    # merge in the Ns and event Ns
    {suppressMessages(
      dplyr::left_join(
        .,
        select(df_n, dplyr::all_of("outcome"), dplyr::starts_with("N", ignore.case = FALSE)) %>% dplyr::distinct()
      )
    )} %>%
    {suppressMessages(
      dplyr::left_join(
        .,
        select(df_n, dplyr::any_of(c("outcome", "strata")), dplyr::starts_with("n", ignore.case = FALSE)) %>% dplyr::distinct()
      )
    )} %>%
    # adding standard gtsummary columns to the tbl
    dplyr::arrange(.data$outcome, dplyr::desc(.data$row_type == "label")) %>%
    mutate(
      variable =
        ifelse(
          rlang::is_empty(rhs_variables),
          "..overall..",
          paste(rhs_variables, collapse = ".")
        ),
      var_type = "categorical",
      var_label = .env$label,
      .before = 1
    ) %>%
    # re-arrange variables
    select(dplyr::any_of(c("outcome", "groupname_col", "variable",
                           "var_type", "row_type", "var_label",
                           "N", "n", "N.event", "n.event",
                           "strata", "label")),
           dplyr::everything())

  # extract column headers -----------------------------------------------------
  column_headers <-
    dplyr::distinct(df_tidy[c("column_name", "label_header")]) %>%
    tibble::deframe() %>%
    as.list()

  # prepping gtsummary object --------------------------------------------------
  result <-
    gtsummary::.create_gtsummary_object(
      table_body = table_body,
      inputs = func_inputs,
      tidy = df_tidy
    ) %>%
    gtsummary::modify_table_styling(
      columns = c("label", names(column_headers)),
      label = c("**Characteristic**", unname(unlist(column_headers))),
      hide = rep_len(FALSE, length(column_headers) + 1)
    ) %>%
    gtsummary::modify_table_styling(
      columns = dplyr::any_of("groupname_col"),
      label = "**Group**",
      hide = FALSE
    )

  # assign class and return tbl ------------------------------------------------
  result %>% structure(class = c("tbl_cuminc", "gtsummary"))
}

#' @export
#' @rdname tbl_cuminc
tbl_cuminc <- function(x, ...) {
  UseMethod("tbl_cuminc")
}

#' Additional Functions for `tbl_cuminc()`
#'
#' @description
#'
#' - `add_p()` Add column with p-value comparing incidence across stratum
#' - `add_n()` Add column with the total N, or N within stratum
#' - `add_nevent()` Add column with the total number of events, or number of events within stratum
#' - `inline_text()` Report statistics from a `tbl_cuminc()` table inline
#'
#' @param x object of class 'tbl_cuminc'
#' @param pvalue_fun function to style/format p-values. Default is
#' `gtsummary::style_pvalue`
#' @param location location to place Ns. When `"label"` total Ns are placed
#' on each variable's label row. When `"level"` level counts are placed on
#' the variable level for categorical variables, and total N on the
#' variable's label row for continuous.
#' @param time time of statistic to report
#' @param column column name of the statistic to report
#' @param level if estimates are stratified, level of the stratum to report
#' @param outcome string indicating the outcome to select from. If `NULL`, the
#' first outcome is used.
#' @inheritParams rlang::args_dots_empty
#'
#' @name add_cuminc
#' @family tbl_cuminc tools
#' @examples
#' # Example 1 ----------------------------------
#' add_cuminc_ex1 <-
#'   cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>%
#'   tbl_cuminc(times = c(12, 24), label_header = "**Month {time}**") %>%
#'   add_nevent() %>%
#'   add_n()
#'
#' # Example 2 ----------------------------------
#' add_cuminc_ex2 <-
#'   cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   tbl_cuminc(times = c(12, 24),
#'              outcomes = c("death from cancer", "death other causes"),
#'              label_header = "**Month {time}**") %>%
#'   add_p() %>%
#'   add_nevent(location = c("label", "level")) %>%
#'   add_n(location = c("label", "level"))
#'
#' # inline_text() ------------------------------
#' inline_text(add_cuminc_ex2, time = 12, level = "Drug A")
#' inline_text(add_cuminc_ex2, column = p.value)
#'
#' @inheritSection cuminc p-values
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_cuminc_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_cuminc_ex2.png}{options: width=60\%}}
NULL

#' @export
#' @rdname add_cuminc
add_p.tbl_cuminc <- function(x, pvalue_fun = gtsummary::style_pvalue, ...) {
  rlang::check_dots_empty()
  if ("..overall.." %in% x$table_body$variable) {
    stop("Cannot add a p-value without a stratifying variable.", call. = FALSE)
  }
  if (!is.function(pvalue_fun)) {

  }

  # reformat glance to long (one line per outcome)
  df_glance <-
    glance(x$inputs$x) %>%
    tidyr::pivot_longer(
      everything(),
      names_to = c(".value", "outcome_id"),
      names_pattern = "(.*)_(.*)"
    ) %>%
    mutate(row_type = "label") %>%
    select(-dplyr::all_of("outcome_id"))

  # merge p-values into tbl
  x %>%
    gtsummary::modify_table_body(
      ~ .x %>%
        dplyr::left_join(
          df_glance,
          by = c("outcome", "row_type")
        )
    ) %>%
    gtsummary::modify_table_styling(
      columns = "p.value",
      hide = FALSE,
      label = "**p-value**",
      footnote = "Gray's Test",
      fmt_fun = pvalue_fun
    ) %>%
    gtsummary::modify_table_styling(
      columns = "statistic",
      label = "**Statistic**",
      fmt_fun = gtsummary::style_sigfig
    ) %>%
    gtsummary::modify_table_styling(
      columns = "df",
      label = "**df**",
      fmt_fun = gtsummary::style_number
    )
}

#' @export
#' @rdname add_cuminc
add_n.tbl_cuminc <- function(x, location = NULL, ...) {
  rlang::check_dots_empty()
  if (is.null(location)) location <- "label"
  location <- match.arg(location, choices = c("label", "level"), several.ok = TRUE)

  x %>%
    gtsummary::modify_table_body(
      ~ .x %>%
        mutate(
          stat_n = NA_integer_,
          stat_n =
            ifelse(.data$row_type == "label" & "label" %in% .env$location,
                   .data$N,
                   .data$stat_n),
          stat_n =
            ifelse(
              .data$row_type == "level" & "level" %in% .env$location,
              .data$n,
              .data$stat_n),
          .after = dplyr::all_of("label")
        )
    ) %>%
    gtsummary::modify_table_styling(
      columns = "stat_n",
      label = "**N**",
      hide = FALSE,
      fmt_fun = gtsummary::style_number
    )
}

#' @export
#' @rdname add_cuminc
add_nevent.tbl_cuminc <- function(x, location = NULL, ...) {
  rlang::check_dots_empty()
  if (is.null(location)) location <- "label"
  location <- match.arg(location, choices = c("label", "level"), several.ok = TRUE)

  x %>%
    gtsummary::modify_table_body(
      ~ .x %>%
        mutate(
          stat_nevent = NA_integer_,
          stat_nevent =
            ifelse(.data$row_type == "label" & "label" %in% .env$location,
                   .data$N.event,
                   .data$stat_nevent),
          stat_nevent =
            ifelse(
              .data$row_type == "level" & "level" %in% .env$location,
              .data$n.event,
              .data$stat_nevent),
          .after = dplyr::all_of("label")
        )
    ) %>%
    gtsummary::modify_table_styling(
      columns = "stat_nevent",
      label = "**N Event**",
      hide = FALSE,
      fmt_fun = gtsummary::style_number
    )
}


#' @export
#' @rdname add_cuminc
inline_text.tbl_cuminc <- function(x,
                                   time = NULL,
                                   column = NULL,
                                   outcome = NULL,
                                   level = NULL, ...) {
  rlang::check_dots_empty()
  column <- rlang::enquo(column)
  column_is_null <-
    tryCatch(rlang::quo_is_null(column) || is.null(rlang::eval_tidy(column)),
             error = function(e) FALSE)

  if (is.null(time) + column_is_null != 1L) {
    stop("Specify only one of `time=` or `column=`.", call. = FALSE)
  }

  if (!is.null(time)) {
    if (!time %in% x$tidy$time) {
      cli::cli_alert_danger("{.code time=} must be one of {.val {unique(x$tidy$time)}}")
      stop("Error in `time=` argument specification.", call. = FALSE)
    }
    column <- filter(x$tidy, .data$time %in% .env$time)$column_name[1]
  }

  # subset on the outcome of interest
  if (is.null(outcome)) outcome <- names(x$inputs$x$failcode)[1]
  outcome <- match.arg(outcome, choices = names(x$inputs$x$failcode))
  x$table_body <- filter(x$table_body, .data$outcome %in% .env$outcome)

  gtsummary::inline_text(
    x = structure(x, class = "gtsummary"), # forcing gtsummary method
    variable = x$table_body$variable[1],
    level = {{ level }},
    column = !!column
  )
}
