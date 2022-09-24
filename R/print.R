#' Print crr object
#'
#' @param x tidycrr object
#' @param ... not used
#' @name print
#' @keywords internal
NULL

#' @rdname print
#' @export
print.tidycrr <- function(x, ...) {
  cli::cli_h1("crr()")
  cli::cli_li("Call {.field {deparse(x$formula)}}")
  cli::cli_li("Failure type of interest {.val {names(x$failcode)}}")
  cat("\n")

  x$tidy %>%
    # adding the HR with CI to data frame
    dplyr::bind_cols(
      select(., dplyr::all_of(c("estimate", "conf.low", "conf.high"))) %>%
        mutate(
          across(
            dplyr::all_of(c("estimate", "conf.low", "conf.high")),
            ~ gtsummary::style_ratio(exp(.), digits = 2)
          ),
          exp_estimate = .data$estimate,
          conf.int = paste(.data$conf.low, .data$conf.high, sep = ", ")
        ) %>%
        select(dplyr::all_of(c("exp_estimate", "conf.int")))
    ) %>%
    # formatting coef and p-value
    mutate(
      across(
        dplyr::all_of(c("estimate", "std.error")),
        purrr::partial(gtsummary::style_sigfig, digits = 3)
      ),
      p.value = gtsummary::style_pvalue(.data$p.value, digits = 2)
    ) %>%
    select(dplyr::all_of(c(
      "term", "estimate", "std.error", "exp_estimate", "conf.int", "p.value"
    ))) %>%
    # adding header row
    tibble::add_row(
      term = "Variable",
      estimate = "Coef",
      std.error = "SE",
      exp_estimate = "HR",
      conf.int = paste0(x$conf.level * 100, "% CI"),
      p.value = "p-value",
      .before = 1
    ) %>%
    # styling the values that will be printed
    mutate(
      across(
        where(is.character),
        ~ stringr::str_pad(., side = "right", width = max(nchar(.)) + 3L)
      ),
      across(
        everything(),
        ~ ifelse(dplyr::row_number() == 1L,
          cli::style_underline(.) %>% cli::style_italic(), .
        )
      ),
      term =
        ifelse(dplyr::row_number() > 1L, cli::style_bold(.data$term), .data$term)
    ) %>%
    {
      purrr::walk(
        seq_len(nrow(.)),
        function(.x) {
          .[.x, ] %>%
            unlist() %>%
            paste(collapse = "") %>%
            cat("\n")
        }
      )
    }

  invisible()
}

#' @rdname print
#' @export
print.tidycuminc <- function(x, ...) {
  cli::cli_h1("cuminc()")

  # selecting times to report
  times <-
    pretty(x$tidy$time) %>%
    purrr::discard(~ .x <= 0 | .x > max(x$tidy$time, na.rm = TRUE))

  # getting summaries at specified timepoints
  df_tidy <- tidy(x, times = times)

  # printing estimates for each outcome
  unique(df_tidy$outcome) %>%
    purrr::walk(
      function(outcome) {
        cat("\n")
        cli::cli_li("Failure type {.val {outcome}}")

        df_tidy %>%
          # filter on the outcome of interest
          filter(.data$outcome %in% .env$outcome) %>%
          select(any_of(c(
            "outcome", "strata", "time", "n.risk",
            "estimate", "std.error",
            "conf.low", "conf.high"
          ))) %>%
          # round all stats
          mutate(
            # round whole numbers to nearest integer
            across(gtsummary::any_of(c("n.risk")), gtsummary::style_number),
            # round all other stats to 3 sig figs
            across(
              where(is.numeric),
              ~ gtsummary::style_sigfig(., digits = 3)
            ),
            # NA will be shown as "NA" in output
            across(where(is.character), ~ tidyr::replace_na(., "NA")),
            conf.int = paste(.data$conf.low, .data$conf.high, sep = ", "),
            .after = dplyr::all_of("std.error")
          ) %>%
          dplyr::rename("{x$conf.level * 100}% CI" := dplyr::all_of("conf.int")) %>%
          select(-dplyr::all_of(c("outcome", "conf.low", "conf.high"))) %>%
          # add header row
          {
            tibble::add_row(
              .data = .,
              !!!stats::setNames(as.list(names(.)), names(.)),
              .before = 0L
            )
          } %>%
          # add cli styling to the header row
          mutate(
            across(
              where(is.character),
              ~ stringr::str_pad(., side = "right", width = max(nchar(.)) + 3L)
            ),
            across(
              everything(),
              ~ ifelse(dplyr::row_number() == 1L,
                cli::style_underline(.) %>% cli::style_italic(), .
              )
            )
          ) %>%
          # print results table
          {
            purrr::walk(
              seq_len(nrow(.)),
              function(.x) {
                .[.x, ] %>%
                  unlist() %>%
                  paste(collapse = "") %>%
                  cat("\n")
              }
            )
          }
      }
    )

  if (!is.null(x$cmprsk$Tests)) {
    cat("\n")
    cli::cli_li("Tests")
    df_glance <- glance(x)

    nrow(x$cmprsk$Tests) %>%
      seq_len() %>%
      purrr::map(
        function(i) {
          df_glance %>%
            select(dplyr::ends_with(paste0("_", i))) %>%
            dplyr::rename_with(
              ~ stringr::str_replace(., stringr::fixed(paste0("_", i)), "")
            )
        }
      ) %>%
      dplyr::bind_rows() %>%
      mutate(
        p.value = gtsummary::style_pvalue(.data$p.value, digits = 2),
        across(where(is.numeric), ~ gtsummary::style_sigfig(., digits = 3))
      ) %>%
      # add header row
      {
        tibble::add_row(
          .data = .,
          !!!stats::setNames(as.list(names(.)), names(.)),
          .before = 0L
        )
      } %>%
      # add cli styling to the header row
      mutate(
        across(
          where(is.character),
          ~ stringr::str_pad(., side = "right", width = max(nchar(.)) + 3L)
        ),
        across(
          everything(),
          ~ ifelse(dplyr::row_number() == 1L,
            cli::style_underline(.) %>% cli::style_italic(), .
          )
        )
      ) %>%
      # print results table
      {
        purrr::walk(
          seq_len(nrow(.)),
          function(.x) {
            .[.x, ] %>%
              unlist() %>%
              paste(collapse = "") %>%
              cat("\n")
          }
        )
      }
  }
}
