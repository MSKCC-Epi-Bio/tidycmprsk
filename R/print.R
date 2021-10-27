#' Print crr object
#'
#' @param x tidycrr obejct
#' @param ... not used
#' @name print
NULL

#' @rdname print
#' @export
print.tidycrr <- function(x, ...) {
  cli::cli_h1("crr()")
  cli::cli_li("Call {.field {deparse(x$formula)}}")
  cli::cli_li("Failure type of interest {.val {names(x$failcode)}}")

  cat("\nFine and Gray's model fit: \n")
  df_print <-
    crr_mod %>%
    broom.helpers::tidy_and_attach(
      model = x,
      tidy_fun = tidy,
      exponentiate = TRUE,
      conf.int = TRUE
    ) %>%
    broom.helpers::tidy_add_reference_rows() %>%
    broom.helpers::tidy_add_header_rows() %>%
    dplyr::mutate(
      dplyr::across(c(where(is.numeric), -.data$p.value), ~gtsummary::style_sigfig(., digits = 3)),
      p.value = gtsummary::style_pvalue(.data$p.value, digits = 2),
      dplyr::across(where(is.character), ~dplyr::if_else(is.na(.), "", .)),
      dplyr::across(where(is.character),
                    ~ifelse(reference_row %in% TRUE & . == "", "\U2014", .))
    )

  df_print_padded <-
    df_print %>%
    tibble::add_row(label = "Variable",
                    estimate = "HR",
                    std.error = "SE",
                    conf.low = "Low",
                    conf.high = "High",
                    p.value = "P",
                    .before = 1)

  for (v in names(df_print_padded)) {
    if (is.character(df_print_padded[[v]]))
      df_print_padded[[v]] <-
        stringr::str_pad(df_print_padded[[v]],
                         side = "right",
                         width = max(nchar(df_print_padded[[v]])) + 1L)
  }

  df_print_padded2 <-
    df_print_padded %>%
    dplyr::mutate(
      dplyr::across(everything(),
                    ~ifelse(dplyr::row_number() == 1L,
                            cli::style_underline(.) %>% cli::style_italic(), .)),
      cli_label =
        ifelse(
          header_row %in% c(NA, TRUE) & dplyr::row_number() > 1,
          cli::style_bold(label) %>% cli::col_black(),
          label
        ),
      cli_label =
        ifelse(
          header_row %in% FALSE & dplyr::row_number() > 1,
          cli::style_italic(label) %>% cli::col_blue(),
          cli_label
        )
    ) %>%
    dplyr::select(.data$cli_label, .data$estimate, .data$std.error,
                  .data$conf.low, .data$conf.high, .data$p.value)

  for (i in seq_len(nrow(df_print_padded2))) {
    df_print_padded2[i,] %>% unlist() %>% paste(collapse = "") %>% cat("\n")
  }

  invisible()
}

#' @rdname print
#' @export
print.tidycuminc <- function(x, ...) {
  cli::cli_h1("cuminc()")
  cli::cli_li("Call {.field {deparse(x$formula)}}")
  cli::cli_li("Failure type of interest {.val {names(x$failcode)}}")

  print(x$original_fit)
}
