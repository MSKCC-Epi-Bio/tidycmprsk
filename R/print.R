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
  cat("\n")

  x$tidy %>%
    dplyr::mutate(
      dplyr::across(c(.data$estimate, .data$conf.low, .data$conf.high), exp),
      dplyr::across(c(where(is.numeric), -.data$std.error, -.data$p.value),
                    ~gtsummary::style_ratio(., digits = 2)),
      std.error = gtsummary::style_sigfig(.data$std.error, digits = 3),
      p.value = gtsummary::style_pvalue(.data$p.value, digits = 2),
      conf.int = paste(conf.low, conf.high, sep = ", "),
      dplyr::across(where(is.character), ~dplyr::if_else(is.na(.), "", .))
    ) %>%
    select(-.data$statistic, -.data$conf.low, -.data$conf.high) %>%
    tibble::add_row(
      term = "Variable",
      estimate = "HR",
      std.error = "SE",
      conf.int = "95% CI",
      p.value = "p-value",
      .before = 1
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~stringr::str_pad(., side = "right", width = max(nchar(.)) + 3L)),
      dplyr::across(everything(),
                    ~ifelse(dplyr::row_number() == 1L,
                            cli::style_underline(.) %>% cli::style_italic(), .)),
      term =
        ifelse(dplyr::row_number() > 1L, cli::style_bold(.data$term), .data$term)
    ) %>%
    dplyr::relocate(.data$conf.int, .before = .data$p.value) %>%
    {purrr::walk(
      seq_len(nrow(.)),
      function(.x) .[.x, ] %>% unlist() %>% paste(collapse = "") %>% cat("\n")
    )}

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
