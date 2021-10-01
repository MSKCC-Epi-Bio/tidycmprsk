# A function that, given named arguments, will make a one-row
# tibble, switching out NULLs for the appropriate NA type.
#' @importFrom purrr map2
#' @importFrom tibble as_tibble_row
#' @importFrom stringr str_split
as_glance_tibble <- function(..., na_types) {

  cols <- list(...)

  if (length(cols) != stringr::str_length(na_types)) {
    stop(
      "The number of columns provided does not match the number of ",
      "column types provided."
    )
  }

  na_types_long <- parse_na_types(na_types)

  entries <- purrr::map2(cols,
                         na_types_long,
                         function(.x, .y) {if (length(.x) == 0) .y else .x})

  tibble::as_tibble_row(entries)

}

parse_na_types <- function(s) {

  positions <- purrr::map(
    stringr::str_split(s, pattern = ""),
    match,
    table = names(na_types_dict)
  ) %>%
    unlist()

  na_types_dict[positions] %>%
    unlist() %>%
    unname()
}

na_types_dict <- list("r" = NA_real_,
                      "i" = rlang::na_int,
                      "c" = NA_character_,
                      "l" = rlang::na_lgl)
