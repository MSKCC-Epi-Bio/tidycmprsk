## code to prepare `DATASET` dataset goes here

# adding a competing risks outcome to the gtsummary::trial dataset
set.seed(1123)
trial <-
  gtsummary::trial %>%
  dplyr::mutate(
    death_cr = dplyr::case_when(
      death == 0 ~ "censor",
      stats::runif(dplyr::n()) < 0.5 ~ "death from cancer",
      TRUE ~ "death other causes"
    ) %>%
      factor(),
    .after = death,
    death_cr_num = as.numeric(death_cr) - 1L
  ) %>%
  labelled::set_variable_labels(death_cr = "Death Status",
                                death_cr_num = "Death Status (numeric)")

usethis::use_data(trial, overwrite = TRUE)
