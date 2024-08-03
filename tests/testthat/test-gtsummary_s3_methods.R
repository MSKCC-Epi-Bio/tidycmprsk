test_that("tbl_regression.tidycrr()", {
  expect_snapshot(
    crr(Surv(ttdeath, death_cr) ~ age + grade, trial) |>
      tbl_regression() |>
      as.data.frame()
  )
})

test_that("global_pvalue_fun.tidycrr()", {
  mod <- crr(Surv(ttdeath, death_cr) ~ age + grade, trial)
  expect_error(
    tbl <- mod |>
      tbl_regression() |>
      gtsummary::add_global_p(),
    NA
  )
  expect_snapshot(tbl |> as.data.frame(col_labels = FALSE))

  expect_equal(
    tbl$table_body$p.value[1:2],
    cardx::ard_aod_wald_test(mod) |>
      dplyr::filter(stat_name %in% "p.value") |>
      dplyr::pull(stat) |>
      unlist()
  )
})
