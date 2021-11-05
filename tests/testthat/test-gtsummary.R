test_that("gtsummary tables work with crr() output", {
  expect_error(
    crr(Surv(ttdeath, death_cr) ~ age + trt, trial) %>%
      gtsummary::tbl_regression(),
    NA
  )
  expect_error(
    crr(Surv(ttdeath, death_cr) ~ age + ordered(grade), trial) %>%
      gtsummary::tbl_regression(),
    NA
  )
  expect_error(
    crr(Surv(ttdeath, death_cr) ~ age, trial) %>%
      gtsummary::tbl_regression(),
    NA
  )
  expect_error(
    crr(Surv(ttdeath, death_cr) ~ trt, trial) %>%
      gtsummary::tbl_regression(),
    NA
  )

  trial_contr <-
    trial
  contrasts(trial_contr$grade) <-  contr.sum(3)
  contrasts(trial_contr$stage) <-
    contr.treatment(attr(trial_contr$stage, "levels"), 3)

  expect_error(
    contr_sum <-
      crr(Surv(ttdeath, death_cr) ~ grade, trial_contr) %>%
      gtsummary::tbl_regression(),
    NA
  )
  expect_equal(
    unique(contr_sum$table_body$contrasts_type),
    "sum"
  )

  expect_error(
    contr_treatment <-
      crr(Surv(ttdeath, death_cr) ~ stage, trial_contr) %>%
      gtsummary::tbl_regression(),
    NA
  )
  expect_equal(
    unique(contr_treatment$table_body$contrasts_type),
    "treatment"
  )
  expect_equal(
    contr_treatment$table_body %>%
      dplyr::filter(reference_row) %>%
      dplyr::pull(label),
    "T3",
    ignore_attr = TRUE
  )
})
