test_that("autoplot() works", {
  expect_error(
    cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
      autoplot(),
    NA
  )

  expect_error(
    cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>%
      autoplot(outcomes = "death from cancer", conf.int = TRUE),
    NA
  )
})
