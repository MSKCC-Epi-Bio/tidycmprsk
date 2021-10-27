test_that("cuminc() works", {
  expect_error(
    cuminc1 <- cuminc(Surv(ttdeath, death_cr) ~ 1, trial),
    NA
  )

  expect_error(
    cuminc2 <- cuminc(Surv(ttdeath, death_cr) ~ trt, trial),
    NA
  )

  # test base methods
  expect_equal(
    model.frame(cuminc1),
    model.frame(Surv(ttdeath, death_cr) ~ 1, data = trial),
    ignore_attr = TRUE
  )
  expect_equal(
    model.frame(cuminc2),
    model.frame(Surv(ttdeath, death_cr) ~ trt, data = trial),
    ignore_attr = TRUE
  )
})
