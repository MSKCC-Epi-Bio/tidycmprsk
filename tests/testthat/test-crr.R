test_that("crr() works", {
  expect_error(
    crr1 <- crr(Surv(ttdeath, death_cr) ~ age, trial),
    NA
  )

  expect_equal(
    tidy(crr1),
    broom::tidy(crr1$original_fit)
  )
  expect_equal(
    glance(crr1),
    broom::glance(crr1$original_fit)
  )

  expect_error(
    as_numeric_failcode(Surv(NOT_A_VAR, death) ~ age, trial, NULL)
  )
  expect_error(
    as_numeric_failcode(Surv(ttdeath, death) ~ age, trial, NULL)
  )
  expect_error(
    as_numeric_failcode(Surv(ttdeath, death_cr) ~ age, trial, failcode = "NOT_A_LEVEL")
  )
  expect_error(
    crr(letters)
  )
})
