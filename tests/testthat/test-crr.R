test_that("crr() works", {
  expect_error(
    crr(Surv(ttdeath, death_cr) ~ age, trial),
    NA
  )
})
