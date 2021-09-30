test_that("crr() works", {
  expect_error(
    crr(crSurv(ttdeath, death_cr_num) ~ age, trial),
    NA
  )
})
