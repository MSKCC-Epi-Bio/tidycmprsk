test_that("broom methods", {
  crr1 <- crr(Surv(ttdeath, death_cr) ~ age + grade, trial)

  expect_error(
    augment(crr1, times = c(10, 12)),
    NA
  )
  expect_error(
    glance(crr1),
    NA
  )
  expect_error(
    tidy(crr1),
    NA
  )
  expect_error(
    tidy(crr1, conf.int = TRUE),
    NA
  )
})
