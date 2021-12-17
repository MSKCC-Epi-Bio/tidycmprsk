test_that("base methods", {
  crr1 <- crr(Surv(ttdeath, death_cr) ~ age, trial)

  expect_error(
    print(crr1),
    NA
  )

  expect_error(
    model.matrix(crr1),
    NA
  )

  expect_error(
    model.frame(crr1),
    NA
  )

  expect_error(
    predict(crr1, times = c(10, 15, 16)),
    NA
  )
  expect_error(
    predict(crr1, times = 20),
    NA
  )
  expect_error(
    predict(crr1, probs = c(0.10, 0.15)),
    NA
  )
  expect_error(
    predict(crr1, probs = 0.10),
    NA
  )
  expect_error(
    coef(crr1),
    NA
  )
  expect_error(
    terms(crr1),
    NA
  )
  expect_error(predict(crr1, times = -20))
  expect_error(predict(crr1, probs = -20))
  expect_error(predict(crr1))
})



