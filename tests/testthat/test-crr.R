test_that("crr() works", {
  expect_error(
    crr1 <- crr(Surv(ttdeath, death_cr) ~ age, trial),
    NA
  )

  expect_equal(
    tidy(crr1),
    broom::tidy(crr1$cmprsk)
  )
  expect_equal(
    glance(crr1),
    broom::glance(crr1$cmprsk)
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
    predict(crr1, times = c(10, 15)),
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
