test_that("base methods", {
  cuminc2 <- cuminc(Surv(ttdeath, death_cr) ~ trt, trial)

  expect_error(
    print(cuminc2),
    NA
  )

  expect_error(
    model.matrix(cuminc2),
    NA
  )

  expect_error(
    model.frame(cuminc2),
    NA
  )
})
