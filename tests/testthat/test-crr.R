test_that("crr() works", {
  cmprsk_crr1 <-
    cmprsk::crr(
      ftime = trial$ttdeath,
      fstatus = as.numeric(trial$death_cr) - 1L,
      cov1 = as.matrix(trial[["age"]]),
      failcode = 1
    )

  expect_error(
    crr1 <- crr(Surv(ttdeath, death_cr) ~ age, data = trial, conf.level = 0.90),
    NA
  )

  expect_equal(
    tidy(crr1, conf.int = TRUE) %>%
      select(all_of(names(.) %>% sort())),
    broom::tidy(crr1$cmprsk, conf.int = TRUE, conf.level = 0.90) %>%
      select(all_of(names(.) %>% sort()))
  )
  expect_equal(
    glance(crr1),
    broom::glance(crr1$cmprsk)
  )
  expect_equal(
    summary(crr1$cmprsk)$coef %>% unname(),
    summary(cmprsk_crr1)$coef %>% unname()
  )
  expect_equal(
    summary(crr1$cmprsk)$conf.int %>% unname(),
    summary(cmprsk_crr1)$conf.int %>% unname()
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
