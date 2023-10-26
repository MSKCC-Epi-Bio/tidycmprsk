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
    crr(Surv(ttdeath, death_cr) ~ age + trt,
        data = trial %>% dplyr::mutate(trt = factor(trt, levels = paste("Drug", c("A", "B", "C"))))) %>%
      utils::modifyList(val = list(data = NULL, blueprint = NULL, xlevels = NULL)),
    crr(Surv(ttdeath, death_cr) ~ age + trt,
        data = trial %>% dplyr::mutate(trt = factor(trt))) %>%
      utils::modifyList(val = list(data = NULL, blueprint = NULL, xlevels = NULL))
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

  # `crr()` works with no observed censoring
  expect_error(
    crr_no_censor<-
      crr(
        Surv(ttdeath, death_cr) ~ trt,
        data = trial %>% dplyr::filter(!death_cr %in% "censor")
      ),
    NA
  )
  expect_equal(
    cmprsk::crr(
      ftime = trial$ttdeath[!trial$death_cr %in% "censor"],
      fstatus = as.numeric(trial$death_cr[!trial$death_cr %in% "censor"]) - 1L,
      cov1 = model.matrix(~., data = trial[!trial$death_cr %in% "censor", "trt"])[, -1, drop = FALSE]
    )[1:6],
    crr_no_censor$cmprsk[1:6]
  )
})
