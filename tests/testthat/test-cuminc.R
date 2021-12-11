test_that("cuminc() works", {
  cmprsk_cuminc1 <-
    cmprsk::cuminc(
      ftime = trial$ttdeath,
      fstatus = as.numeric(trial$death_cr) - 1L
    )
  cmprsk_cuminc2 <-
    cmprsk::cuminc(
      ftime = trial$ttdeath,
      fstatus = as.numeric(trial$death_cr) - 1L,
      group = trial$trt
    )

  expect_error(
    cuminc1 <- cuminc(Surv(ttdeath, death_cr) ~ 1, trial),
    NA
  )
  expect_equal(
    cuminc1$cmprsk,
    cmprsk_cuminc1
  )

  expect_error(
    cuminc2 <- cuminc(Surv(ttdeath, death_cr) ~ trt, trial),
    NA
  )
  expect_equal(
    cuminc2$cmprsk,
    cmprsk_cuminc2
  )

  expect_equal(
    names(cuminc1$failcode),
    c("death from cancer", "death other causes")
  )
  expect_equal(
    names(cuminc2$failcode),
    c("death from cancer", "death other causes")
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

test_that("broom methods", {
  cuminc1 <- cuminc(Surv(ttdeath, death_cr) ~ 1, trial)
  cuminc2 <- cuminc(Surv(ttdeath, death_cr) ~ trt, trial)
  tidy_survfit1_cancer <-
    survival::survfit(Surv(ttdeath, death_cr == "death from cancer") ~ 1, trial) %>%
    broom::tidy()
  tidy_survfit1_other <-
    survival::survfit(Surv(ttdeath, death_cr == "death from cancer") ~ 1, trial) %>%
    broom::tidy()
  tidy_survfit2_cancer <-
    survival::survfit(Surv(ttdeath, death_cr == "death from cancer") ~ trt, trial) %>%
    broom::tidy()
  tidy_survfit2_other <-
    survival::survfit(Surv(ttdeath, death_cr == "death from cancer") ~ trt, trial) %>%
    broom::tidy()

  expect_error(
    glance(cuminc2),
    NA
  )
  expect_error(
    cuminc2_tidy <- tidy(cuminc2),
    NA
  )
  expect_equal(
    cuminc2_tidy,
    cuminc2$tidy
  )

  expect_error(
    glance(cuminc1),
    NA
  )
  expect_error(
    cuminc1_tidy <- tidy(cuminc1),
    NA
  )
  expect_equal(
    cuminc1_tidy,
    cuminc1$tidy
  )

  # checking n.risk, n.event, and n.censor for a stratified estimate
  # checking tidycmprsk numbers against `survfit() %>% tidy()`
  survfit_check2 <-
    cuminc2_tidy %>%
    dplyr::mutate(strata = paste0("trt=", strata)) %>%
    dplyr::select(outcome, strata, time, n.risk, n.event) %>%
    dplyr::inner_join(
      tidy_survfit2_cancer %>%
        dplyr::select(strata, time, n.risk, n.event),
      by = c("strata", "time")
    )

  expect_equal(
    survfit_check2$n.risk.x,
    survfit_check2$n.risk.y
  )
  expect_equal(
    survfit_check2$n.event.x,
    survfit_check2$n.event.y
  )

  # checking n.risk, n.event, and n.censor for an  unstratified estimate
  # checking tidycmprsk numbers against `survfit() %>% tidy()`
  survfit_check1 <-
    cuminc1_tidy %>%
    dplyr::select(time, n.risk, n.event) %>%
    dplyr::inner_join(
      tidy_survfit1_cancer %>%
        dplyr::select(time, n.risk, n.event),
      by = c("time")
    )

  expect_equal(
    survfit_check1$n.risk.x,
    survfit_check1$n.risk.y
  )
  expect_equal(
    survfit_check1$n.event.x,
    survfit_check1$n.event.y
  )

  # all estimates fall within CI
  expect_true(
    cuminc2_tidy %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        check =
          dplyr::between(estimate, conf.low, conf.high) |
          (estimate == 0 & is.na(conf.low) & is.na(conf.high))
      ) %>%
      dplyr::pull(check) %>%
      all()
  )

  # when estimate is zero, the other estimates fall in line with that
  expect_true(
    cuminc2_tidy %>%
      dplyr::filter(estimate == 0) %>%
      dplyr::mutate(
        check =
          estimate == 0 & std.error == 0 &
          is.na(conf.low) & is.na(conf.high) &
          n.event == 0 & n.censor == 0
      ) %>%
      dplyr::pull(check) %>%
      all()
  )

  expect_error(
    cuminc_tidy2 <- tidy(cuminc2, conf.int = FALSE, times = c(0, 12)),
    NA
  )

  expect_error(
    cuminc_tidy1 <- tidy(cuminc1, conf.int = FALSE, times = c(0, 12)),
    NA
  )
  expect_equal(
    cuminc_tidy1$n.censor,
    rep_len(0L, 4)
  )

})
