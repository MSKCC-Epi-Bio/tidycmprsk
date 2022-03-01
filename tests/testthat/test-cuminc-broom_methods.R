test_that("broom methods", {
  cuminc1 <- cuminc(Surv(ttdeath, death_cr) ~ 1, trial)
  cuminc2 <- cuminc(Surv(ttdeath, death_cr) ~ trt, trial)
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
  tidy_survfit1_cancer <-
    survival::survfit(Surv(ttdeath, death_cr == "death from cancer") ~ 1, trial) %>%
    broom::tidy()
  survfit1_cancer_times <- summary(survival::survfit(Surv(ttdeath, death_cr == "death from cancer") ~ 1, trial), times = c(0, 5, 10, 15, 20))
  tidy_survfit1_other <-
    survival::survfit(Surv(ttdeath, death_cr == "death from cancer") ~ 1, trial) %>%
    broom::tidy()
  tidy_survfit2_cancer <-
    survival::survfit(Surv(ttdeath, death_cr == "death from cancer") ~ trt, trial) %>%
    broom::tidy()
  tidy_survfit2_other <-
    survival::survfit(Surv(ttdeath, death_cr == "death from cancer") ~ trt, trial) %>%
    broom::tidy()

  tidy_survfit1_cancer_censor <-
    survival::survfit(Surv(ttdeath, death_cr != "censor") ~ 1, trial) %>%
    broom::tidy()
  tidy_survfit2_cancer_censor <-
    survival::survfit(Surv(ttdeath, death_cr != "censor") ~ trt, trial) %>%
    broom::tidy()

  expect_equal(
    tidy(cuminc1, times = 15) %>%
      dplyr::pull(estimate),
    cmprsk::timepoints(cmprsk_cuminc1, times = 15)$est %>% c()
  )
  expect_equal(
    tidy(cuminc1, times = 15) %>%
      dplyr::pull(std.error),
    cmprsk::timepoints(cmprsk_cuminc1, times = 15)$var %>% sqrt() %>% c()
  )

  expect_equal(
    tidy(cuminc2, times = 15) %>%
      dplyr::arrange(outcome) %>%
      dplyr::pull(estimate),
    cmprsk::timepoints(cmprsk_cuminc2, times = 15)$est %>% c()
  )

  expect_equal(
    tidy(cuminc1, times = 15) %>%
      dplyr::arrange(outcome) %>%
      dplyr::pull(std.error),
    cmprsk::timepoints(cmprsk_cuminc1, times = 15)$var %>% sqrt() %>% c()
  )

  expect_false(
    identical(
      cuminc1 %>% tidy(times = c(12, 24)),
      cuminc1 %>% tidy(times = c(12, 24), conf.level = 0.90)
    )
  )
  expect_true(
    !any(c("conf.low", "conf.high") %in%
      names(cuminc1 %>% tidy(times = c(12, 24), conf.int = FALSE)))
  )

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
    filter(outcome == "death from cancer") %>%
    mutate(strata = paste0("trt=", strata)) %>%
    select(outcome, strata, time, n.risk, n.event) %>%
    dplyr::inner_join(
      tidy_survfit2_cancer %>%
        select(strata, time, n.risk, n.event),
      by = c("strata", "time")
    )
  survfit_censor_check2 <-
    cuminc2_tidy %>%
    filter(outcome == "death from cancer") %>%
    mutate(strata = paste0("trt=", strata)) %>%
    select(outcome, strata, time, n.censor) %>%
    dplyr::inner_join(
      tidy_survfit2_cancer_censor %>%
        select(strata, time, n.censor),
      by = c("strata", "time")
    )
  survfit_censor_check1 <-
    cuminc1_tidy %>%
    filter(outcome == "death from cancer") %>%
    select(outcome, time, n.censor) %>%
    dplyr::inner_join(
      tidy_survfit1_cancer_censor %>%
        select(time, n.censor),
      by = c("time")
    )


  expect_equal(
    survfit_censor_check2$n.censor.x,
    survfit_censor_check2$n.censor.y
  )
  expect_equal(
    survfit_censor_check1$n.censor.x,
    survfit_censor_check1$n.censor.y
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
    filter(outcome == "death from cancer") %>%
    select(time, n.risk, n.event) %>%
    dplyr::inner_join(
      tidy_survfit1_cancer %>%
        select(time, n.risk, n.event),
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

  # Selected time points

  cuminc1_tidy_time <- tidy(cuminc1, times = c(0, 5, 10, 15, 20))
  survfit_check1_time <-
    cuminc1_tidy_time %>%
    filter(outcome == "death from cancer") %>%
    select(time, n.risk, n.event) %>%
    dplyr::inner_join(
      data.frame(
        time = survfit1_cancer_times$time,
        n.risk = survfit1_cancer_times$n.risk,
        n.event = survfit1_cancer_times$n.event
      ),
      by = c("time")
    )

  expect_equal(
    survfit_check1_time$n.risk.x,
    survfit_check1_time$n.risk.y
  )

  expect_equal(
    survfit_check1_time$n.event.x,
    survfit_check1_time$n.event.y
  )


  # all estimates fall within CI
  expect_true(
    cuminc2_tidy %>%
      dplyr::rowwise() %>%
      mutate(
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
      filter(estimate == 0) %>%
      mutate(
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
  expect_equal(
    cuminc_tidy2$cum.censor,
    rep_len(0L, 8)
  )
  expect_equal(
    cuminc_tidy2$cum.event,
    c(0L, 3L, 0L, 6L, 0L, 9L, 0L, 5L)
  )



  expect_error(
    cuminc_tidy1 <- tidy(cuminc1, conf.int = FALSE, times = c(0, 12)),
    NA
  )
  expect_equal(
    cuminc_tidy1$n.censor,
    rep_len(0L, 4)
  )
  expect_equal(
    cuminc_tidy1$cum.censor,
    rep_len(0L, 4)
  )
  expect_equal(
    cuminc_tidy1$cum.event,
    c(0L, 12L, 0L, 11L)
  )

  # testing tidy with problematic times
  expect_message(
    tidy_cuminc1_time <- cuminc1 %>% tidy(times = c(-1, 0, 150))
  )
  expect_equal(
    tidy_cuminc1_time$time,
    c(0, 150, 0, 150)
  )
  expect_equal(
    tidy_cuminc1_time$estimate,
    c(0, NA, 0, NA)
  )
  expect_equal(
    tidy_cuminc1_time$std.error,
    c(0, NA, 0, NA)
  )
  expect_equal(
    tidy_cuminc1_time$conf.low,
    c(NA_real_, NA_real_, NA_real_, NA_real_)
  )
  expect_equal(
    tidy_cuminc1_time$conf.high,
    c(NA_real_, NA_real_, NA_real_, NA_real_)
  )
  expect_equal(
    tidy_cuminc1_time$n.risk,
    c(200L, 0L, 200L, 0L)
  )
  expect_equal(
    tidy_cuminc1_time$n.event,
    c(0L, 0L, 0L, 0L)
  )
  expect_equal(
    tidy_cuminc1_time$n.censor,
    c(0L, 0L, 0L, 0L)
  )

  # testing that n.event over intervals is correct when 0 is and is not specified
  tt <- cuminc(Surv(ttdeath, death_cr) ~ 1, trial)

  expect_equal(
    tidy(tt, times = c(0, 24)) %>%
      dplyr::select(time, outcome, estimate, n.event, n.censor) %>%
      dplyr::filter(time %in% 24),
    tidy(tt, times = c(24)) %>%
      dplyr::select(time, outcome, estimate, n.event, n.censor)
  )

  # checking factor class in internal tidy object
  trial2 <- trial
  levels(trial2$grade) <- c("III", "II", "I")
  expect_equal(
    cuminc(Surv(ttdeath, death_cr) ~ grade, data = trial2) %>%
      tidy(times = c(0, 24)) %>%
      purrr::pluck("strata") %>%
      levels(),
    c("III", "II", "I")
  )
})
