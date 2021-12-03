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

  expect_error(
    glance(cuminc2),
    NA
  )
  expect_error(
    cuminc_tidy <- tidy(cuminc2),
    NA
  )
  expect_equal(
    cuminc_tidy,
    cuminc2$tidy
  )

  # all estimates fall within CI
  expect_true(
    cuminc_tidy %>%
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
    cuminc_tidy %>%
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
  expect_equal(
    cuminc_tidy1$n.event,
    c(0L, 12L, 0L, 11L)
  )
  expect_equal(
    cuminc_tidy1$n.risk,
    c(200L, 177L, 200L, 177L)
  )



})


