library(dplyr)

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
  expect_equal(
    cuminc1$tidy %>%
      dplyr::group_by(outcome) %>%
      filter(time <= 15) %>%
      dplyr::slice_tail() %>%
      dplyr::pull(estimate),
    cmprsk::timepoints(cmprsk_cuminc1, times = 15)$est %>% c()
  )
  expect_equal(
    cuminc1$tidy %>%
      dplyr::group_by(outcome) %>%
      filter(time <= 15) %>%
      dplyr::slice_tail() %>%
      dplyr::pull(std.error),
    cmprsk::timepoints(cmprsk_cuminc1, times = 15)$var %>% sqrt() %>% c()
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

  expect_error(
    cuminc(Surv(ttdeath, death_cr) ~ grade + trt, data = trial),
    NA
  )

  # checking factor class in internal tidy object
  expect_true(
    cuminc(Surv(ttdeath, death_cr) ~ grade, data = trial) %>%
      purrr::pluck("tidy", "strata") %>%
      inherits("factor")
  )

  # no warnings with tied times
  expect_warning(
    cuminc(Surv(drat, factor(cyl)) ~ 1, mtcars),
    NA
  )

  # check when one stratum has no observed values of a particular outcome
  expect_equal(
    cuminc(
      Surv(ttdeath, death_cr) ~ grade,
      # remove all other cause death obs for grade "I"
      data = trial %>% dplyr::filter(!(grade == "I" & death_cr == "death other causes"))
    ) %>%
      purrr::pluck("tidy") %>%
      dplyr::filter(time == 0),
    tibble::tribble(
      ~time,         ~outcome, ~strata, ~estimate, ~std.error,       ~conf.low,       ~conf.high, ~n.risk, ~n.event, ~n.censor, ~cum.event, ~cum.censor,
      0,  "death from cancer",     "I",         0,          0,        NA_real_,         NA_real_,     53L,       0L,        0L,         0L,          0L,
      0, "death other causes",     "I",         0,          0,        NA_real_,         NA_real_,     53L,       0L,        0L,         0L,          0L,
      0,  "death from cancer",    "II",         0,          0,        NA_real_,         NA_real_,     68L,       0L,        0L,         0L,          0L,
      0, "death other causes",    "II",         0,          0,        NA_real_,         NA_real_,     68L,       0L,        0L,         0L,          0L,
      0,  "death from cancer",   "III",         0,          0,        NA_real_,         NA_real_,     64L,       0L,        0L,         0L,          0L,
      0, "death other causes",   "III",         0,          0,        NA_real_,         NA_real_,     64L,       0L,        0L,         0L,          0L
    ) %>%
      dplyr::mutate(strata = factor(strata, levels = c("I", "II", "III")))
  )


  # `cuminc()` works with no observed censoring
  expect_error(
    cuminc_no_censor<-
      cuminc(
        Surv(ttdeath, death_cr) ~ 1,
        data = trial %>% dplyr::filter(!death_cr %in% "censor")
      ),
    NA
  )
  expect_equal(
    cmprsk::cuminc(
      ftime = trial$ttdeath[!trial$death_cr %in% "censor"],
      fstatus = as.numeric(trial$death_cr[!trial$death_cr %in% "censor"]) - 1L
    ),
    cuminc_no_censor$cmprsk
  )
})

