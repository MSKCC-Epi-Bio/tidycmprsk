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

test_that("predict() preserves fitted factor columns in newdata", {
  fit <- crr(
    Surv(ttdeath, death_cr) ~ age + trt,
    data = trial
  )

  newdata <-
    trial |>
    dplyr::mutate(
      trt = factor("Drug A", levels = levels(trt))
    )

  padded_newdata <-
    dplyr::bind_rows(
      newdata,
      trial |>
        dplyr::filter(trt == "Drug B") %>%
        dplyr::slice(1)
    )

  expected <-
    predict(
      fit,
      times = 20,
      newdata = padded_newdata
    )[["time 20"]][seq_len(nrow(newdata))]

  observed <-
    predict(
      fit,
      times = 20,
      newdata = newdata
    )[["time 20"]]

  expect_equal(observed, expected)
})

test_that("predict() uses training levels for character predictors", {
  data_chr <-
    trial |>
    dplyr::mutate(
      trt = as.character(trt)
    )

  fit <- crr(
    Surv(ttdeath, death_cr) ~ age + trt,
    data = data_chr
  )

  newdata <-
    data_chr |>
    dplyr::mutate(
      trt = "Drug A"
    )

  padded_newdata <-
    dplyr::bind_rows(
      newdata,
      data_chr |>
        dplyr::filter(trt == "Drug B") %>%
        dplyr::slice(1)
    )

  expected <-
    predict(
      fit,
      times = 20,
      newdata = padded_newdata
    )[["time 20"]][seq_len(nrow(newdata))]

  observed <-
    predict(
      fit,
      times = 20,
      newdata = newdata
    )[["time 20"]]

  expect_equal(observed, expected)
})

test_that("predict() does not restore factor levels dropped during fitting", {
  data_unused <-
    trial |>
    dplyr::mutate(
      trt = factor(
        trt,
        levels = paste("Drug", c("A", "B", "C"))
      )
    )

  data_dropped <-
    data_unused |>
    dplyr::mutate(
      trt = droplevels(trt)
    )

  fit_unused <- crr(
    Surv(ttdeath, death_cr) ~ age + trt,
    data = data_unused
  )

  fit_dropped <- crr(
    Surv(ttdeath, death_cr) ~ age + trt,
    data = data_dropped
  )

  newdata_unused <-
    data_unused |>
    dplyr::mutate(
      trt = factor("Drug B", levels = levels(trt))
    )

  newdata_dropped <-
    data_dropped |>
    dplyr::mutate(
      trt = factor("Drug B", levels = levels(trt))
    )

  expect_equal(
    predict(
      fit_unused,
      times = 20,
      newdata = newdata_unused
    ),
    predict(
      fit_dropped,
      times = 20,
      newdata = newdata_dropped
    )
  )
})

test_that("predict() only requires predictors in newdata", {
  fit <- crr(
    Surv(ttdeath, death_cr) ~ age + trt,
    data = trial
  )

  newdata <-
    trial |>
    dplyr::select(age, trt)

  expect_error(
    predict(
      fit,
      times = 20,
      newdata = newdata
    ),
    NA
  )
})
