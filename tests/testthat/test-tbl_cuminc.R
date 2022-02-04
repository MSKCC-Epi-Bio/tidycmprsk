test_that("tbl_cuminc() works", {
  expect_error(
    tbl0 <-
      cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>%
      tbl_cuminc(times = c(12, 24), label_header = "**Month {time}**") %>%
      add_nevent(location = c("label", "level")) %>%
      add_n(location = c("label", "level")),
    NA
  )
  expect_equal(inline_text(tbl0, time = 12), "6.0% (3.3%, 9.9%)")

  expect_error(
    tbl1 <-
      cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
      tbl_cuminc(times = c(12, 24), label_header = "**Month {time}**") %>%
      add_p() %>%
      add_nevent(location = c("label", "level")) %>%
      add_n(location = c("label", "level")),
    NA
  )
  expect_equal(
    inline_text(tbl1, time = 12, level = "Drug A"),
    "3.1% (0.82%, 8.0%)"
  )
  expect_equal(
    inline_text(tbl1, column = "p.value"),
    "0.2"
  )

})
