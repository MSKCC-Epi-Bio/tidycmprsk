test_that("my_mean() works", {
  expect_error(
    res <- my_mean(1:2),
    NA
  )

  expect_equal(
    res,
    1.5
  )

  expect_error(
    my_mean("daniel")
  )
})
