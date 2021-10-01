test_that("crr() works", {
  expect_error(
    crr(crSurv(ttdeath, death_cr_num) ~ age, trial),
    tidy(crr(crSurv(ttdeath, death_cr_num) ~ age, trial)),
    predict(crr(crSurv(ttdeath, death_cr_num) ~ age, trial)),
    glance(crr(crSurv(ttdeath, death_cr_num) ~ age, trial)),
    augment(crr(crSurv(ttdeath, death_cr_num) ~ age, trial)),
    model.frame(crr(crSurv(ttdeath, death_cr_num) ~ age, trial)),
    model.matrix(crr(crSurv(ttdeath, death_cr_num) ~ age, trial)),
    NA
  )
})
