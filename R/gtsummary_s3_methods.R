#' gtsummary methods
#'
#' @description
#' These functions are S3 methods for working with [`tidycmprsk::crr()`] model
#' results.
#'
#' - `tbl_regression.tidycrr()`: This function sets the tidycmprsk tidier for `crr()` models.
#' - `global_pvalue_fun.tidycrr()`: This function ensures that `gtsummary::add_global_p(anova_fun)` uses
#'     the Wald test by default (instead of `car::Anova()`, which does not support this model type).
#'     The Wald test is executed with `cardx::ard_aod_wald_test()`, which wraps `aod::wald.test()`.
#'
#' @param x (`tidycrr`)\cr
#'   `tidycmprsk::crr()` regression object
#' @param tidy_fun (`function`)\cr
#'    Tidier function for the model. Default is `tidycmprsk::tidy()`.
#' @param type not used
#' @inheritParams gtsummary::tbl_regression
#'
#' @name gtsummary_s3_methods
#' @return gtsummary table or data frame of results
#'
#' @examples
#' crr(Surv(ttdeath, death_cr) ~ age + grade, trial) |>
#'   gtsummary::tbl_regression() |>
#'   gtsummary::add_global_p() |>
#'   gtsummary::as_gt()
NULL

#' @rdname gtsummary_s3_methods
#' @export
tbl_regression.tidycrr <- function(x, tidy_fun = tidycmprsk::tidy, ...) {
  asNamespace("gtsummary")[["tbl_regression.default"]](x = x, tidy_fun = tidy_fun, ...)
}

#' @rdname gtsummary_s3_methods
#' @export
global_pvalue_fun.tidycrr <- function(x, type, ...) {
  check_pkg_installed("cardx")

  cardx::ard_aod_wald_test(x, ...)
}
