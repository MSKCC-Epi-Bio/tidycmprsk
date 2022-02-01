#' Competing Risks Cumulative Incidence
#'
#' @inheritParams crr
#' @inheritParams cmprsk::cuminc
#' @param x input object
#'
#' @section Confidence intervals:
#'
#' The confidence intervals for cumulative incidence estimates use the recommended method in
#' *Competing Risks: A Practical Perspective* by Melania Pintilie.
#'
#' \deqn{x^{exp(±z * se / (x * log(x)))}}
#'
#' where \eqn{x} is the cumulative incidence estimate, \eqn{se} is the
#' standard error estimate, and \eqn{z} is the z-score associated with the
#' confidence level of the interval, e.g. \eqn{z = 1.96} for a 95% CI.
#'
#' @section p-values:
#'
#' The p-values reported in `cuminc()`, `glance.tidycuminc()` and `add_p.tbl_cuminc()`
#' are Gray's test as described in
#' Gray RJ (1988) *A class of K-sample tests for comparing the cumulative incidence of a competing risk*, Annals of Statistics, 16:1141-1154.
#'
#' @return tidycuminc object
#' @family cuminc() functions
#' @name cuminc
#' @examples
#' # calculate risk for entire cohort -----------
#' cuminc(Surv(ttdeath, death_cr) ~ 1, trial)
#'
#' # calculate risk by treatment group ----------
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial)
NULL

# Formula method
#' @rdname cuminc
#' @export
cuminc.formula <- function(formula, data, strata, rho = 0, conf.level = 0.95, ...) {

  # extracting failure level ---------------------------------------------------
  failcode_numeric <-
    as_numeric_failcode(formula = formula, data = data, keep_all = TRUE)

  # hardhat::mold() doesn't allow for a constant on the RHS,
  # performing check, and re-formulating formula with NULL on RHS if needed
  if (tryCatch(abs(rlang::f_rhs(formula) - 1) < 10e-9, error = function(e) FALSE)) {
    formula <- stats::reformulate("NULL", formula[[2]])
    processed <-
      hardhat::mold(
        formula, data,
        blueprint = hardhat::default_formula_blueprint(intercept = TRUE)
      )
  } else {
    # instead of using dummy variables, try to keep covariates "as is".
    processed <-
      hardhat::mold(
        formula, data,
        blueprint = hardhat::default_formula_blueprint(indicators = "none")
      )

    # use covariate interactions as input to cmprsk::cuminc
    processed$predictors <- interaction(processed$predictors)
  }

  # building model -------------------------------------------------------------
  cuminc_bridge(processed, formula, data, strata, rho, failcode_numeric, conf.level)
}

# Generic
#' @rdname cuminc
#' @export
cuminc <- function(x, ...) {
  UseMethod("cuminc")
}

# Default
#' @rdname cuminc
#' @export
cuminc.default <- function(x, ...) {
  stop("`cuminc()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

cuminc_impl <- function(predictors, outcomes, strata, rho) {

  # function to run cuminc and summarize with tidy (implementation)

  cuminc_fit <-
    cmprsk::cuminc(
      ftime = outcomes[, 1],
      fstatus = outcomes[, 2],
      group = predictors,
      cencode = 0,
      strata = strata,
      rho = rho
    )

  list(
    cmprsk = cuminc_fit
  )
}


cuminc_bridge <- function(processed, formula, data, strata, rho, failcode, conf.level) {

  # function to connect object and implementation

  # validate_outcomes_are_univariate(processed$outcomes)

  predictors <- as.matrix(processed$predictors)
  outcomes <- as.matrix(processed$outcomes[, 1, drop = TRUE])

  fit <- cuminc_impl(predictors, outcomes, strata, rho)

  output <-
    new_cuminc(
      formula = formula,
      data = data,
      failcode = failcode,
      cmprsk = fit$cmprsk,
      blueprint = processed$blueprint,
      conf.level = conf.level
    )

  output
}

new_cuminc <- function(formula, data, failcode, blueprint, cmprsk, conf.level) {
  new_cuminc <-
    hardhat::new_model(
      formula = formula,
      data = data,
      failcode = failcode,
      cmprsk = cmprsk,
      blueprint = blueprint,
      conf.level = conf.level,
      class = "tidycuminc"
    )
  new_cuminc <-
    new_cuminc %>%
    purrr::list_modify(tidy = first_cuminc_tidy(new_cuminc, conf.level = conf.level))
  new_cuminc
}
