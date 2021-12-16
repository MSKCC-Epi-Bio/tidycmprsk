#' Competing Risks Cumulative Incidence
#'
#' @inheritParams crr
#' @inheritParams cmprsk::cuminc
#' @param x input object
#'
#' @section confidence intervals:
#'
#' The confidence intervals for cumulative incidence estimates use the recommended method in
#' *Competing Risks: A Practical Perspective* by Melania Pintilie.
#'
#' \deqn{x^exp(±z * se / (x * log(x)))}
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
cuminc.formula <- function(formula, data, strata, rho = 0, ...) {

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
  }
  else {
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
  cuminc_bridge(processed, formula, data, strata, rho, failcode_numeric)
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


cuminc_bridge <- function(processed, formula, data, strata, rho, failcode) {

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
      blueprint = processed$blueprint
    )

  output
}

new_cuminc <- function(formula, data, failcode, blueprint, cmprsk) {
  new_cuminc <-
    hardhat::new_model(
      formula = formula,
      data = data,
      failcode = failcode,
      cmprsk = cmprsk,
      blueprint = blueprint,
      class = "tidycuminc"
    )
  new_cuminc <-
    new_cuminc %>%
    purrr::list_modify(tidy = first_cuminc_tidy(new_cuminc))
  new_cuminc
}
