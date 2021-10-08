#' Competing Risks Cumulative Incidence
#'
#' @param x input object
#' @param formula formula with `Surv()` on LHS and covariates on RHS.
#' @param data data frame
#' @param ... passed to methods
#'
#' @return tidycuminc object
#' @name cuminc
#' @examples
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial)
NULL

# Formula method
#' @rdname cuminc
#' @export
cuminc.formula<- function(formula, data, strata, rho = 0, ...) {

  # checking inputs and assigning the numeric failcode -------------------------
  # failcode_numeric <-
  #   as_numeric_failcode(formula = formula, data = data, failcode = failcode)

  # building model -------------------------------------------------------------
  processed <- hardhat::mold(formula, data)
  cuminc_bridge(processed, formula, data, strata, rho)
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
    cmprsk::cuminc(ftime = outcomes[, 1],
                   fstatus = outcomes[, 2],
                   group = predictors,
                   cencode = 0,
                   strata = strata,
                   rho = rho)

  list(
    original_fit = cuminc_fit
  )
}


cuminc_bridge <- function(processed, formula, data, strata, rho) {

  # function to connect object and implementation

  # validate_outcomes_are_univariate(processed$outcomes)

  predictors <- as.matrix(processed$predictors)
  outcomes <- as.matrix(processed$outcomes[, 1, drop = TRUE])

  fit <- cuminc_impl(predictors, outcomes, strata, rho)

  output <-
    new_cuminc(
      formula = formula,
      data = data,
      original_fit = fit$original_fit,
      blueprint = processed$blueprint
    )

  output
}

new_cuminc <- function(formula, original_fit, data, blueprint) {
  hardhat::new_model(
    formula = formula,
    data = data,
    original_fit = original_fit,
    model = data,
    blueprint = blueprint,
    class = "tidycuminc"
  )
}
