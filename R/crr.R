#' Competing Risks Regression
#'
#' @param x input object
#' @param formula formula with `Surv()` on LHS and covariates on RHS.
#' @param data data frame
#' @param failcode indicates event of interest. Default is `NULL` and event
#' of interest is inferred from the data.
#' @param ... passed to methods
#'
#' @return tidycrr object
#' @name crr
#' @examples
#' crr(Surv(ttdeath, death_cr) ~ age, trial)
NULL

# Formula method
#' @rdname crr
#' @export
crr.formula <- function(formula, data, failcode = NULL, ...) {

  # checking inputs and assigning the numeric failcode -------------------------
  failcode_numeric <-
    as_numeric_failcode(formula = formula, data = data, failcode = failcode)

  # process model variables ----------------------------------------------------
  processed <-
    hardhat::mold(
      formula, data,
      blueprint = hardhat::default_formula_blueprint(intercept = TRUE)
    )
  #remove intercept
  processed$predictors <- processed$predictors[,-1]

  # building model -------------------------------------------------------------
  crr_bridge(processed, formula, data, failcode_numeric)
}

as_numeric_failcode <- function(formula, data, failcode) {
  # evaluating LHS of formula --------------------------------------------------
  formula_lhs <-
    tryCatch(
      {
        rlang::f_lhs(formula) %>%
          rlang::eval_tidy(data = data)
      },
      error = function(e) {
        cli::cli_alert_danger("There was an error evaluating the LHS of the formula.")
        stop(e, call. = FALSE)
      }
    )

  # checking type of LHS -------------------------------------------------------
  if (!inherits(formula_lhs, "Surv") ||
      !identical(attr(formula_lhs, "type"), "mright")) {
    paste(
      "The LHS of the formula must be of class 'Surv' and type 'mright'.",
      "Please review syntax in the help file."
    ) %>%
      stop(call. = FALSE)
  }

  # checking the failcode argument ---------------------------------------------
  failcode <- failcode %||% attr(formula_lhs, "states")[1]
  if (!is.null(failcode) && !failcode %in% attr(formula_lhs, "states")) {
    stop("Invalid `failcode=` specification.")
  }
  failcode_numeric <- which(attr(formula_lhs, "states") %in% failcode)

  return(failcode_numeric %>% rlang::set_names(failcode))
}

new_crr <- function(coefs, coef_names, formula, tidy, original_fit, data, failcode, blueprint) {

  # function to create an object

  if (!is.numeric(coefs)) {
    stop("`coefs` should be a numeric vector.", call. = FALSE)
  }

  if (!is.character(coef_names)) {
    stop("`coef_names` should be a character vector.", call. = FALSE)
  }

  if (length(coefs) != length(coef_names)) {
    stop("`coefs` and `coef_names` must have the same length.")
  }

  hardhat::new_model(
    coefs = coefs,
    coef_names = coef_names,
    formula = formula,
    data = data,
    failcode = failcode,
    tidy = tidy,
    original_fit = original_fit,
    model = data,
    blueprint = blueprint,
    class = "tidycrr"
  )
}

crr_impl <- function(predictors, outcomes, failcode) {

  # function to run crr and summarize with tidy (implementation)
  crr_fit <-
    cmprsk::crr(ftime = outcomes[, 1], fstatus = outcomes[, 2], cov1 = predictors, failcode = failcode)

  tidy <- broom::tidy(crr_fit)

  coefs <- tidy$estimate
  coef_names <- tidy$term

  list(
    coefs = coefs,
    coef_names = coef_names,
    tidy = tidy,
    original_fit = crr_fit
  )
}

crr_bridge <- function(processed, formula, data, failcode) {

  # function to connect object and implementation

  # validate_outcomes_are_univariate(processed$outcomes)

  predictors <- as.matrix(processed$predictors)
  outcomes <- as.matrix(processed$outcomes[, 1, drop = TRUE])

  fit <- crr_impl(predictors, outcomes, failcode)

  output <-
    new_crr(
      coefs = fit$coefs,
      coef_names = fit$coef_names,
      formula = formula,
      data = data,
      tidy = fit$tidy,
      original_fit = fit$original_fit,
      failcode = failcode,
      blueprint = processed$blueprint
    )

  output
}


# Generic
#' @rdname crr
#' @export
crr <- function(x, ...) {
  UseMethod("crr")
}

# Default
#' @rdname crr
#' @export
crr.default <- function(x, ...) {
  stop("`crr()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}
