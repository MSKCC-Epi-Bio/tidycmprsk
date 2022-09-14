#' Competing Risks Regression
#'
#' @param x input object
#' @param formula formula with `Surv()` on LHS and covariates on RHS.
#' The event status variable must be a factor, with the first level indicating
#' 'censor' and subsequent levels the competing risks. The `Surv(time2=)`
#' argument cannot be used.
#' @param data data frame
#' @param failcode Indicates event of interest. If `failcode=` is `NULL`,
#' the first competing event will be used as the event of interest.
#' Default is `NULL`.
#' @param conf.level confidence level. Default is 0.95.
#' @param ... passed to methods
#'
#' @return tidycrr object
#' @family crr() functions
#' @name crr
#' @examples
#' crr(Surv(ttdeath, death_cr) ~ age + grade, trial)
NULL

# Formula method
#' @rdname crr
#' @export
crr.formula <- function(formula, data, failcode = NULL, conf.level = 0.95, ...) {
  # checking inputs and assigning the numeric failcode -------------------------
  failcode_numeric <-
    as_numeric_failcode(formula = formula, data = data, failcode = failcode)

  # process model variables ----------------------------------------------------
  processed <- crr_mold(formula, data)

  # building model -------------------------------------------------------------
  crr_bridge(processed, formula, data, failcode_numeric, conf.level = conf.level)
}

crr_mold <- function(formula, data) {
  processed <-
    hardhat::mold(
      formula, data,
      blueprint = hardhat::default_formula_blueprint(intercept = TRUE)
    )
  # remove intercept
  processed$predictors <- processed$predictors[, -1]

  # removing unobserved levels (converted to all zero for dummy variable)
  processed$predictors <-
    processed$predictors %>%
    purrr::map_dfc(
      function(.x) {
        if (all(.x == 0)) return(NULL)
        .x
      }
    )

  processed
}

as_numeric_failcode <- function(formula, data, failcode, keep_all = FALSE) {
  # evaluating LHS of formula --------------------------------------------------
  formula_lhs <-
    tryCatch(
      {
        rlang::f_lhs(formula) %>%
          rlang::eval_tidy(data = data)
      },
      error = function(e) {
        cli::cli_alert_danger("There was an error evaluating the LHS of the formula.")
        stop(as.character(e), call. = FALSE)
      }
    )

  # checking type of LHS -------------------------------------------------------
  if (!inherits(formula_lhs, "Surv") ||
    !identical(attr(formula_lhs, "type"), "mright")) {
    paste(
      "The LHS of the formula must be of class 'Surv' and type 'mright'.",
      "Please review expected syntax in the help file.",
      "The status variable must be a factor, where the first level indicates",
      "the observation was censored, and subsequent levels are the",
      "competing events. Cannot use `Surv(time2=)` argument."
    ) %>%
      stop(call. = FALSE)
  }

  # checking the failcode argument ---------------------------------------------
  all_failues <-
    seq_len(length(attr(formula_lhs, "states"))) %>%
    stats::setNames(attr(formula_lhs, "states"))
  if (isTRUE(keep_all)) {
    return(all_failues)
  }

  failcode <- failcode %||% names(all_failues)[1]
  if (!is.null(failcode) && !failcode %in% names(all_failues)) {
    stop("Invalid `failcode=` specification.")
  }
  failcode_numeric <- which(attr(formula_lhs, "states") %in% failcode)

  return(failcode_numeric %>% rlang::set_names(failcode))
}

new_crr <- function(coefs, coef_names, formula, tidy, cmprsk, data,
                    failcode, blueprint, conf.level) {

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
    coefs = coefs %>% stats::setNames(coef_names),
    formula = formula,
    data = data,
    failcode = failcode,
    conf.level = conf.level,
    xlevels =
      stats::model.frame(formula, data = data)[, -1, drop = FALSE] %>%
        purrr::map(
          function(.x) {
            if (inherits(.x, "factor")) {
              return(levels(.x))
            }
            if (inherits(.x, "character")) {
              return(unique(.x) %>% sort())
            }
            return(NULL)
          }
        ) %>%
        purrr::compact(),
    tidy = tidy,
    cmprsk = cmprsk,
    blueprint = blueprint,
    class = "tidycrr"
  )
}

crr_impl <- function(predictors, outcomes, failcode, conf.level) {

  # function to run crr and summarize with tidy (implementation)
  crr_fit <-
    cmprsk::crr(
      ftime = outcomes[, 1],
      fstatus = outcomes[, 2],
      cov1 = predictors,
      failcode = failcode
    )

  tidy <- broom::tidy(crr_fit, conf.int = TRUE, conf.level = conf.level)

  coefs <- tidy$estimate
  coef_names <- tidy$term

  list(
    coefs = coefs,
    coef_names = coef_names,
    tidy = tidy,
    cmprsk = crr_fit
  )
}

crr_bridge <- function(processed, formula, data, failcode, conf.level) {

  # function to connect object and implementation

  # validate_outcomes_are_univariate(processed$outcomes)

  predictors <- as.matrix(processed$predictors)
  outcomes <- as.matrix(processed$outcomes[, 1, drop = TRUE])

  fit <- crr_impl(predictors, outcomes, failcode, conf.level = conf.level)

  output <-
    new_crr(
      coefs = fit$coefs,
      coef_names = fit$coef_names,
      formula = formula,
      data = data,
      tidy = fit$tidy,
      cmprsk = fit$cmprsk,
      failcode = failcode,
      blueprint = processed$blueprint,
      conf.level = conf.level
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
