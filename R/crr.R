#' Competing Risks Regression
#'
#' @param x placeholder
#' @param ... placeholder
#'
#' @return
#' @name crr
#' @examples
#' # ADD EXAMPLE!
NULL

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

# Formula method
#' @rdname crr
#' @export
crr.formula<- function(formula, data, ...) {
  processed <- hardhat::mold(formula, data)
  crr_bridge(processed)
}


new_crr <- function(coefs, coef_names, tidy, original_fit, blueprint) {

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
    tidy = tidy,
    original_fit = original_fit,
    blueprint = blueprint,
    class = "tidycrr"
  )
}

crr_impl <- function(predictors, outcomes) {
  crr_fit <-
    cmprsk::crr(ftime = outcomes[, 1], fstatus = outcomes[, 2], cov1 = predictors)

  tidy <-
    summary(crr_fit)$coef %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::select(.data$term, estimate = .data$coef, std.error = .data$`se(coef)`,
                  statistic = .data$z, p.value = .data$`p-value`)

  coefs <- tidy$estimate
  coef_names <- tidy$term

  list(
    coefs = coefs,
    coef_names = coef_names,
    tidy = tidy,
    original_fit = crr_fit
  )
}

crr_bridge <- function(processed) {

  # validate_outcomes_are_univariate(processed$outcomes)

  predictors <- as.matrix(processed$predictors)
  outcomes <- as.matrix(processed$outcomes)

  fit <- crr_impl(predictors, outcomes)

  new_crr(
    coefs = fit$coefs,
    coef_names = fit$coef_names,
    tidy = fit$tidy,
    original_fit = fit$original_fit,
    blueprint = processed$blueprint
  )
}




