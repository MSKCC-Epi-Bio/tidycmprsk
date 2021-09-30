#' Competing Risks Regression
#'
#' @param x placeholder
#' @param formula placeholder
#' @param data placeholder
#' @param failcode placeholder
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
crr.formula<- function(formula, data, failcode=1, ...) {
  processed <- hardhat::mold(formula, data)
  crr_bridge(processed, formula, failcode)
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
    failcode = failcode,
    tidy = tidy,
    original_fit = original_fit,
    model = data,
    blueprint = blueprint
    #class = "tidycrr"
  )
}

crr_impl <- function(predictors, outcomes, failcode) {

  # function to run crr and summarize with tidy (implementation)

  crr_fit <-
    cmprsk::crr(ftime = outcomes[, 1], fstatus = outcomes[, 2], cov1 = predictors, failcode = failcode)

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

crr_bridge <- function(processed, formula, failcode) {

  # function to connect object and implementation

  # validate_outcomes_are_univariate(processed$outcomes)

  predictors <- as.matrix(processed$predictors)
  outcomes <- as.matrix(processed$outcomes)

  fit <- crr_impl(predictors, outcomes, failcode)

  output <- new_crr(
    coefs = fit$coefs,
    coef_names = fit$coef_names,
    formula = formula,
    tidy = fit$tidy,
    original_fit = fit$original_fit,
    data = cbind(processed$outcomes,processed$predictors),
    failcode = failcode,
    blueprint = processed$blueprint
  )
  class(output) = "tidycmprsk"
  output
}

# Print method
#' @rdname crr
#' @export
print.tidycmprsk <- function(object, ...){
  cat("Call: \n")
  print(object$formula)
  cat(paste("Failure type of interest:",object$failcode,"\n"))
  cat("Fine and Gray's model fit: \n")
  print(object$tidy)
  invisible(object)
}


# model.matrix
#' @rdname crr
#' @export
model.matrix.tidycmprsk <- function(object, ...){
  stats::model.matrix(object$formula,object$model)[,-1]
  # by default there is no intercept term in F&G's model
}

# model.frame
#' @rdname crr
#' @export
model.frame.tidycmprsk <- function(object, ...){
  processed <- hardhat::mold(object$formula, object$data)
  cbind(processed$outcomes,processed$predictors)
}

