#' Competing Risks Regression
#'
#' @param x placeholder
#' @param formula placeholder
#' @param data placeholder
#' @param failcode placeholder
#' @param new_data placeholder
#' @param object placeholder
#' @param quantiles placeholder
#' @param ... placeholder
#'
#' @return tidycrr object
#' @name crr
#' @examples
#' # ADD EXAMPLE!
NULL
#' @import broom hardhat
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
print.tidycmprsk <- function(x, ...){
  cat("Call: \n")
  print(x$formula)
  cat(paste("Failure type of interest:",x$failcode,"\n"))
  cat("Fine and Gray's model fit: \n")
  print(x$tidy)
  invisible(x)
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
model.frame.tidycmprsk <- function(formula, ...){
  processed <- hardhat::mold(formula$formula, formula$model)
  cbind(processed$outcomes,processed$predictors)
}


# model_frame.tidycmprsk <- function(object, ...){
#   processed <- hardhat::mold(object$formula, object$model)
#   frame_tib <- tibble::as_tibble(cbind(processed$outcomes,processed$predictors))
#   frame_tib
# }


# tidy
#' @rdname crr
#' @export
#' @family tidycmprsk tidiers
tidy.tidycmprsk <- function(object, ...){
  tibble::as_tibble(object$tidy)
}

# glance
#' @rdname crr
#' @export
#' @family tidycmprsk tidiers
glance.tidycmprsk <- function(object, ...){
  s <- summary(object$original_fit)
  as_glance_tibble(
    n = s$n,
    n.missing = s$n.missing,
    statistic.pseudoLRT = s$logtest[1],
    df.pseudoLRT = s$logtest[2],
    logpseudoLik = as.numeric(s$loglik),
    na_types = "iirrr"
  )
}

############################ Prediction

# predict
#' @rdname crr
#' @export
predict.tidycmprsk <- function(object, new_data = NULL, quantiles = seq(0,1,0.25), ...) {

  if(is.null(new_data)){
    new_data <- object$model
  }

  # Enforces column order, type, column names, etc
  processed <- hardhat::forge(new_data, object$blueprint)

  out <- cmprsk::predict.crr(object$original_fit, as.matrix(processed$predictors))
  colnames(out) <- c("time",rownames(processed$predictors))

  # CIF at time quantiles
  quarter.time <- quantile(out[,"time"],probs=quantiles,type=1)
  quarter.labels <- paste(names(quarter.time), round(quarter.time,2))
  qout <- t(out[out[,"time"] %in% quarter.time,-1])
  colnames(qout) <- quarter.labels
  qout <- tibble::as_tibble(qout)
  attr(qout,"CIF_at_quantile")
  validate_prediction_size(qout, new_data)

  # linear predictor
  coefs <- object$coefs
  pred <- as.vector(as.matrix(processed$predictors) %*% coefs)
  lpout <- hardhat::spruce_numeric(pred)
  names(lpout) = "lp"
  attr(lpout,"lp")
  validate_prediction_size(lpout, new_data)

  list(
    newdata = processed$predictors,
    qout = qout,
    lpout = lpout
  )

}

# augment
#' @rdname crr
#' @export
#' @family tidycmprsk tidiers
augment.tidycmprsk <- function(object, quantiles = seq(0,1,0.25), ...){

  pred <- predict.tidycmprsk(object, new_data = object$model, quantiles = quantiles)
  out <- cbind(pred$newdata,
               pred$qout,
               pred$lpout)
  tibble::as_tibble(out)
}
