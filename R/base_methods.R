#' Methods for tidycmprsk objects
#'
#' @param x,object,formula a tidycmprsk object
#' @param quantiles vector of quantiles
#' @param new_data a data frame
#' @param ... not used
#' @name base_methods
NULL

#' @export
#' @rdname base_methods
model.matrix.tidycrr <- function(object, ...){
  # by default there is no intercept term in F&G's model
  stats::model.matrix(object$formula,object$model)[, -1, drop = FALSE]
}

#' @export
#' @rdname base_methods
model.frame.tidycrr <- function(formula, ...){
  stats::model.frame(formula = formula$formula, data = formula$data)
}

#' Predict
#'
#' @export
#' @rdname base_methods
predict.tidycrr <- function(object, new_data = NULL, quantiles = seq(0,1,0.25), ...) {

  if(is.null(new_data)){
    new_data <- object$model
  }

  # Enforces column order, type, column names, etc
  processed <- hardhat::forge(new_data, object$blueprint)

  out <- cmprsk::predict.crr(object$original_fit, as.matrix(processed$predictors))
  colnames(out) <- c("time",rownames(processed$predictors))

  # CIF at time quantiles
  quarter.time <- stats::quantile(out[,"time"],probs=quantiles,type=1)
  quarter.labels <- paste(names(quarter.time), round(quarter.time,2))
  qout <- t(out[out[,"time"] %in% quarter.time,-1])
  colnames(qout) <- quarter.labels
  qout <- tibble::as_tibble(qout)
  attr(qout,"CIF_at_quantile")
  # validate_prediction_size(qout, new_data)

  # linear predictor
  coefs <- object$coefs
  pred <- as.vector(as.matrix(processed$predictors) %*% coefs)
  lpout <- hardhat::spruce_numeric(pred)
  names(lpout) = "lp"
  attr(lpout,"lp")
  # validate_prediction_size(lpout, new_data)

  list(
    newdata = processed$predictors,
    qout = qout,
    lpout = lpout
  )
}

#' @export
#' @rdname base_methods
terms.tidycrr <- function(x, ...) {
  x$blueprint$terms
}

