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
model.matrix.tidycrr <- function(object, ...) {
  # by default there is no intercept term in F&G's model
  stats::model.matrix(object$formula, object$model)[, -1, drop = FALSE]
}

#' @export
#' @rdname base_methods
model.frame.tidycrr <- function(formula, ...) {
  stats::model.frame(formula = formula$formula, data = formula$data)
}

#' @export
#' @rdname base_methods
model.frame.tidycuminc <- function(formula, ...) {
  stats::model.frame(formula = formula$formula, data = formula$data)
}

#' Predict
#'
#' @export
#' @rdname base_methods
predict.tidycrr <- function(object, new_data = NULL, quantiles = seq(0, 1, 0.25), ...) {
  if (is.null(new_data)) {
    new_data <- object$model
  }

  # Enforces column order, type, column names, etc
  processed <- hardhat::forge(new_data, object$blueprint)

  out <- cmprsk::predict.crr(object$original_fit, as.matrix(processed$predictors))
  colnames(out) <- c("time", rownames(processed$predictors))

  # Observed event time
  tout <- out[, 1]
  tout <- tibble::as_tibble(tout)
  names(tout) <- "event time"
  attr(tout, "time")
  # validate_prediction_size(qout, new_data)

  # CIF at all event time
  cifout <- t(out[, -1])
  colnames(cifout) <- out[, 1]
  cifout <- tibble::as_tibble(cifout)
  attr(cifout, "CIF")
  # validate_prediction_size(qout, new_data)

  # CIF at time quantiles
  quarter.time <- stats::quantile(out[, "time"], probs = quantiles, type = 1)
  quarter.labels <- paste(names(quarter.time), round(quarter.time, 2))
  qout <- t(out[out[, "time"] %in% quarter.time, -1])
  colnames(qout) <- quarter.labels
  qout <- tibble::as_tibble(qout)
  attr(qout, "CIF_at_quantile")
  # validate_prediction_size(qout, new_data)

  # linear predictor
  coefs <- object$coefs
  pred <- as.vector(as.matrix(processed$predictors) %*% coefs)
  lpout <- hardhat::spruce_numeric(pred)
  names(lpout) <- "lp"
  attr(lpout, "lp")
  # validate_prediction_size(lpout, new_data)

  list(
    newdata = processed$predictors,
    tout = tout,
    cifout = cifout,
    qout = qout,
    lpout = lpout
  )
}

#' @export
#' @rdname base_methods
terms.tidycrr <- function(x, ...) {
  stats::terms(x = x$formula, data = x$data)
}

#' @export
#' @rdname base_methods
coef.tidycrr <- function(object, ...) {
  object$coefs
}



predict_practice <- function(object, times = NULL, probs = NULL, newdata = NULL, ...) {
  # checking inputs ------------------------------------------------------------
  if (is.null(times) + is.null(probs) != 1L) {
    stop("Must specify one and only one of `times=` and `probs=`.", call. = FALSE)
  }
  if (!is.null(times) && times < 0) {
    stop("`times=` must be non-negative.", call. = FALSE)
  }
  if (!is.null(probs) && !all(between(probs, 0, 1))) {
    stop("`probs=` must be between 0 and 1.", call. = FALSE)
  }

  # getting predictions on the original model fit ------------------------------
  processed <- crr_mold(object$formula, newdata %||% object$data)
  matrix_pred <-
    predict(mod$original_fit, cov1 = as.matrix(processed$predictors))

  if (!is.null(times)) return(probs_at_times(matrix_pred, times))
  if (!is.null(probs)) return(times_at_probs(matrix_pred, probs))
}

probs_at_times <- function(matrix_pred, times) {
  # defining times for predictions ---------------------------------------------
  all_times <- union(0, matrix_pred[, 1]) %>% sort()
  if (max(times) >= max(all_times)) {
    stringr::str_glue("`times=` cannot be larger than {max(all_times)}") %>%
      stop(call. = FALSE)
  }
  times_obs <-
    purrr::map_dbl(times, ~all_times[which.min(all_times <= .x) - 1L])

  # named list of the risks, the names are the times,
  # the values are the estimates of risk at the covar levels
  lst_risk_time <-
    purrr::map(seq_len(length(all_times) - 1L), ~matrix_pred[.x, -1]) %>%
    stats::setNames(all_times[-1]) %>%
    dplyr::bind_cols() %>%
    dplyr::mutate(`0` = 0, .before = 1) %>%
    as.list()

  # extracting vector of risks at specified time -------------------------------
  if (length(times_obs) == 1L)
    return(lst_risk_time[[as.character(times_obs)]])

  lst_risk_time[as.character(times_obs)] %>%
    stats::setNames(paste("time", times))
}
