#' Methods for tidycmprsk objects
#'
#' @param x,object,formula a tidycmprsk object
#' @param times times
#' @param probs probs
#' @param newdata a data frame
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
predict.tidycrr <- function(object, times = NULL, probs = NULL, newdata = NULL, ...) {
  # checking inputs ------------------------------------------------------------
  if (is.null(times) + is.null(probs) != 1L) {
    stop("Must specify one and only one of `times=` and `probs=`.", call. = FALSE)
  }
  if (!is.null(times) && times < 0) {
    stop("`times=` must be non-negative.", call. = FALSE)
  }
  if (!is.null(probs) && !all(dplyr::between(probs, 0, 1))) {
    stop("`probs=` must be between 0 and 1.", call. = FALSE)
  }

  # getting predictions on the original model fit ------------------------------
  processed <- crr_mold(object$formula, newdata %||% object$data)
  matrix_pred <-
    stats::predict(object$original_fit, cov1 = as.matrix(processed$predictors))

  if (!is.null(times)) return(probs_at_times(matrix_pred, times))
  if (!is.null(probs)) return(times_at_probs(matrix_pred, probs))
}

times_at_probs <- function(matrix_pred, probs) {
  matrix_zero <- matrix(c(0, 0), ncol = 2)
  lst_time_risk <-
    purrr::map(
      probs,
      ~purrr::map_dbl(
        seq_len(ncol(matrix_pred) - 1L),
        function(i) {
          # browser()
          m <- rbind(matrix_zero, matrix_pred[, c(1L, i + 1L)])

          # return NA if the qunatiles are all missing OR prob is larger than observed
          if (all(is.na(m[-1, 2])) || .x >= max(m[, 2])) return(NA)
          return(m[which.min(m[, 2] <= .x), 1])
        }
      )
    ) %>%
    stats::setNames(paste0("prob ", probs * 100, "%"))

  # extracting vector of risks at specified time -------------------------------
  if (length(probs) == 1L)
    return(lst_time_risk[[1]])
  lst_time_risk
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
    as.list() %>%
    stats::setNames(paste("time", times))

  # extracting vector of risks at specified time -------------------------------
  if (length(times_obs) == 1L)
    return(lst_risk_time[[as.character(times_obs)]])

  lst_risk_time[as.character(times_obs)]
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



