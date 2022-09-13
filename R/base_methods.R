#' Estimate subdistribution functions for crr objects
#'
#' @param object a tidycrr object
#' @param times Numeric vector of times to obtain risk estimates at
#' @param probs Numeric vector of quantiles to obtain estimates at
#' @param newdata A `base::data.frame()` or `tibble::tibble()` containing all
#' the original predictors used to create x. Defaults to `NULL`.
#' @inheritParams base_methods_crr
#'
#' @return named list of prediction estimates
#' @family crr() functions
#' @export
#' @examples
#' crr(Surv(ttdeath, death_cr) ~ age, trial) %>%
#'   predict(times = 12, newdata = trial[1:10, ])
predict.tidycrr <- function(object, times = NULL, probs = NULL, newdata = NULL, ...) {
  # checking inputs ------------------------------------------------------------
  if (is.null(times) + is.null(probs) != 1L) {
    stop("Must specify one and only one of `times=` and `probs=`.", call. = FALSE)
  }
  if (!is.null(times) && any(times < 0)) {
    stop("`times=` must be non-negative.", call. = FALSE)
  }
  if (!is.null(probs) && !all(dplyr::between(probs, 0, 1))) {
    stop("`probs=` must be between 0 and 1.", call. = FALSE)
  }

  # getting predictions on the original model fit ------------------------------
  processed <- crr_mold(object$formula, newdata %||% object$data)
  matrix_pred <-
    stats::predict(object$cmprsk, cov1 = as.matrix(processed$predictors))

  if (!is.null(times)) {
    return(probs_at_times(matrix_pred, times))
  }
  if (!is.null(probs)) {
    return(times_at_probs(matrix_pred, probs))
  }
}

times_at_probs <- function(matrix_pred, probs) {
  matrix_zero <- matrix(c(0, 0), ncol = 2)
  lst_time_risk <-
    purrr::map(
      probs,
      ~ purrr::map_dbl(
        seq_len(ncol(matrix_pred) - 1L),
        function(i) {
          # adding 0 to the matrix, keeping only the time column, and col of interest
          m <- rbind(matrix_zero, matrix_pred[, c(1L, i + 1L)])

          # return NA if the quantiles are all missing OR prob is larger than observed
          if (isTRUE(all(is.na(m[-1, 2])) || .x > max(m[, 2]))) {
            return(NA)
          }
          if (isTRUE(.x %in% m[, 2])) {
            return(m[m[, 2] %in% .x, 1])
          }
          return(m[which.min(m[, 2] <= .x), 1])
        }
      )
    ) %>%
    stats::setNames(paste0("prob ", probs * 100, "%"))

  # returning results ----------------------------------------------------------
  lst_time_risk
}


probs_at_times <- function(matrix_pred, times) {
  # defining times for predictions ---------------------------------------------
  all_times <- union(0, matrix_pred[, 1]) %>% sort()
  if (isTRUE(max(times) > max(all_times))) {
    stringr::str_glue("`times=` cannot be larger than {max(all_times)}") %>%
      stop(call. = FALSE)
  }
  times_obs <-
    purrr::map_dbl(
      times,
      function(.x) {
        if (isTRUE(.x %in% all_times)) {
          return(all_times[all_times %in% .x])
        }
        all_times[which.min(all_times <= .x) - 1L]
      }
    )

  # named list of the risks, the names are the times,
  # the values are the estimates of risk at the covar levels
  lst_risk_time <-
    purrr::map(seq_len(length(all_times) - 1L), ~ matrix_pred[.x, -1]) %>%
    stats::setNames(all_times[-1]) %>%
    dplyr::bind_cols() %>%
    mutate(`0` = 0, .before = 1) %>%
    as.list() %>%
    stats::setNames(all_times)

  # extracting risks at specified time -----------------------------------------
  lst_risk_time[as.character(times_obs)] %>%
    stats::setNames(paste("time", times))
}


#' Functions for tidycrr objects
#'
#' @param x,object a tidycrr object
#' @param formula a formula
#' @param ... not used
#'
#' @return coef vector, model matrix, model frame, terms object
#' @name base_methods_crr
#' @examples
#' mod <- crr(Surv(ttdeath, death_cr) ~ age + grade, trial)
#'
#' coef(mod)
#'
#' model.matrix(mod) %>% head()
#'
#' model.frame(mod) %>% head()
#'
#' terms(mod)
NULL

#' @export
#' @rdname base_methods_crr
coef.tidycrr <- function(object, ...) {
  object$coefs
}

#' @export
#' @rdname base_methods_crr
vcov.tidycrr <- function(object, ...) {
  object$cmprsk$var
}

#' @export
#' @rdname base_methods_crr
model.matrix.tidycrr <- function(object, ...) {
  # by default there is no intercept term in F&G's model
  stats::model.matrix(object$formula, object$data)[, -1, drop = FALSE]
}

#' @export
#' @rdname base_methods_crr
model.frame.tidycrr <- function(formula, ...) {
  stats::model.frame(formula = formula$formula, data = formula$data)
}

#' @export
#' @rdname base_methods_crr
terms.tidycrr <- function(x, ...) {
  stats::terms(x = x$formula, data = x$data)
}


#' Functions for tidycuminc objects
#'
#' @param object a tidycuminc object
#' @param formula a formula
#' @param ... not used
#'
#' @return a model frame, or model matrix
#' @name base_methods_cuminc
#' @examples
#' fit <- cuminc(Surv(ttdeath, death_cr) ~ trt, trial)
#'
#' model.matrix(fit) %>% head()
#'
#' model.frame(fit) %>% head()
NULL

#' @export
#' @rdname base_methods_cuminc
model.frame.tidycuminc <- function(formula, ...) {
  stats::model.frame(formula = formula$formula, data = formula$data)
}

#' @export
#' @rdname base_methods_cuminc
model.matrix.tidycuminc <- function(object, ...) {
  # by default there is no intercept term in F&G's model
  stats::model.matrix(object$formula, object$data)[, -1, drop = FALSE]
}
