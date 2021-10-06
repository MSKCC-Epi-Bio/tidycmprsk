#' Broom methods for tidycmprsk objects
#'
#' @inheritParams broom::tidy.crr
#' @param new_data placeholder
#' @param x placeholder
#' @param quantiles placeholder
#' @param ... not used
#'
#' @name broom_methods
#' @return a tibble
NULL

# tidy
#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
tidy.tidycrr <- function(x, exponentiate = FALSE, conf.level = 0.95, ...){
  broom::tidy(
    x$original_fit, exponentiate = exponentiate, conf.level = conf.level, ...)
}

#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
glance.tidycrr <- function(x, ...){
  s <- summary(x$original_fit)
  as_glance_tibble(
    n = s$n,
    n.missing = s$n.missing,
    statistic.pseudoLRT = s$logtest[1],
    df.pseudoLRT = s$logtest[2],
    logpseudoLik = as.numeric(s$loglik),
    na_types = "iirrr"
  )
}

#' @rdname broom_methods
#' @export
#' @family tidycrr tidiers
augment.tidycrr <- function(x, quantiles = seq(0,1,0.25), new_data, ...){

  pred <- predict.tidycrr(x, new_data = x$model, quantiles = quantiles)
  out <- cbind(pred$newdata,
               pred$qout,
               pred$lpout)
  tibble::as_tibble(out)
}
