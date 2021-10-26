# model_frame
#' @export
# model_frame.tidycrr <- function(object, ...){
#   processed <- hardhat::mold(object$formula, object$model)
#   frame_tib <- tibble::as_tibble(cbind(processed$outcomes,processed$predictors))
#   frame_tib
# }
#
# # Recipe method
# crr.recipe <- function(x, data, failcode = NULL, ...) {
#
#   # checking inputs and assigning the numeric failcode -------------------------
#   failcode_numeric <-
#     as_numeric_failcode(formula = formula, data = data, failcode = failcode)
#
#   # building model -------------------------------------------------------------
#   processed <- hardhat::mold(formula, data)
#   crr_bridge(processed, formula, failcode_numeric, data)
# }
