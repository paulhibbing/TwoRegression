#' Calculate coefficient of variation for groupings of accelerometer data points
#'
#' @inheritParams get_cvPER
#'
#' @return a numeric vector of coefficient of variation values
#'
#' @keywords internal
get_cv_static <- function(big_data, window_size = 10, verbose = FALSE) {

  if (verbose) message_update(31, window_size = window_size)
  block <- rep(
    seq(ceiling(length(big_data) / window_size)),
    each = window_size
  )[seq(length(big_data))]

  cvs <- tapply(big_data, block, cv)
  ifelse(table(block) == window_size, cvs, NA)
}
