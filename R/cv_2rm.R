# Main function -----------------------------------------------------------


#' Coefficient of variation for two-regression models
#'
#' Calculates coefficient of variation using static or sliding methods, with
#' potential for custom methods as well
#'
#' @param x a numeric vector on which to perform the calculation
#' @param window_size width of the sliding window, in data points
#' @param approach character scalar naming the desired calculation approach to
#'   use. Can be \code{"sliding"} (the default; see Crouter et al., 2010),
#'   \code{"static"} (see Crouter et al., 2006), or \code{"custom"} (see details
#'   below)
#' @param verbose logical. Print progress updates?
#' @param pad_size The number of NA values to append at the start and end of
#'   \code{x} (when \code{approach == "custom"}) before executing the rolling
#'   minimum step.
#' @param ... arguments passed to functions in the \code{RcppRoll} package
#'
#' @return a numeric vector of values, giving the desired coefficient of variation
#'
#' @details For \code{approach == "sliding"}, the value for each epoch
#'   represents the lowest CV value of all the sliding windows that overlap with
#'   that epoch. For \code{aproach == "static"}, a truncated vector of CV values
#'   is given, which reflects CV values from a non-overlapping sliding window.
#'   For \code{approach == "custom"}, users can pass arguments into
#'   \code{RcppRoll} functions and create a variation on the sliding approach.
#'   Behavior of this feature is not well documented and subject to change if
#'   people start using it and requesting specific behavior.
#'
#' @examples
#' data(raw_for_cv)
#' cv_2rm(raw_for_cv$ENMO)
#'
#' @seealso
#'   \href{https://pubmed.ncbi.nlm.nih.gov/20400882/}{Crouter et al. (2010, \emph{Med Sci Sports Exerc})}
#'   \href{https://pubmed.ncbi.nlm.nih.gov/16322367/}{Crouter et al. (2006, \emph{J Appl Physiol})}
#'
#' @name get_cv
#' @export
cv_2rm <- function(
  x, window_size = 10, approach = c("sliding", "static", "custom"),
  verbose = FALSE, ...
) {

  approach <- match.arg(approach)

  if (verbose) cat("\n... Calculating a", approach, "CV")

  switch(
    approach,
    "static"  = static_cv(x, window_size),
    "sliding" = sliding_cv(x, window_size),
    "custom"  = custom_cv(x, window_size),
    stop(
      "Error determining a setting for `by`",
      " (see ?RcppRoll::`RcppRoll-exports`)",
      call. = FALSE
    )
  )

}


# Helper functions --------------------------------------------------------

#' @rdname get_cv
#' @keywords internal
static_cv <- function(x, window_size) {

  x %<>%
    length(.) %>%
    {. / window_size} %>%
    ceiling(.) %>%
    {. * window_size} %>%
    {x[1:.]}

  RcppRoll::roll_mean(x, window_size, by = window_size) %>%
  {ifelse(
    . == 0,
    0,
    RcppRoll::roll_sd(x, window_size, by = window_size) / . * 100
  )}

}

#' @rdname get_cv
#' @keywords internal
sliding_cv <- function(x, window_size) {

  RcppRoll::roll_mean(x, window_size) %>%
  {ifelse(
    . == 0,
    0,
    RcppRoll::roll_sd(x, window_size) / . * 100
  )} %>%
  append(rep(NA, window_size - 1), 0) %>%
  append(rep(NA, window_size - 1)) %>%
  RcppRoll::roll_min(window_size, na.rm = TRUE)

}

#' @rdname get_cv
#' @keywords internal
custom_cv <- function(x, window_size, pad_size = window_size - 1, ...) {

  warning(
    "Code behavior has not been tested for custom",
    " CVs -- use at your own risk!", call. = FALSE
  )

  RcppRoll::roll_mean(x, window_size, ...) %>%
  {ifelse(
    . == 0,
    0,
    RcppRoll::roll_sd(x, window_size, ...) / . * 100
  )} %>%
  append(rep(NA, pad_size), 0) %>%
  append(rep(NA, pad_size)) %>%
  RcppRoll::roll_min(window_size, na.rm = TRUE)

}

# Purely internal function ------------------------------------------------

get_cv_vars <- function(
  Algorithm, accel_var = "ENMO",
  gyro_var = "Gyroscope_VM_DegPerS", verbose = FALSE
) {

  cvs <-
    c(accel_var, gyro_var, gyro_var) %>%
    {.[1:3 %in% Algorithm]} %>%
    unique(.)

  if(verbose) message_update(11, cvs = cvs)

  cvs

}
