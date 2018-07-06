#' Calculate coefficient of variation in sliding windows
#'
#' Calculates coefficient of variation using the approach of Crouter et al. (2010, \emph{Med Sci Sports Exerc})
#'
#' @param big_data a numeric vector on which to perform the calculation
#' @param window_size width of the sliding window, in data points
#' @param verbose logical. Print progress updates?
#' @param sliding logical. Make calculations using a sliding window approach?
#'
#' @return a numeric vector of values, giving the lowest coefficient of
#'   variation among the sliding windows that correspond to each epoch of data
#'   (if \code{sliding} is \code{TRUE}), or giving the static coefficient of
#'   variation within each grouping of data (if \code{sliding} is \code{FALSE}).
#'
#' @examples
#' data(raw_for_cv)
#' get_cvPER(raw_for_cv$ENMO)
#'
#' @export
get_cvPER <- function(big_data, window_size = 10, verbose = FALSE,
  sliding = TRUE) {
  if (!sliding) return(get_cv_static(big_data, window_size, verbose))
  if (verbose) message_update(13, window_size = window_size)

  indices <-
    sapply(seq(length(big_data ) - (window_size - 1)),
      function(x) x + (seq(window_size) - 1),
      simplify = FALSE)

  stopifnot(min(unlist(indices)) == 1,
    max(unlist(indices)) == length(big_data ))

  cvs <-
    sapply(indices, function(x) cv(big_data [x]))

  cvs <-
    c(rep(NA, window_size - 1),
      cvs,
      rep(NA, window_size - 1))

  cvs <-
    sapply(seq(length(big_data )),
      function(x) {
        indices <- x:(x + (window_size - 1))
        if(all(is.na(cvs[indices]))) return(NA)
        min(cvs[indices], na.rm = TRUE)
      },
      simplify = FALSE)

  cvs <- do.call(c, cvs)

  if (verbose) message_update(6)
  return(cvs)
}
