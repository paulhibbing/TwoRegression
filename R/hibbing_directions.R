#' Calculate direction changes per five seconds in sliding windows
#'
#' \code{get_directions} is legacy code, whereas \code{get_dcp5} is cleaner and
#' (possibly) faster
#'
#' @inheritParams get_cv
#'
#' @return a numeric vector of values, giving the number of direction
#'   changes in the sliding window that corresponds to each epoch of data
#'
#' @name directions
#' @keywords internal
get_directions <- function(x, window_size = 5) {

  ## Get set up

  if (window_size%%2 != 1) stop(
    "window_size must be an odd number, to look forward and",
    " backward of the observation by equal amounts.", call. = FALSE
  )

  window_size <- (window_size - 1)/2

  inds <-
    seq_along(x) %>%
    sapply(
      function(i, N, window_size) {
        if ((i - window_size > 0) & (i + window_size <= length(x))) {
          seq(i - window_size, i + window_size)
        } else NA
      },
      N = length(x),
      window_size = window_size,
      simplify = FALSE
    )

  ## Run the operation

  inds %>%
    sapply(
      function(i, x) {

        if (is.na(i[1])) return(NA)

        as.factor(x[i]) %>%
        as.numeric(.) %>%
        diff(.) %>%
        {ifelse(. == 0, 0, 1)} %>%
        sum(.)

      },
      x = x
    )

}

#' @rdname directions
#' @keywords internal
get_dcp5 <- function(x, window_size = 5) {

  ## Number of windows
  window_size %T>%
  {if (.%%2 != 1) stop(
    "`get_dcp5` only works for odd-numbered window_size"
  )} %>%
  {length(x) - (window_size - 1)} %>%
  ## Indices of each window
  seq(.) %>%
  lapply(`+`, 0:(window_size - 1)) %>%
  ## Changes each window
  sapply(
    function(i, x) {
      ## Subtract 1 because first row doesn't count as a change
      length(rle(x[i])$values) - 1
    },
    x = if (is.factor(x) | !is.atomic(x)) as.character(x) else x
  ) %>%
  ## Pad to appropriate length
  append(rep(NA, (window_size - 1)/2)) %>%
  append(rep(NA, (window_size - 1)/2), 0)

}
