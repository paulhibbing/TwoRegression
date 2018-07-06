#' Calculate coefficient of variation
#'
#' @param signal the variable on which to perform calculation
#'
#' @examples
#' \dontrun{
#' TwoRegression:::cv(rep(0, 10))
#' TwoRegression:::cv(seq(1, 10))
#' }
#'
#' @keywords internal
#'
cv <- function(signal) {
  if (any(is.na(signal))) return(NA)
  if (mean(signal) == 0) {
    0
  } else {
    stats::sd(signal)/mean(signal) * 100
  }
}

#' Determine which variable(s) to calculate coefficient of variation on
#'
#' \code{get_cv_vars} returns the name(s) of variables on which to calculate the
#' coefficient of variation in sliding windows, depending on which
#' two-regression algorithm(s) have been designated for application to the data.
#'
#' @param Algorithm the algorithm(s) selected to process the accelerometer/IMU
#'   data
#'
#' @examples
#' \dontrun{
#' TwoRegression:::get_cv_vars(1)
#' TwoRegression:::get_cv_vars(2)
#' TwoRegression:::get_cv_vars(c(1,2))
#' }
#'
#' @keywords internal
get_cv_vars <- function(Algorithm, verbose = FALSE) {
  cvs <-
    unique(c("ENMO",
      "Gyroscope_VM_DegPerS",
      "Gyroscope_VM_DegPerS")[1:3 %in% Algorithm]
    )

  if(verbose) message_update(11, cvs = cvs)
  if(verbose) message_update(12)
  return(cvs)
}

#' Calculate coefficient of variation in sliding windows
#'
#' Calculates coefficient of variation using the approach of Crouter et al. (2010, \emph{Med Sci Sports Exerc}). Function speedup available using \code{\link{get_cvPER}}
#'
#' @param big_data a numeric vector on which to perform the calculation
#' @param window_secs size of the sliding window, in seconds
#' @inheritParams hibbing18_twoReg_process
#'
#' @return a numeric vector of values, giving the lowest coefficient of variation among the sliding windows that correspond to each epoch of data
#'
#' @examples
#' \dontrun{
#' data(raw_for_cv)
#' TwoRegression:::get_cvPER_old(raw_for_cv$ENMO, Algorithm = 1)
#' }
#'
#' @keywords internal
get_cvPER_old <- function(big_data, window_secs = 10, Algorithm, verbose = FALSE) {
    # if (verbose) message_update(13, window_secs = window_secs)

  inds <-
    sapply(seq(big_data),
          function(x) {
            sapply(window_secs:1, function(y) {
              ((x - y):(x - y + window_secs - 1)) + 1
            })
          }, simplify = F)

  CVS <-
    do.call(rbind,
      lapply(inds,
        function(x) {
            values <- sapply(data.frame(x), function(y) {
                Y <- y[y > 0 & y <= length(big_data)]
                if (length(y) != length(Y)) {
                  data.frame(CV = NA)
                } else {
                  data.frame(CV = cv(big_data[Y]))
                }
              }, simplify = F)

            CV <- sapply(do.call(rbind, values), min, na.rm = TRUE)
            return(CV)
    }
    ))

  stopifnot(ncol(CVS)==1 | is.vector(CVS))
  CVS <- as.vector(CVS)
  # if (verbose) message_update(6)
  return(CVS)
}

#' Calculate direction changes per five seconds in sliding windows
#'
#' @inheritParams get_cvPER_old
#'
#' @return a numeric vector of values, giving the number of direction changes in the sliding window that corresponds to each epoch of data
#'
#' @examples
#' \dontrun{
#' ##All possible directions
#' directions <-
#'   c("N", "NNE", "NE", "ENE",
#'     "E", "ESE", "SE", "SSE",
#'     "S", "SSW", "SW", "WSW",
#'     "W", "WNW", "NW", "NNW")
#'
#' ##Reproducible results
#' set.seed(55)
#' direction_vector <- sample(directions, 50, replace = TRUE)
#'
#' ##Vector of direction changes per 5-s. First and last two values are always NA
#' get_directions(direction_vector)
#' }
#'
#' @export
get_directions <- function(big_data, window_secs = 5) {
    if (window_secs%%2 != 1)
        stop("window_secs must be an odd number, to look forward and backward of the observation by equal amounts.")

    window_secs <- (window_secs - 1)/2

    inds <-
      sapply(seq_along(big_data), function(x) {
        if ((x - window_secs > 0) & (x + window_secs <= length(big_data))) {
          seq(x - window_secs, x + window_secs)
        } else
          NA
      }, simplify = F)

    changes <-
      sapply(inds, function(x) {
        if (is.na(x[1])) {
          NA
        } else {
          compareDirs <-
            diff(as.numeric(as.factor(big_data[x])))
          dirChange <-
            sum(ifelse(compareDirs == 0, 0, 1))
      }
    })

    return(changes)
}

#' Apply a Hibbing 2018 two-regression algorithm
#'
#' Applies the specified two-regression algorithm from \href{https://www.ncbi.nlm.nih.gov/pubmed/29271847}{Hibbing et al. (2018, \emph{Med Sci Sports Exerc})} to data from the primary accelerometer and IMU (if applicable)
#' @param which_algorithm a dataframe specifying which algorithm to use, based on \code{Wear_Location} and \code{Algorithm}
#' @param all_data a dataframe providing the processed GT9X data on which to make the predictions
#' @param verbose logical. Print processing updates?
#'
#' @return a numeric vector of predicted energy expenditure values, expressed in metabolic equivalents
#'
#' @examples
#' \dontrun{
#' data(all_data)
#' process  <-
#'     data.frame(Wear_Location = "Left Wrist",
#'         Algorithm = 2,
#'         stringsAsFactors = FALSE)
#'
#' TwoRegression:::apply_two_regression_hibbing18(process, all_data)
#' }
#' @keywords internal
apply_two_regression_hibbing18 <-
  function(which_algorithm = data.frame(Wear_Location = "Hip", Algorithm = 1),
    all_data, verbose) {

    Site <-
      sapply(which_algorithm$Wear_Location, function(x)
        switch(
          x,
          Hip = "Hip",
          `Left Wrist` = "LW",
          `Right Wrist` = "RW",
          `Left Ankle` = "LA",
          `Right Ankle` = "RA"
        ))

    matched_Algorithm <- Algorithms[[Site]]
    matched_Algorithm <- matched_Algorithm[[which_algorithm$Algorithm]]

    if (length(matched_Algorithm) == 0) {
      stop(paste("Didn't find a matching algorithm.",
        "This could take some work to figure out..."))
    }

    if (length(matched_Algorithm) != 8) {
      stop(paste("Found too many matching algorithms.",
        "This could take some work to figure out...",
        "Make sure there's only one wear",
        "location/algorithm passed to the function."
      ))
    }

    predict(matched_Algorithm, all_data, verbose)
}
