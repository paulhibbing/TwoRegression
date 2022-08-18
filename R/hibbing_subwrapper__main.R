#' Apply a Hibbing 2018 two-regression algorithm
#'
#' Applies the specified two-regression algorithm from
#' \href{https://pubmed.ncbi.nlm.nih.gov/29271847/}{Hibbing et al. (2018,
#' \emph{Med Sci Sports Exerc})} to data from the primary accelerometer and IMU
#' (if applicable)
#'
#' @param which_algorithm a dataframe specifying which algorithm to use, based
#'   on \code{Wear_Location} and \code{Algorithm} columns
#' @inheritParams TwoRegression-Function
#'
#' @return a numeric vector of predicted energy expenditure values, expressed in
#'   metabolic equivalents
#'
#' @examples
#' \donttest{
#' data(all_data, package = "TwoRegression")
#' process  <-
#'     data.frame(Wear_Location = "Left Wrist",
#'         Algorithm = 2,
#'         stringsAsFactors = FALSE)
#'
#' TwoRegression:::apply_two_regression_hibbing18(process, all_data)
#' }
#'
#' @name hibbing-helpers
#' @keywords internal
apply_two_regression_hibbing18 <- function(
  which_algorithm = data.frame(Wear_Location = "Hip", Algorithm = 1),
  AG, verbose = FALSE
) {

  Site <-
    which_algorithm$Wear_Location %>%
    sapply(function(x) switch(
      x,
      Hip = "Hip",
      `Left Wrist` = "LW",
      `Right Wrist` = "RW",
      `Left Ankle` = "LA",
      `Right Ankle` = "RA"
    ))

  matched_Algorithm <- Algorithms[[Site]]
  matched_Algorithm <- matched_Algorithm[[which_algorithm$Algorithm]]

  if (length(matched_Algorithm) == 0) stop(
    "Didn't find a matching algorithm. ",
    "This could take some work to figure out...",
    call. = FALSE
  )

  if (length(matched_Algorithm) != 9) stop(
    "Found too many matching algorithms. ",
     "This could take some work to figure out...",
     "\nMake sure there's only one wear ",
     "location/algorithm passed to the function.",
    call. = FALSE
  )

  predict(matched_Algorithm, AG, verbose)

}
