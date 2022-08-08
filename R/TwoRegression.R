#' Apply an existing two-regression model
#'
#' Acts as a generic function that dispatches to specific implementation
#' functions (\code{\link{crouter_2006}}, \code{\link{crouter_2010}},
#' \code{\link{crouter_2012}}, or \code{\link{hibbing_2018}})
#'
#' @param AG data frame of actigraph data
#' @param method character scalar telling which model to apply to the data.
#'   Currently supported selections are \code{"Crouter 2006"}, \code{"Crouter
#'   2010"}, \code{"Crouter 2012"}, and \code{"Hibbing 2018"}. See following
#'   subsections
#' @param verbose logical. Print updates to console?
#' @param ... arguments passed to the implementation function indicated by
#'   \code{method} (and further passed to \code{\link{smooth_2rm}}) when
#'   \code{method == "Hibbing 2018"}
#'
#' @seealso
#' \href{https://pubmed.ncbi.nlm.nih.gov/16322367/}{Crouter et al. (2006, \emph{J Appl Physiol})}
#' \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2891855/}{Crouter et al. (2010, \emph{Med Sci Sports Exerc})}
#' \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3324667/}{Crouter et al. (2012, \emph{Med Sci Sports Exerc})}
#' \href{https://pubmed.ncbi.nlm.nih.gov/29271847/}{Hibbing et al. (2018,
#' \emph{Med Sci Sports Exerc})}
#'
#' \code{\link{apply_two_regression_hibbing18}}
#' \code{\link{smooth_2rm}}
#'
#'
#' @return The original data appended with columns giving activity
#'   classification (sedentary, ambulatory, or intermittent) and energy
#'   expenditure (i.e, METs)
#'
#'
#'
#' @examples
#'
#' ## Datasets
#'
#'   data(count_data, package = "TwoRegression")
#'   data(all_data, package = "TwoRegression")
#'
#' ## Crouter 2006-2012 models
#'
#'   TwoRegression(
#'     count_data, "Crouter 2006",
#'     movement_var = "Axis1", time_var = "time"
#'   )
#'
#'   TwoRegression(
#'     count_data, "Crouter 2010",
#'     movement_var = "Axis1", time_var = "time"
#'   )
#'
#'   TwoRegression(
#'     count_data, "Crouter 2012", movement_var = "Axis1",
#'     time_var = "time", model = "VA", check = FALSE
#'   )
#'
#'   TwoRegression(
#'     count_data, "Crouter 2012", movement_var = "Vector.Magnitude",
#'     time_var = "time", model = "VM", check = FALSE
#'   )
#'
#' ## Hibbing 2018 models (can be vectorized)
#'
#'   all_data$ENMO_CV10s <- NULL
#'   all_data$GVM_CV10s  <- NULL
#'   all_data$Direction  <- NULL
#'
#'   result <- TwoRegression(
#'     all_data, "Hibbing 2018", gyro_var = "Gyroscope_VM_DegPerS",
#'     direction_var = "mean_magnetometer_direction",
#'     site = c("Left Ankle", "Right Ankle"), algorithm = 1:2
#'   )
#'
#'   utils::head(result)
#'
#' @name TwoRegression-Function
#' @export
#'
TwoRegression <- function(
    AG,
    method = c("Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018"),
    verbose = FALSE,
    ...
) {

  method <- match.arg(method)

  stopifnot(inherits(AG, "data.frame"))

  timer <- PAutilities::manage_procedure(
    "Start", "\nBeginning TwoRegression procedures", verbose = verbose
  )

  switch(
    method,
    "Crouter 2006" = crouter_2006(AG, ...),
    "Crouter 2010" = crouter_2010(AG, ...),
    "Crouter 2012" = crouter_2012(AG, ...),
    "Hibbing 2018" = hibbing_2018(AG, verbose = verbose, ...)
  ) %T>%
  {PAutilities::manage_procedure(
    "End",
    "\nTwo-Regression processing complete. Total processing time:",
    PAutilities::get_duration(timer), "minutes\n\n",
    verbose = verbose
  )}

}
