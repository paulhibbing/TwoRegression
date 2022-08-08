#' Develop and Apply Two-Regression Algorithms
#'
#' The TwoRegression package is designed to make working with
#' two-regression algorithms quick, easy, and accurate.
#'
#' Originally, the package was designed to house the algorithms created by
#' Hibbing et al. (2018). Since then, support has been added for other
#' algorithms, including Crouter et al. (2006), Crouter et al. (2010), and
#' Crouter et al. (2012). Functionality has also been added to develop and
#' cross-validate new two-regression algorithms. The package \code{RcppRoll}
#' has also been invoked to speed up rolling coefficient of variation
#' calculations.
#'
#' @section Associated References:
#'
#' Hibbing PR, LaMunion SR, Kaplan AS, & Crouter SE (2018). Estimating
#' energy expenditure with ActiGraph GT9X Inertial Measurement Unit.
#' \emph{Medicine and Science in Sports and Exercise}. 50(5), 1093-1102.
#' doi: 10.1249/MSS.0000000000001532
#'
#' Crouter, S. E., Clowers, K. G., & Bassett Jr, D. R. (2006). A novel method
#' for using accelerometer data to predict energy expenditure. \emph{Journal of
#' Applied Physiology}, 100(4), 1324-1331.
#'
#' Crouter, S. E., Kuffel, E., Haas, J. D., Frongillo, E. A., & Bassett Jr, D.
#' R. (2010). Refined Two-Regression Model for the ActiGraph Accelerometer.
#' \emph{Medicine and Science in Sports and Exercise}, 42(5), 1029.
#'
#' Crouter, S. E., Horton, M., & Bassett Jr, D. R. (2012). Use of a
#' Two-regression model for estimating energy expenditure in children.
#' \emph{Medicine and Science in Sports and Exercise}, 44(6), 1177.
#'
#' @examples
#' \donttest{
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
#' }
#'
#'
#' @docType package
#' @name TwoRegression-Package
NULL
