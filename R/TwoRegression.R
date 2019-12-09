if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Process Data from Wearable Research Devices Using Two-Regression Algorithms
#'
#' The TwoRegression package is designed to make implementation of
#' two-regression algorithms quick, easy, and accurate.
#'
#' Originally, the package was designed to house the algorithms created by
#' Hibbing et al. (2018). Since then, support has been added for other
#' algorithms, including Crouter et al. (2006), Crouter et al. (2010), and
#' Crouter et al. (2012). Functionality has also been added to develop and
#' cross-validate new two-regression algorithms.
#'
#' @section Associated References:
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
#' \dontrun{
#' raw_file <-
#'     system.file("extdata",
#'         "TestID_LeftWrist_RAW.csv",
#'         package = "TwoRegression")
#'
#' imu_file <-
#'     system.file("extdata",
#'         "TestID_LeftWrist_IMU.csv",
#'         package = "TwoRegression")
#'
#' wear <- "Left Wrist"
#' id <- "Test"
#' alg <- 1:2
#'
#' hibbing18_twoReg_process(raw_file, imu_file, wear, id, alg)
#' }
#'
#'
#' @docType package
#' @name TwoRegression
NULL

#' @import ggplot2
NULL

#' @import magrittr
NULL

#' @importFrom stats predict sd setNames lm
NULL

#' @importFrom utils read.csv data
NULL
