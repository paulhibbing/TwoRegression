#' Calculate vector magnitude
#'
#' @param triaxial a dataframe of triaxial data on which to calculate vector magnitude
#' @param verbose print information about variable search criteria?
#'
#' @examples
#' \dontrun{
#' data(imu_to_collapse)
#'
#' vm_columns <-
#'     grepl("accelerometer",
#'         names(imu),
#'         ignore.case = T)
#'
#' get_VM(imu[, vm_columns])
#' }
#'
#' @return a vector of vector magnitude values
#' @keywords internal
get_VM <- function(triaxial, verbose = FALSE) {
  if (verbose) {
    vm_variables <-
      gsub("\"", "", substring(deparse(substitute(triaxial)), unlist(gregexpr(
        "\"", deparse(substitute(triaxial))
      ))[1],
        unlist(gregexpr(
          "\"", deparse(substitute(triaxial))
        ))[2]))

    if (verbose) message_update(2, vm_variables = vm_variables)
  }
  triaxial <- triaxial[, !grepl("VM", names(triaxial))]
  stopifnot(ncol(triaxial) == 3)
  apply(triaxial, 1, function(x) sqrt(sum(x^2)))
}

#' Low-Pass filter the Gyroscope data at 35 Hz
#'
#' @inheritParams check_second
#'
#' @examples
#' \dontrun{
#' data(imu_to_collapse)
#' imu_filter_gyroscope(imu)
#' }
#'
#' @keywords internal
imu_filter_gyroscope <- function(AG, samp_rate, verbose = FALSE) {
  if (verbose) message_update(5)
  AG[, grepl("gyroscope", names(AG), ignore.case = T)] <-
    sapply(AG[, grepl("gyroscope", names(AG),
      ignore.case = T)], function(x) {
        seewave::bwfilter(
          wave = x,
          f = samp_rate,
          n = 2,
          to = 35
        )
      })
  if (verbose) message_update(6)
  return(AG)
}

#' Convert magnetometer signal to cardinal direction
#'
#' @param x x-axis magnetometer data
#' @param y y-axis magnetometer data
#' @param z z-axis magnetometer data
#' @param orientation the conversion scheme to use, from c("vertical",
#'   "horizontal")
#'
#' @examples
#' \dontrun{
#' data(imu_to_collapse)
#'
#' X <- mean(imu$Magnetometer.X)
#' Y <- mean(imu$Magnetometer.Y)
#' Z <- mean(imu$Magnetometer.Z)
#'
#' classify_magnetometer(X, Y, Z)
#' }
#'
#' @seealso
#'   \url{http://s3.amazonaws.com/actigraphcorp.com/wp-content/uploads/2017/11/26205750/ActiGraph_IMU_White_Paper.pdf}
#'
#' @keywords internal
classify_magnetometer <- function(x = "Magnetometer X", y = "Magnetometer Y", z = "Magnetometer Z", orientation = "vertical") {

  if (length(x) != length(y)) {
    message_update(19, is_message = TRUE)
    return(NULL)
  }
  n <- length(x)
  if (length(x) > 1 | length(y) > 1) {
    x <- mean(x)
    y <- mean(y)
    message_update(20, n = n, is_message = TRUE)
  }

  ## Calculate direction for vertical orientation
  zdir <- as.character(cut(z, c(-Inf, -22, -16.71, -11.43, -6.14, -0.86, 4.43, 9.71, 15, Inf), c("N", "NNx",
    "Nx", "xNx", "x", "xSx", "Sx", "SSx", "S"), right = F))


  dir <- if (grepl("x", zdir, ignore.case = T)) {
    gsub("x", if (x > 22)
      "E" else "W", zdir)
  } else zdir

  ## Calculate direction for non-vertical orientation
  if (orientation != "vertical") {
    xdir <- as.character(cut(x, c(-Inf, -6, -0.29, 5.43, 11.14, 16.86, 22.57, 28.29, 34, Inf), c("N", "NNy",
      "Ny", "yNy", "y", "ySy", "Sy", "SSy", "S"), right = F))


    dir <- if (grepl("y", xdir, ignore.case = T)) {
      gsub("y", if (y > 4)
        "E" else "W", xdir)
    } else xdir
  }

  return(rep(dir, n))
}
