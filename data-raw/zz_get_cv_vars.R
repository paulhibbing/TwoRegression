#' @param accel_varname character. Name of accelerometer data column
#' @param gyro_varname character. Name of gyroscope data column
#' @rdname get_cv
#' @keywords internal
get_cv_vars <- function(
    Algorithm, accel_varname = "ENMO",
    gyro_varname = "Gyroscope_VM_DegPerS", verbose = FALSE
) {

  cvs <-
    c(accel_varname, gyro_varname, gyro_varname) %>%
    {.[1:3 %in% Algorithm]} %>%
    unique(.)

  if(verbose) message_update(11, cvs = cvs)
  if(verbose) message_update(12)

  cvs

}


