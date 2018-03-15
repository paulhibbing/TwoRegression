#' Identify mis-specified Hibbing 2018 algorithms, based on files provided
#'
#' @inheritParams hibbing18_twoReg_process
#'
#' @examples
#' \dontrun{
#' algorithm_verify(NULL, 4)
#' algorithm_verify(NULL, NULL)
#' }
#'
#' @keywords internal
algorithm_verify <- function(IMU, Algorithm) {
  if (!all(Algorithm %in% 1:3)) {
    message_update(23, is_message = TRUE)
    Algorithm <- Algorithm[Algorithm %in% 1:3]
  }

  if (length(Algorithm) == 0) {
    message_update(24, is_message = TRUE)
    return(1)
  }

  if (all(is.null(IMU), sum(Algorithm) != 1)) {
    message_update(17, is_message = TRUE)
    return(1)
  }

  return(Algorithm)
}

#' Identify IMU files that can be ignored when Algorithm = 1
#'
#' @inheritParams hibbing18_twoReg_process
#'
#' @examples
#' \dontrun{
#' imu_file <-
#'     system.file("extdata",
#'         "TestID_LeftWrist_IMU.csv",
#'         package = "TwoRegression")
#'
#' imu_verify(imu_file, 1, TRUE)
#' imu_verify(imu_file, 1, FALSE)
#' }
#'
#' @keywords internal
imu_verify <- function(IMU, Algorithm, IMU_ignore_A1) {
  if (all(!is.null(IMU), sum(Algorithm) == 1, IMU_ignore_A1)) {
    message_update(22, is_message = TRUE)
    return(NULL)
  }
  return(IMU)
}

#' Identify mis-specified Hibbing 2018 attachment site
#'
#' @inheritParams hibbing18_twoReg_process
#'
#' @examples
#' \dontrun{
#' attachment_verify("Left Wrist")
#' attachment_verify(c("Left Wirst", "Right Wrist"))
#' attachment_verify("Left Wirst")
#' }
#'
#' @keywords internal
attachment_verify <- function(Wear_Location) {
  valid_sites <-
    c("Hip", "Left Wrist", "Right Wrist", "Left Ankle", "Right Ankle")

  if (!all(Wear_Location %in% valid_sites)) {
    message_update(25, is_message = TRUE)
  }

  Wear_Location <- Wear_Location[Wear_Location %in% valid_sites]

  if (length(Wear_Location) == 0) {
    message_update(26, is_message = TRUE)
    Wear_Location <- "Hip"
  }

  return(Wear_Location)
}
