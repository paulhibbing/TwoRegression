#' Internal functions for the 2RM wrapper
#'
#' @inheritParams hibbing_2018
#' @param varname character scalar. Name of variable to check
#' @param sensor character scalar indicating if \code{varname} corresponds to an
#'   accelerometer variable or gyroscope variable
#'
#' @keywords internal
#' @name hibbing-input-validation
TwoRegression_Hibbing18_variable_validate <- function(
  varname, AG, algorithm,
  sensor = c("accelerometer", "gyroscope", "magnetometer")
) {

  ## Verify that variable names have been provided for
  ## the necessary variables in the selected algorithm
  ## (runs for one algorithm at a time)

  sensor <- match.arg(sensor)

  if (sensor %in% "gyroscope") {

    if (!any(2:3 %in% algorithm)) {

      return(varname)

    } else {

      if (
        is.null(varname) | is.na(varname) | !inherits(varname, "character")
      ) stop(
        "You must provide a character scalar value for gyro_varname",
        call. = FALSE
      )

    }

  } else if (sensor %in% "magnetometer") {

    if (!3 %in% algorithm) {

     return(varname)

    } else {

      if (
        is.null(varname) | is.na(varname) | !inherits(varname, "character")
      ) stop(
        "You must provide a character scalar value for mag_varname",
        call. = FALSE
      )

    }

  } else if (sensor %in% "accelerometer") {

    ## Accelerometer is in all algorithms -- just test the varname
    if (
      is.null(varname) | is.na(varname) | !inherits(varname, "character")
    ) stop(
      "You must provide a character scalar value for accel_varname",
      call. = FALSE
    )

  } else {

    stop(
      "Unknown error in TwoRegression variable validation",
      " for Hibbing18 models", call. = FALSE
    )

  }

  arg <- switch(
    sensor,
    "accelerometer" = "accel_var",
    "gyroscope" = "gyro_var",
    "magnetometer" = "direction_var",
    stop("Error matching `sensor` with an `arg` value")
  )

  if (!varname %in% names(AG)) {
    stop(
      "The expected ", sensor, " variable name (", sQuote(varname),
      ") cannot be found in ", sQuote("AG"), "\nThe ", sQuote(arg),
      " argument must be set to an existing variable name",
      "\n  (Try passing `algorithm = 1` if you need a way of dodging this.)",
      call. = FALSE
    )
  }

  varname

}


#' @keywords internal
#' @rdname hibbing-input-validation
TwoRegression_Hibbing18_algorithm_validate <- function(algorithm) {

  if (!any(algorithm %in% 1:3)) {
    message(
      "\nAlgorithm(s) {", paste(algorithm, collapse = ", "),
      "} does/do not exist. Setting to algorithm #1 (accelerometer only)"
    )
    algorithm <- 1
  }

  if (!all(algorithm %in% 1:3)) {
    message_update(23, is_message = TRUE)
    algorithm %<>% {.[. %in% 1:3]}
  }

  algorithm

}
