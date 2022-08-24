#' @param accel_var Character scalar. Name of accelerometer variable to
#'   operate on (expected format is Euclidian Norm Minus One, in
#'   milli-gravitational units)
#' @param gyro_var character scalar. Name of gyroscope variable to operate
#'   on (expected format is gyroscope vector magnitude, in degrees per second)
#' @param direction_var character scalar. Name of magnetometer variable to
#'   operate on (expected format is a vector of directions, likely produced by
#'   \code{AGread::classify_magnetometer})
#' @param site character scalar or vector of attachment sites (more than one may
#'   be desired, e.g., if results are to be compared from running both of the
#'   wrist-specific algorithms)
#' @param algorithm An integer/numeric scalar or vector giving the algorithm(s)
#'   to apply to the data from the primary accelerometer and (if applicable)
#'   IMU. Must be \code{1} (accelerometer only), \code{2} (accelerometer and
#'   gyroscope), \code{3} (accelerometer, gyroscope, and magnetometer), or any
#'   combination thereof
#' @param smooth logical. Should data be averaged over a longer time period after processing?
#'
#'
#' @rdname TwoRegression-Function
#' @keywords internal
hibbing_2018 <- function(
  AG, accel_var = "ENMO", gyro_var = "GVM",
  direction_var = "Direction", time_var = "Timestamp",
  site = c("Hip", "Left Wrist", "Right Wrist", "Left Ankle", "Right Ankle"),
  algorithm = 1:3, smooth = FALSE, verbose = FALSE, ...
) {

  timer <- PAutilities::manage_procedure(
    "Start",
    "\nApplying Hibbing 2018 two-regression model(s)",
    verbose = verbose
  )

  site <- match.arg(
    site,
    c("Hip", "Left Wrist", "Right Wrist", "Left Ankle", "Right Ankle"),
    TRUE
  )


  ## Check variables

    accel_var %<>% TwoRegression_Hibbing18_variable_validate(
      AG, algorithm, "accelerometer"
    )


    gyro_var %<>% TwoRegression_Hibbing18_variable_validate(
      AG, algorithm, "gyroscope"
    )

    if (!gyro_var %in% names(AG)) AG[ ,gyro_var] <- NA


    direction_var %<>% TwoRegression_Hibbing18_variable_validate(
      AG, algorithm, "magnetometer"
    )

    if (!direction_var %in% names(AG)) AG[ ,direction_var] <- NA


    stopifnot(
      time_var %in% names(AG),
      inherits(AG[ ,time_var], "POSIXt"),
      epoch_length(AG[ ,time_var]) == 1
    )


  ## Check algorithm

    algorithm %<>% TwoRegression_Hibbing18_algorithm_validate(.)


  ## Get CVs

    AG <-
      get_cv_vars(algorithm, accel_var, gyro_var, verbose) %>%
      sapply(
        function(x, AG, verbose) cv_2rm(AG[ ,x], verbose = FALSE),
        AG = AG, verbose = verbose
      ) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      stats::setNames(., gsub(accel_var, "ENMO_CV10s", names(.))) %>%
      stats::setNames(., gsub(gyro_var, "GVM_CV10s", names(.))) %>%
      data.frame(AG, .)


  ## Fix names for model implementation -- will undo later

    AG %<>%
      dplyr::rename("ENMO" := !!as.name(accel_var)) %>%
      {if (any(2:3 %in% algorithm))
        dplyr::rename(., "Gyroscope_VM_DegPerS" := !!as.name(gyro_var))
        else .} %>%
      {if (3 %in% algorithm)
        dplyr::rename(., "direction_var" := !!as.name(direction_var))
        else .}

    other_direction_var <-
      ## Accounts for the case where direction_var is some other
      ## variable name, but 'Direction' still exists in the data
      direction_var != "Direction" & "Direction" %in% names(AG) &
      3 %in% algorithm

    if (other_direction_var) {
      ## Further issues will only arise if there is ALSO a lowercase `direction`
      ## variable in the dataset. But `rename` will throw an error in that case,
      ## and it would be bad practice on the user's part to have both
      ## `direction` and `Direction` in the dataset, so I'm comfortable with the
      ## way this is handled
      AG %<>% dplyr::rename("direction" = "Direction")
    }

    if (3 %in% algorithm) {
      AG$Direction <- get_dcp5(AG$direction_var)
    }


  ## Get the predictions

    all_processes <-
      expand.grid(
        Wear_Location = site,
        Algorithm = algorithm,
        stringsAsFactors = FALSE
      ) %>%
      split(., seq(nrow(.)))

    all_predictions <-
      lapply(
        all_processes,
        apply_two_regression_hibbing18,
        AG = AG,
        verbose = verbose
      ) %>%
      lapply(
        dplyr::select,
        dplyr::all_of(c("Classification", "METs"))
      ) %>%
      do.call(cbind, .) %>%
      data.frame(stringsAsFactors = FALSE)

    names(all_predictions) <-
      lapply(
        all_processes,
        function(x) {
          paste(
            gsub(" ", "_", x$Wear_Location),
            paste("Algorithm", x$Algorithm, sep = ""),
            c("Classification", "METs"),
            sep = "_"
          )
        }
      ) %>%
      unlist(.)


  ## Final formatting and output of the data

    AG %<>% cbind(all_predictions)

    if (smooth) AG %<>% smooth_2rm(time_var, verbose = verbose, ...)

    AG %>%
    dplyr::rename(!!as.name(accel_var) := "ENMO") %>%
    {if (any(2:3 %in% algorithm))
      dplyr::rename(., !!as.name(gyro_var) := "Gyroscope_VM_DegPerS")
      else .} %>%
    {if (3 %in% algorithm)
      dplyr::rename(
        .,
        "direction_changes_5s" = "Direction",
        !!as.name(direction_var) := "direction_var"
      )
      else .} %>%
    {if (other_direction_var)
      dplyr::rename(., "Direction" = "direction")
      else .} %T>%
    {PAutilities::manage_procedure(
      "End", "\nProcess complete -- Elapsed time:",
      PAutilities::get_duration(timer), "mins",
      verbose = verbose
    )}

}
