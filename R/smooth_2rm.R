#' Smooth two-regression estimates over specified periods
#'
#' @inheritParams TwoRegression-Function
#' @param unit the interval to use for smoothing (see
#'   \code{\link[lubridate]{floor_date}}). Default is \code{"60 sec"}
#'
#' @return Smoothed data, collapsed in the specified intervals
#'
#' @examples
#'
#' data(all_data, package = "TwoRegression")
#'
#'   result <- TwoRegression(
#'     all_data, "Hibbing 2018", gyro_var = "Gyroscope_VM_DegPerS",
#'     direction_var = "mean_magnetometer_direction",
#'     site = c("Left Ankle", "Right Ankle"), algorithm = 1:2
#'   )
#'
#'   smooth_2rm(result)
#'
#' @export
smooth_2rm <- function(
  AG, time_var = "Timestamp", unit = "60 sec", verbose = FALSE
) {

  AG %T>%
  {if (verbose) cat(
    "\nSmoothing data (collapsing every ", unit, ")", sep = ""
  )} %>%
  dplyr::group_by(
    !!as.name(time_var) := lubridate::floor_date(
      !!as.name(time_var), unit
    )
  ) %>%
  dplyr::summarise(

    dplyr::across(
      where(function(x) !is.numeric(x)) & !dplyr::matches("Classification$"),
      dplyr::first
    ),

    dplyr::across(
      where(is.numeric) & !dplyr::matches(c("METs$", "CV10s", "cv_10")),
      sum,
      na.rm = TRUE
    ),

    dplyr::across(
      dplyr::matches("Classification$"),
      function(x) dplyr::tibble(
        SB_epochs = sum(x == "SB"),
        walkrun_epochs = sum(x == "walkrun"),
        intermittent_epochs = sum(x == "intermittent")
      )
    ),

    dplyr::across(
      dplyr::matches(c("METs$", "CV10s", "cv_10")),
      mean,
      na.rm = TRUE
    ),

    .groups = "drop"

  ) %>%
  tidyr::unpack(
    dplyr::matches("Classification$"),
    names_sep = "_"
  ) %>%
  stats::setNames(., gsub("Classification_", "", names(.))) %>%
  dplyr::relocate(!dplyr::matches("epochs$")) %>%
  dplyr::relocate(!dplyr::matches("METs$")) %>%
  data.frame(stringsAsFactors = FALSE)

}
