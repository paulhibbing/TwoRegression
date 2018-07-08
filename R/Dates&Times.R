#' Numerical Minute of the Day.
#'
#' Converts a timestamp to a numerical value between 0 (midnight) and 1439
#' (23:59). Seconds can be represented using a rational decimal.
#'
#' @param timestamp A character vector containing timestamp information
#' @param format The date-time format of the \code{timestamp} vector
#' @param rational A logical scalar. Use rational number to represent seconds?
#'
#' @examples
#' \dontrun{
#' key_times <-
#'     paste("2018-03-15",
#'           c("00:00:00",
#'             "01:00:00",
#'             "12:00:00",
#'             "23:59:59"))
#'
#' # Warns about deprecation
#'
#' TwoRegression:::get_minute(key_times)
#' TwoRegression:::get_minute(key_times, rational = TRUE)
#'
#' # Use the following instead
#'
#' AGread:::get_minute(key_times)
#' AGread:::get_minute(key_times, rational = TRUE)
#'
#' }
#'
#' @name get_minute-deprecated
#' @usage get_minute(timestamp, format = "\%Y-\%m-\%d \%H:\%M:\%S", rational = FALSE)
#' @seealso \code{\link{TwoRegression-deprecated}}
#' @keywords internal
NULL

#' @rdname TwoRegression-deprecated
#' @section \code{get_minute}:
#' For \code{get_minute}, use \code{\link[AGread]{get_minute}}
#'
#' @keywords internal
get_minute <- function(timestamp, format = "%Y-%m-%d %H:%M:%S", rational = FALSE) {

    .Deprecated("AGread::get_minute")

    AGread::get_minute(timestamp, format, rational)
}


#' Julian Date
#'
#' A wrapper to retrieve the Julian date.
#' @inheritParams get_minute
#'
#' @return A numeric vector of Julian dates.
#'
#' @examples
#' \dontrun{
#' key_dates <- c("2018-01-01", "2018-12-31")
#'
#' # Warns about deprecation
#' TwoRegression:::get_day_of_year(key_dates, "%Y-%m-%d")
#'
#' # Use the following instead
#' AGread::get_day_of_year(key_dates, "%Y-%m-%d")
#' }
#'
#' @name get_day_of_year-deprecated
#' @usage get_day_of_year(timestamp, format = "\%Y-\%m-\%d \%H:\%M:\%S")
#' @seealso \code{\link{TwoRegression-deprecated}}
#' @keywords internal
NULL

#' @rdname TwoRegression-deprecated
#' @section \code{get_day_of_year}:
#' For \code{get_day_of_year} use \code{\link[AGread]{get_day_of_year}}
#'
#' @keywords internal
get_day_of_year <- function(timestamp, format = "%Y-%m-%d %H:%M:%S") {

    .Deprecated("AGread::get_day_of_year")

    AGread::get_day_of_year(timestamp, format)
}
