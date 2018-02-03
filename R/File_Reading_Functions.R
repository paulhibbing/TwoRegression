#' File reading function for primary accelerometer files
#'
#' @param file A character scalar giving path to primary accelerometer file
#' @param output_window the desired epoch length; defaults to one second
#'
#' @return A dataframe giving processed raw data from the primary accelerometer in the specified epoch length
#' @export
read_AG_raw <- function(file, output_window = 1, verbose = FALSE) {
  timer <- proc.time()

  if (verbose) message_update(1, file = file)

  meta <- get_raw_file_meta(file)

  AG <-
    data.table::fread(file, stringsAsFactors = F, showProgress = F, skip = 10)
  AG$Timestamp <-
    meta$start + ((seq(nrow(AG)) - 1) / meta$samp_freq)

  AG <- AG_collapse(AG, output_window, meta$samp_freq)

  ## Get ENMO
  ## Adapted from code written by Vincent van Hees
  ENMO <- sqrt(AG$Gx^2 + AG$Gy^2 + AG$Gz^2) - 1
  ENMO[which(ENMO < 0)] <- 0
  ENMO2 <- cumsum(ENMO)
  ENMO3 <-
    diff(ENMO2[seq(1, length(ENMO), by = (meta$samp_freq * output_window))]) /
    (meta$samp_freq * output_window)

  final_length <- min(c(length(ENMO3), nrow(data)))
  AG <- data.frame(AG$AG[1:final_length, ])
  ENMO3 <- ENMO3[1:final_length]
  AG$ENMO <- ENMO3 * 1000
  ## /end adapted van Hees code
  
  if(any(grepl('block', names(AG), ignore.case = TRUE))) {
    AG[ , grepl('block', names(AG), ignore.case = TRUE)] <-
      NULL
  }

  AG$file_source_PrimaryAccel <- basename(file)
  AG$date_processed_PrimaryAccel <- Sys.time()

  AG$day_of_year <-
    get_day_of_year(AG$Timestamp, format = "%Y-%m-%d %H:%M:%S")
  AG$minute_of_day <-
    get_minute(AG$Timestamp, format = "%Y-%m-%d %H:%M:%S")

  order <-
    c("file_source_PrimaryAccel",
      "date_processed_PrimaryAccel",
      "Timestamp",
      "day_of_year",
      "minute_of_day",
      "ENMO")
  AG <- AG[, c(order, setdiff(names(AG), order))]

  duration <-
    unname((proc.time() - timer)[3])

  if (verbose) message_update(4, duration = duration)

  return(AG)
}

#' File reading function for IMU files
#'
#' @param file character scalar giving the path to the IMU file
#' @param output_window_secs the desired epoch length; defaults to one second
#'
#' @return A dataframe giving processed IMU data in the specified epoch length
#' @export
read_IMU <- function(file, output_window_secs = 1, verbose = FALSE) {
  timer <- proc.time()
  if (verbose) message_update(1, file = file)

  meta <- get_imu_file_meta(file, output_window_secs)

  AG <-
    suppressWarnings(try(data.table::fread(
      file,
      stringsAsFactors = FALSE,
      skip = 10,
      #nrows = 25,
      showProgress = FALSE
    ))
    )
  if (sum(unlist(sapply(AG, function(x) sum(grepl("error", x, ignore.case = T))))) > 0) {
    message_update(18, is_message = TRUE)
    return(NULL)
  }
  AG <- data.frame(AG)

  AG$file_source_IMU <- basename(file)
  AG$date_processed_IMU <- Sys.time()
  AG$Timestamp <- meta$start_time + (0:(nrow(AG) - 1) / meta$samp_rate)

  AG <- check_second(AG)
  AG <- imu_filter_gyroscope(AG, meta$samp_rate, verbose = verbose)

  # Calculate vector magnitudes
  if (verbose) message_update(7)

  AG$mean_Accel_VM <-
    getVM(AG[, grepl("accelerometer", names(AG), ignore.case = T)], verbose = verbose)

  AG$Gyroscope_VM_DegPerS <-
    getVM(AG[, grepl("gyroscope", names(AG), ignore.case = T)], verbose = verbose)

  AG$Magnetometer_VM_MicroT <-
    getVM(AG[, grepl("magnetometer", names(AG), ignore.case = T)], verbose = verbose)

  if (verbose) message_update(8)

  AG <- imu_collapse(AG, meta$block_size, verbose = verbose)

  first_variables <- c("file_source_IMU", "date_processed_IMU", "Timestamp")

  AG <- AG[, c(first_variables, setdiff(names(AG), first_variables))]

  AG$epoch <- NULL

  duration <- unname((proc.time() - timer)[3])
  if (verbose) message_update(4, duration = duration)
  return(AG)
}
