#' Check if the AG data start on an exact second
#'
#' @param AG a dataframe of IMU data
#'
#' @keywords internal
check_second <- function(AG) {
  AG$ms <-
    as.numeric(format(AG$Timestamp, "%OS3"))%%1
  if(AG$ms[1]!=0) {
    AG <- AG[-c(1:(which(AG$ms == 0)[1] - 1)),]
  }
  return(AG)
}

#' Get file metadata (sampling frequency and timestamps) for primary accelerometer
#'
#' @param file character scalar giving path to primary accelerometer file
#'
#' @keywords internal
get_raw_file_meta <- function(file) {
  file_meta <-
    data.frame(data.table::fread(
      file,
      nrow = 10,
      header = F,
      sep = "\n"
    ))
  samp_freq <-
    file_meta[sapply(file_meta, function(x) {
      grepl("Hz", x, ignore.case = T)
    }),]
  samp_freq <-
    as.numeric(unlist(strsplit(samp_freq, " "))[which(grepl("Hz", unlist(strsplit(samp_freq, " ")), ignore.case = T)) - 1])

  start_time <-
    gsub("[[:alpha:] ,]", "", file_meta[sapply(file_meta, function(x) {
      grepl("start[. ]time", x,
        ignore.case = T)
    }), ]
    )
  start_date <-
    gsub("[[:alpha:] ,]", "", file_meta[sapply(file_meta, function(x) {
      grepl("start[. ]date", x,
        ignore.case = T)
    }), ])
  start <-
    as.POSIXlt(paste(start_date, start_time), format = "%m/%d/%Y %H:%M:%S")
  if(is.na(start)) message_update(3, is_message = TRUE)
  return(list(start = start, samp_freq = samp_freq))
}

#' Get file metadata (sampling frequency, start time, and samples per epoch) for primary accelerometer
#'
#' @param file character scalar giving path to IMU file
#' @param output_window_secs the desired epoch length, over which to average IMU data
#'
#' @keywords internal
get_imu_file_meta <- function(file, output_window_secs) {
  header <- read.csv(file, nrow = 20, stringsAsFactors = F, header = F)

  samp_rate <- unlist(strsplit(header[, 1], " "))
  samp_rate <-
    suppressWarnings(try(as.numeric(samp_rate[which(samp_rate == "Hz") - 1])))
  if (grepl("error", samp_rate, ignore.case = T)) {
    message_update(21, is_message = TRUE)
    samp_rate = 100
  }

  date_index <-
    which(grepl("start date", header[, 1], ignore.case = T))
  time_index <-
    which(grepl("start time", header[, 1], ignore.case = T))

  start_time <-
    as.POSIXlt(gsub("[[:alpha:] ]", "",
      paste(header[date_index, 1],
        header[time_index, 1])),
      format = "%m/%d/%Y%H:%M:%S")

  block_size <- samp_rate * output_window_secs
  return(list(
    start_time = start_time,
    block_size = block_size,
    samp_rate = samp_rate
  ))
}

#' Collapse primary accelerometer data
#'
#' @param AG a dataframe of raw primary accelerometer data
#'
#' @keywords internal
AG_collapse <- function(AG, output_window, samp_freq) {
  blocksize <- samp_freq * output_window
  Block <-
    rep(seq(floor(nrow(AG) / blocksize)),
      each = blocksize)

  AG$Block <- Block
  Gx = AG$`Accelerometer X`
  Gy = AG$`Accelerometer Y`
  Gz = AG$`Accelerometer Z`  ## For ENMO in next section
  AG <- AG %>% dplyr::group_by(Block) %>% dplyr::summarise(Timestamp = first(Timestamp))
  return(list(AG = AG, Gx = Gx, Gy = Gy, Gz = Gz))
}

#' Collapse raw IMU data to a specified epoch
#'
#' @param AG dataframe containing raw IMU data
#' @param block_size number of samples per epoch
#'
#' @return dataframe of IMU data averaged over the specified epoch length
#' @keywords internal
imu_collapse <- function(AG, block_size, verbose = FALSE) {

  if (nrow(AG) %% block_size != 0) {
    message_update(9, is_message = TRUE)
    final_obs <-
      rev(seq_len(nrow(AG)))[which(rev(seq_len(nrow(AG)))[1:block_size] %%
          (block_size) ==
          0)[1]]
    AG <- AG[1:final_obs, ]
  }

  AG$epoch <- rep(1:(nrow(AG) / block_size), each = block_size)

  if (verbose) message_update(10)
  AG <-
    AG %>% dplyr::group_by(epoch) %>%
    dplyr::summarise(
      date_processed_IMU = first(date_processed_IMU),
      file_source_IMU = first(file_source_IMU),
      Timestamp = first(Timestamp),
      #Time = first(Time),
      Gyroscope_VM_DegPerS = mean(Gyroscope_VM_DegPerS),
      mean_abs_Gyroscope_x_DegPerS = mean(abs(`Gyroscope.X`)),
      mean_abs_Gyroscope_y_DegPerS = mean(abs(`Gyroscope.Y`)),
      mean_abs_Gyroscope_z_DegPerS = mean(abs(`Gyroscope.Z`)),
      mean_magnetometer_direction = classify_magnetometer(
        mean(`Magnetometer.X`),
        mean(`Magnetometer.Y`),
        mean(`Magnetometer.Z`)
      )
    )

  if (verbose) message_update(6)
  AG <- as.data.frame(AG)
}
