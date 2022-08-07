## All of this is old code that may be useful at some point

#' #' Smooth predicted values from a two-regression algorithm
#' #'
#' #' Collapses data over a specified epoch length
#' #'
#' #' @param AG A data frame of monitor data that has been processed with a two-regression algorithm
#' #' @param timestamps A vector of POSIX-formatted timestamps corresponding to the two-regression data
#' #' @param epoch The desired epoch length over which to collapse
#' #' @param verbose A logical scalar: print progress updates?
#' #' @param class_summary A logical scalar: Include summary of two-regression classification in the output?
#' #'
#' #' @return A data frame of collapsed two-regression data
#' #' @export
#' #'
#' #' @examples
#' #' \donttest{
#' #' raw_file <- system.file(
#' #'   "extdata",
#' #'   "TestID_LeftWrist_RAW.csv",
#' #'   package = "TwoRegression"
#' #' )
#' #'
#' #' imu_file <- system.file(
#' #'   "extdata",
#' #'   "TestID_LeftWrist_IMU.csv",
#' #'   package = "TwoRegression"
#' #' )
#' #'
#' #' wear <- "Left Wrist"
#' #' id <- "Test"
#' #' alg <- 1:2
#' #' full_data <-
#' #'     hibbing18_twoReg_process(raw_file, imu_file, wear, id, alg)
#' #'
#' #' AG_smooth(full_data, as.POSIXlt(full_data$Timestamp))
#' #' }
#' AG_smooth <- function(
#'     AG, timestamps, epoch = 60, class_summary = FALSE, verbose
#' ) {
#'
#'   if (!"minute_of_day" %in% names(AG)) {
#'     message_update(28, is_message = TRUE)
#'     return(AG)
#'   }
#'
#'   start_epoch <- as.numeric(diff(timestamps[1:2]))
#'   rows_per_block <- epoch / start_epoch
#'
#'   minutes <- strftime(timestamps, "%Y-%m-%d %H:%M", "UTC")
#'   excess  <- tapply(minutes, minutes, length)
#'   excess  <- excess[excess != rows_per_block]
#'
#'   AG <- AG[!minutes %in% names(excess), ]
#'   stopifnot(nrow(AG) %% epoch == 0)
#'
#'   if (rows_per_block %% 1 != 0) {
#'
#'     message_update(
#'       29, start_epoch = start_epoch, epoch = epoch, is_message = TRUE
#'     )
#'     return(AG)
#'
#'   }
#'
#'   minutes <- setdiff(minutes, names(excess))
#'
#'   indices <-
#'     seq(rows_per_block) %>%
#'     {rep(., length(unique(minutes)))}
#'
#'   block_number <-
#'     cumsum(indices == 1) %T>%
#'     {stopifnot(sd(table(.)) == 0)}
#'
#'   AG$Block <- block_number
#'
#'   measure_names <- names(AG)[
#'     grepl("Classification$", names(AG)) | grepl("METs$", names(AG))
#'   ]
#'
#'   descriptive_names <- c(
#'     "PID", "file_source_PrimaryAccel", "date_processed_PrimaryAccel",
#'     "file_source_IMU", "date_processed_IMU", "Timestamp", "day_of_year",
#'     "minute_of_day", "ENMO", "Gyroscope_VM_DegPerS",
#'     "mean_abs_Gyroscope_x_DegPerS", "mean_abs_Gyroscope_y_DegPerS",
#'     "mean_abs_Gyroscope_z_DegPerS", "mean_magnetometer_direction",
#'     "ENMO_CV10s", "GVM_CV10s", "Direction", "Block"
#'   )
#'
#'   ## Collapse descriptive part of data set
#'
#'   descriptive_set <- AG[ ,setdiff(names(AG), measure_names)]
#'
#'   if (!all(names(descriptive_set) %in% descriptive_names)) {
#'
#'     missing_vars <- names(descriptive_set)[
#'       !names(descriptive_set) %in% descriptive_names
#'     ]
#'
#'     message_update(30, missing_vars = missing_vars, is_message = TRUE)
#'
#'     return(AG)
#'
#'   }
#'
#'   descriptive_data <-
#'     names(descriptive_set) %>%
#'     lapply(smooth_column, AG = descriptive_set) %>%
#'     c(stringsAsFactors = FALSE) %>%
#'     do.call(data.frame, .)
#'
#'   names(descriptive_data) %<>%
#'     strsplit("\\.") %>%
#'     lapply(function(x) x[length(x)]) %>%
#'     unlist(.)
#'
#'   ## Collapse MET variables
#'
#'   met_vars <- measure_names[grepl("METs$", measure_names)]
#'
#'   classification_vars <- measure_names[
#'     grepl("Classification$", measure_names)
#'   ]
#'
#'   stopifnot(setequal(
#'     measure_names, c(met_vars, classification_vars)
#'   ))
#'
#'   METs <-
#'     met_vars %>%
#'     sapply(function(x) {
#'       stats::setNames(
#'         data.frame(tapply(AG[ ,x], AG$Block, mean, na.rm = TRUE)),
#'         paste("mean", x, sep = "_")
#'       )
#'     }, USE.NAMES = FALSE) %>%
#'     do.call(data.frame, .)
#'
#'   ## Collapse classifications (if applicable) and finish up
#'
#'   if (class_summary) {
#'
#'     classification <-
#'       classification_vars %>%
#'       sapply(function(x) {
#'         tapply(
#'           AG[ ,x],
#'           AG$Block,
#'           function(y) {
#'             data.frame(
#'               SED_prop = sum(y == "SED") / length(y),
#'               CWR_prop = sum(y == "CWR") / length(y),
#'               ILA_prop = sum(y == "ILA") / length(y)
#'             )
#'           }
#'         ) %>%
#'           do.call(rbind, .)
#'       }, simplify = FALSE) %>%
#'       do.call(data.frame, .)
#'
#'     names(classification) %<>% gsub("Classification\\.", "", .)
#'
#'     AG <- data.frame(descriptive_data, classification, METs)
#'
#'   } else {
#'
#'     AG <- data.frame(descriptive_data, METs)
#'
#'   }
#'
#'   AG$Block <- NULL
#'
#'   AG
#'
#' }
#'
#' #' Rules to collapse columns of data passed to \code{\link{AG_smooth}}
#' #'
#' #' @param col_name The name of a single column in \code{AG} to be collapsed
#' #' @inheritParams AG_smooth
#' #'
#' #' @return A vector of data collapsed according to the specified rule for \code{col_name}
#' #' @keywords internal
#' #'
#' smooth_column <- function(col_name, AG) {
#'
#'   switch(
#'     col_name,
#'     "PID" = AG$PID[1],
#'     "file_source_PrimaryAccel" = AG$file_source_PrimaryAccel[1],
#'     "date_processed_PrimaryAccel" =
#'       as.character(AG$date_processed_PrimaryAccel)[1],
#'     "file_source_IMU" = AG$file_source_IMU[1],
#'     "date_processed_IMU" =
#'       as.character(AG$date_processed_IMU[1]),
#'     "Timestamp" =
#'       data.frame(Timestamp =
#'                    tapply(AG$Timestamp, AG$Block, function(x) x[1]),
#'                  stringsAsFactors = FALSE),
#'     "day_of_year" = AG$day_of_year[1],
#'     "minute_of_day" = AG$minute_of_day[1],
#'
#'     "ENMO" =
#'       data.frame(mean_ENMO =
#'                    tapply(AG$ENMO, AG$Block, mean, na.rm = TRUE)),
#'     "Gyroscope_VM_DegPerS" =
#'       data.frame(mean_Gyroscope_VM_DegPerS =
#'                    tapply(AG$Gyroscope_VM_DegPerS, AG$Block, mean, na.rm = TRUE)),
#'     "mean_abs_Gyroscope_x_DegPerS" =
#'       data.frame(mean_abs_Gyroscope_x_DegPerS =
#'                    tapply(AG$mean_abs_Gyroscope_x_DegPerS, AG$Block, mean, na.rm = TRUE)),
#'     "mean_abs_Gyroscope_y_DegPerS" =
#'       data.frame(mean_abs_Gyroscope_y_DegPerS =
#'                    tapply(AG$mean_abs_Gyroscope_y_DegPerS, AG$Block, mean, na.rm = TRUE)),
#'     "mean_abs_Gyroscope_z_DegPerS" =
#'       data.frame(mean_abs_Gyroscope_z_DegPerS =
#'                    tapply(AG$mean_abs_Gyroscope_z_DegPerS, AG$Block, mean, na.rm = TRUE)),
#'
#'     "mean_magnetometer_direction" = NA,
#'     "ENMO_CV10s" =
#'       data.frame(mean_ENMO_CV10s =
#'                    tapply(AG$ENMO_CV10s, AG$Block, mean, na.rm = TRUE)),
#'     "GVM_CV10s" =
#'       data.frame(mean_GVM_CV10s =
#'                    tapply(AG$GVM_CV10s, AG$Block, mean, na.rm = TRUE)),
#'     "Direction" =
#'       data.frame(mean_direction_changes =
#'                    tapply(AG$Direction, AG$Block, mean, na.rm = TRUE)),
#'     "Left_Wrist_Algorithm1_Classification" = NA,
#'     "Left_Wrist_Algorithm1_METs" = NA,
#'     "Left_Wrist_Algorithm2_Classification" = NA,
#'     "Left_Wrist_Algorithm2_METs" = NA,
#'     "Block" =
#'       data.frame(Block = tapply(AG$Block, AG$Block, mean, na.rm = TRUE))
#'   )
#'
#' }
