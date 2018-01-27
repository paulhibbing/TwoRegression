#' Process GT9X Files with Hibbing Two-Regression Algorithms
#'
#' @param RAW A character scalar giving path to primary accelerometer data file
#' @param IMU A character scalar giving path to IMU data file
#' @param Wear_Location A character scalar indicating the device's attachment site
#' @param PID A character scalar giving the participant identification
#' @param Algorithm A numeric vector giving the algorithm(s) to apply to the data from the primary accelerometer and (if applicable) IMU
#'
#' @return A data frame giving the data and predictions
#' @export
#'
hibbing18_twoReg_process <-
  function(RAW,
    IMU = NULL,
    Wear_Location = c("Hip", "Left Wrist", "Right Wrist", "Left Ankle",
      "Right Ankle"),
    PID,
    Algorithm = 1, verbose = FALSE) {

    ## Read the data
    raw_data <- read_AG_raw(RAW)
    imu_data <- read_IMU(IMU)

    raw_data$Timestamp <- as.character(raw_data$Timestamp)
    imu_data$Timestamp <- as.character(imu_data$Timestamp)

    ## Merge the data
    all_data <- merge(raw_data, imu_data, "Timestamp")

    names(all_data) <-
      gsub("\\.y$", "_IMU", gsub("\\.x$", "_PrimaryAccel", names(all_data)))

    ## Add the ID and order the variables
    all_data$PID <- PID
    firstVars <-
      c(
        "PID",
        "file_source_PrimaryAccel",
        "date_processed_PrimaryAccel",
        "file_source_IMU",
        "date_processed_IMU",
        "Timestamp",
        "day_of_year",
        "minute_of_day"
      )
    all_data <- all_data[, c(firstVars, setdiff(names(all_data), firstVars))]

    ## Retrieve CV/10-s variable(s), then calculate CV, then add to all_data
    cv_vars <- get_cv_vars(Algorithm, verbose)
    CVS <- sapply(cv_vars, function(x) get_cvPER(all_data[, x]))

    cvs <-
      sapply(cv_vars, function(x)
        switch(
          x,
          ENMO = "ENMO_CV10s",
          Gyroscope_VM_DegPerS = "GVM_CV10s",
          mean_abs_Gyroscope_y_DegPerS = "GYA_CV10s"
        ))
    all_data <-
      cbind(all_data,
        setNames(data.frame(CVS), cvs))

    ## Calculate Direction Changes per 5s and add it to the data set
    all_data$Direction <-
      get.directions(all_data$mean_magnetometer_direction)

    ## Get the predictions
    allProcesses <- expand.grid(Wear_Location = Wear_Location, Algorithm = Algorithm, stringsAsFactors = F)
    allPredictions <- setNames(data.frame(do.call(cbind, lapply(split(allProcesses, seq_len(nrow(allProcesses))),
        apply.TwoRegression, all_data = all_data)), stringsAsFactors = F), unlist(lapply(split(allProcesses,
        seq_len(nrow(allProcesses))), function(x) {
        paste(x$Wear_Location, gsub("^", "Agorithm", x$Algorithm), "METs", sep = "_")
    })))

    ## Final formatting and output of the data
    all_data <- cbind(all_data, allPredictions)

    cat("\n\n")
    print("All two-regression processing complete.")
    return(all_data)
}
