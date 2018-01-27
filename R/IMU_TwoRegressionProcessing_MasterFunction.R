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
    Algorithm = 1) {

    ## Read the data
    raw_data <- read_AG_raw(RAW)
    imu_data <- read_IMU(IMU)

    raw_data$Timestamp <- as.character(raw_data$Timestamp)
    imu_data$Timestamp <- as.character(imu_data$Timestamp)

    ## Merge the data
    alldata <- merge(raw_data, imu_data, "Timestamp")

    names(alldata) <-
      gsub("\\.y$", "_IMU", gsub("\\.x$", "_PrimaryAccel", names(alldata)))

    ## Add the ID and order the variables
    alldata$PID <- PID
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
    alldata <- alldata[, c(firstVars, setdiff(names(alldata), firstVars))]

    ## Calculate CV per 10s and add it to the data set
    cvs <- unique(sapply(1:3, function(x) {
        switch(x, "ENMO", "Gyroscope_VM_DegPerS", "Gyroscope_VM_DegPerS")
    })[1:3 %in% Algorithm])

    print(paste("Calculating CV per 10s for:", paste(cvs, collapse = " and ")))
    print("This could take awhile. Be patient...")

    CVS <- sapply(cvs, function(x) get.cvPER(alldata[, x]))

    cvs <- sapply(cvs, function(x) switch(x, ENMO = "ENMO_CV10s", Gyroscope_VM_DegPerS = "GVM_CV10s", mean_abs_Gyroscope_y_DegPerS = "GYA_CV10s"))
    alldata <- cbind(alldata, setNames(data.frame(CVS), cvs))

    ## Calculate Direction Changes per 5s and add it to the data set
    alldata$Direction <- get.directions(alldata$mean_MagnetometerDirection)

    ## Get the predictions
    allProcesses <- expand.grid(Wear_Location = Wear_Location, Algorithm = Algorithm, stringsAsFactors = F)
    allPredictions <- setNames(data.frame(do.call(cbind, lapply(split(allProcesses, seq_len(nrow(allProcesses))),
        apply.TwoRegression, alldata = alldata)), stringsAsFactors = F), unlist(lapply(split(allProcesses,
        seq_len(nrow(allProcesses))), function(x) {
        paste(x$Wear_Location, gsub("^", "Agorithm", x$Algorithm), "METs", sep = "_")
    })))

    ## Final formatting and output of the data
    alldata <- cbind(alldata, allPredictions)

    cat("\n\n")
    print("All two-regression processing complete.")
    return(alldata)
}
