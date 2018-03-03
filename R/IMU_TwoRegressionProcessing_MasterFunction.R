#' Process GT9X Files with Hibbing Two-Regression Algorithms
#'
#' Process GT9X primary accelerometer and (if applicable) IMU files using one or more of the algorithms from \href{https://www.ncbi.nlm.nih.gov/pubmed/29271847}{Hibbing et al. (2018, \emph{Med Sci Sports Exerc})}.
#'
#' @param RAW A character scalar giving path to primary accelerometer data file
#' @param IMU A character scalar giving path to IMU data file
#' @param Wear_Location A character scalar indicating the device's attachment site
#' @param PID A character scalar giving the participant identification
#' @param Algorithm A numeric vector giving the algorithm(s) to apply to the data from the primary accelerometer and (if applicable) IMU
#' @param verbose A logical scalar: print progress updates?
#' @param IMU_ignore_A1 A logical scalar. If Algorithm = 1, should IMU files be ignored?
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
    Algorithm = 1, verbose = FALSE, IMU_ignore_A1 = TRUE) {

    t <- proc.time()

    Algorithm <- algorithm_verify(IMU, Algorithm)
    IMU <- imu_verify(IMU, Algorithm, IMU_ignore_A1)
    Wear_Location <- attachment_verify(Wear_Location)

    ## Read the data, then merge if necessary
    raw_data <- read_AG_raw(RAW, verbose = verbose)
    raw_data$Timestamp <- as.character(raw_data$Timestamp)

    if (!is.null(IMU)) {
      imu_data <- read_IMU(IMU, verbose = verbose)
      imu_data$Timestamp <- as.character(imu_data$Timestamp)
      all_data <- merge(raw_data, imu_data, "Timestamp")
    } else {
      all_data <- raw_data
    }

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
    firstVars <- firstVars[firstVars %in% names(all_data)]
    all_data  <- all_data[, c(firstVars, setdiff(names(all_data), firstVars))]

    ## Retrieve CV/10-s variable(s), then calculate CV, then add to all_data
    cv_vars <- get_cv_vars(Algorithm, verbose)
    CVS <-
      sapply(cv_vars, function(x)
        get_cvPER(all_data[, x], Algorithm = Algorithm, verbose = verbose))

    cv_names <-
      sapply(cv_vars, function(x)
        switch(
          x,
          ENMO = "ENMO_CV10s",
          Gyroscope_VM_DegPerS = "GVM_CV10s",
          mean_abs_Gyroscope_y_DegPerS = "GYA_CV10s"
        ))

    all_data <-
      cbind(all_data,
        stats::setNames(data.frame(CVS), cv_names))

    ## Calculate Direction Changes per 5s and add it to the data set
    if (!is.null(IMU)) {
      all_data$Direction <-
        get_directions(all_data$mean_magnetometer_direction)
    }

    ## Get the predictions
    all_processes <-
      expand.grid(
        Wear_Location = Wear_Location,
        Algorithm = Algorithm,
        stringsAsFactors = F
      )

    all_predictions <-
      data.frame(
        do.call(cbind,
                lapply(split(all_processes, seq(nrow(all_processes))),
                       apply_two_regression_hibbing18,
                       all_data = all_data
                      )
                    ),
        stringsAsFactors = F
      )

    names(all_predictions) <-
        unlist(lapply(split(all_processes, seq(nrow(all_processes))),
                function(x) {
                    paste(
                      gsub(" ", "_", x$Wear_Location),
                      paste("Algorithm", x$Algorithm, sep = ""),
                      c("Classification", "METs"),
                      sep = "_")
              })
          )

    ## Final formatting and output of the data
    all_data <- cbind(all_data, all_predictions)

    if (verbose) message_update(14)
    if (verbose) message_update(15)

    duration <-
      unname((proc.time() - t)[3])

    if (verbose) message_update(16, duration = duration)
    return(all_data)
}
