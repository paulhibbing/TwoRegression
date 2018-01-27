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

#' Calculate vector magnitude
#'
#' @param triaxial a dataframe of triaxial data on which to calculate vector magnitude
#' @param verbose print information about variable search criteria?
#'
#' @return a vector of vector magnitude values
getVM <- function(triaxial, verbose = T) {
    if (verbose) {
      vm_variables <-
        gsub("\"", "", substring(deparse(substitute(triaxial)), unlist(gregexpr(
          "\"", deparse(substitute(triaxial))
        ))[1],
          unlist(gregexpr(
            "\"", deparse(substitute(triaxial))
          ))[2]))

        message_update(2, vm_variables = vm_variables)
    }
    triaxial <- triaxial[, !grepl("VM", names(triaxial))]
    stopifnot(ncol(triaxial) == 3)
    apply(triaxial, 1, function(x) sqrt(sum(x^2)))
}

#' Calculate coefficient of variation
#'
#' @param signal the variable on which to perform calculation
#'
#' @keywords internal
#'
cv <- function(signal) {
  if (mean(signal) == 0) {
    0
  } else {
    sd(signal)/mean(signal) * 100
  }
}

#' Convert magnetometer signal to cardinal direction
#'
#' @param x x-axis magnetometer data
#' @param y y-axis magnetometer data
#' @param z z-axis magnetometer data
#' @param orientation the conversion scheme to use, from c("vertical", "horizontal")
#'
#' @keywords internal
classify_magnetometer <- function(x = "Magnetometer X", y = "Magnetometer Y", z = "Magnetometer Z", orientation = "vertical") {

    if (length(x) != length(y)) {
        message("Length of X and Y differ. Returning NULL.")
        return(NULL)
    }
    n <- length(x)
    if (length(x) > 1 | length(y) > 1) {
        x <- mean(x)
        y <- mean(y)
        message(paste("Determining direction from mean values of x and y, replicating", n, "times."))
    }

    ## Calculate direction for vertical orientation
    zdir <- as.character(cut(z, c(-Inf, -22, -16.71, -11.43, -6.14, -0.86, 4.43, 9.71, 15, Inf), c("N", "NNx",
        "Nx", "xNx", "x", "xSx", "Sx", "SSx", "S"), right = F))


    dir <- if (grepl("x", zdir, ignore.case = T)) {
        gsub("x", if (x > 22)
            "E" else "W", zdir)
    } else zdir

    ## Calculate direction for non-vertical orientation
    if (orientation != "vertical") {
        xdir <- as.character(cut(x, c(-Inf, -6, -0.29, 5.43, 11.14, 16.86, 22.57, 28.29, 34, Inf), c("N", "NNy",
            "Ny", "yNy", "y", "ySy", "Sy", "SSy", "S"), right = F))


        dir <- if (grepl("y", xdir, ignore.case = T)) {
            gsub("y", if (y > 4)
                "E" else "W", xdir)
        } else xdir
    }

    return(rep(dir, n))
}

#' File reading function for primary accelerometer files
#'
#' @param file A character scalar giving path to primary accelerometer file
#' @param output_window the desired epoch length; defaults to one second
#'
#' @return A dataframe giving processed raw data from the primary accelerometer in the specified epoch length
#' @export
read.AG.raw <- function(file, output_window = 1) {
    timer <- proc.time()

    message_update(1, file = file)

    meta <- get_raw_file_meta(file)

    AG <-
      data.table::fread(file, stringsAsFactors = F, showProgress = F, skip = 10)
    AG$Timestamp <-
      meta$start + ((seq(nrow(AG)) - 1) / meta$samp_freq)

    AG <- AG_collapse(AG, output_window, meta$samp_freq)

    ## Get ENMO
    ENMO <- sqrt(AG$Gx^2 + AG$Gy^2 + AG$Gz^2) - 1
    ENMO[which(ENMO < 0)] <- 0
    ENMO2 <- cumsum(ENMO)
    ENMO3 <-
      diff(ENMO2[seq(1, length(ENMO), by = (meta$samp_freq * output_window))]) /
      (meta$samp_freq * output_window)

    final_length = min(c(length(ENMO3), nrow(data)))
    AG <- data.frame(AG$AG[1:final_length, ])
    ENMO3 <- ENMO3[1:final_length]
    AG$ENMO <- ENMO3 * 1000

    if(any(grepl('block', names(AG), ignore.case = TRUE))) {
      AG[ , grepl('block', names(AG), ignore.case = TRUE)] <-
        NULL
    }

    AG$file_source <- basename(file)
    AG$date_processed <- Sys.time()

    AG$day_of_year <-
      get_day_of_year(AG$Timestamp, format = "%Y-%m-%d %H:%M:%S")
    AG$minute_of_day <-
      get_minute(AG$Timestamp, format = "%Y-%m-%d %H:%M:%S")

    order <-
      c("file_source",
        "date_processed",
        "Timestamp",
        "day_of_year",
        "minute_of_day",
        "ENMO")
    AG <- AG[, c(order, setdiff(names(AG), order))]

    duration <-
      unname((proc.time() - timer)[3])

    message_update(4, duration = duration)

    return(AG)
}

get_imu_file_meta <- function(file, output_window_secs) {
  header <- read.csv(file, nrow = 20, stringsAsFactors = F, header = F)

  samp_rate <- unlist(strsplit(header[, 1], " "))
  samp_rate <-
    suppressWarnings(try(as.numeric(samp_rate[which(samp_rate == "Hz") - 1])))
  if (grepl("error", samp_rate, ignore.case = T)) {
    message("Unable to detect sampling rate. Defaulting to 100")
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

#' Low-Pass filter the Gyroscope data at 35 Hz
#'
#' @inheritParams check_second
#'
#' @keywords internal
imu_filter_gyroscope <- function(AG, samp_rate) {
  message_update(5)
  AG[, grepl("gyroscope", names(AG), ignore.case = T)] <-
    sapply(AG[, grepl("gyroscope", names(AG),
      ignore.case = T)], function(x) {
        seewave::bwfilter(
          wave = x,
          f = samp_rate,
          n = 2,
          to = 35
        )
      })
  message_update(6)
  return(AG)
}

#' Collapse raw IMU data to a specified epoch
#'
#' @param AG
#'
#' @return
#' @export
#'
#' @examples
imu_collapse <- function(AG, block_size) {

  if (nrow(AG) %% block_size != 0) {
    message_update(9, is_message = TRUE)
    final_obs <-
      rev(seq_len(nrow(AG)))[which(rev(seq_len(nrow(AG)))[1:block_size] %%
          (block_size) ==
          0)[1]]
    AG <- AG[1:final_obs, ]
  }

  AG$epoch <- rep(1:(nrow(AG) / block_size), each = block_size)

  message_update(10)
  AG <-
    AG %>% dplyr::group_by(epoch) %>%
    dplyr::summarise(
      date_processed = first(date_processed),
      file_source = first(file_source),
      Timestamp = first(Timestamp),
      #Time = first(Time),
      Gyroscope_VM_DegPerS = mean(Gyroscope_VM_DegPerS),
      mean_abs_Gyroscope_x_DegPerS = mean(abs(`Gyroscope.X`)),
      mean_abs_Gyroscope_y_DegPerS = mean(abs(`Gyroscope.Y`)),
      mean_abs_Gyroscope_z_DegPerS = mean(abs(`Gyroscope.Z`)),
      mean_MagnetometerDirection = classify_magnetometer(
        mean(`Magnetometer.X`),
        mean(`Magnetometer.Y`),
        mean(`Magnetometer.Z`)
      )
    )

  message_update(6)
  AG <- as.data.frame(AG)
}

#' File reading function for IMU files
#'
#' @param file character scalar giving the path to the IMU file
#' @param output_window_secs the desired epoch length; defaults to one second
#'
#' @return A dataframe giving processed IMU data in the specified epoch length
#' @export
read.IMU <- function(file, output_window_secs = 1) {
    timer <- proc.time()
    message_update(1, file = file)

    meta <- get_imu_file_meta(file, output_window_secs)

    AG <-
      suppressWarnings(try(data.table::fread(
        file,
        stringsAsFactors = F,
        skip = 10,
        #nrows = 25,
        showProgress = F
      ))
      )
    if (sum(unlist(sapply(AG, function(x) sum(grepl("error", x, ignore.case = T))))) > 0) {
        message("Error in file formatting. Returning NULL.")
        return(NULL)
    }
    AG <- data.frame(AG)

    AG$file_source <- basename(file)
    AG$date_processed <- Sys.time()
    AG$Timestamp <- meta$start_time + (0:(nrow(AG) - 1) / meta$samp_rate)

    AG <- check_second(AG)
    AG <- imu_filter_gyroscope(AG, meta$samp_rate)

    # Calculate vector magnitudes
    message_update(7)

    AG$mean_Accel_VM <-
      getVM(AG[, grepl("accelerometer", names(AG), ignore.case = T)])

    AG$Gyroscope_VM_DegPerS <-
      getVM(AG[, grepl("gyroscope", names(AG), ignore.case = T)])

    AG$Magnetometer_VM_MicroT <-
      getVM(AG[, grepl("magnetometer", names(AG), ignore.case = T)])

    message_update(8)

    AG <- imu_collapse(AG, meta$block_size)

    first_variables <- c("file_source", "date_processed", "Timestamp")

    AG <- AG[, c(first_variables, setdiff(names(AG), first_variables))]

    AG$epoch <- NULL

    duration <- unname((proc.time() - timer)[3])
    message_update(4, duration = duration)
    return(AG)
}

## Functions to calculate CV per 10s and Direction changes per 5s
get.cvPER <- function(BigData, windowSecs = 10, verbose = F) {
    if (verbose)
        cat(paste("\n... Getting ", windowSecs, "s CVs", sep = ""))

    inds <- sapply(seq_along(BigData), function(x) sapply(windowSecs:1, function(y) ((x - y):(x - y + windowSecs -
        1)) + 1), simplify = F)

    CVS <- do.call(rbind, lapply(inds, function(x) {
        values <- sapply(data.frame(x), function(y) {
            Y <- y[y > 0 & y <= length(BigData)]
            if (length(y) != length(Y)) {
                data.frame(CV = NA)
            } else {
                data.frame(CV = cv(BigData[Y]))
            }
        }, simplify = F)

        CV <- sapply(do.call(rbind, values), min, na.rm = T)
        return(CV)
    }))

    if (verbose)
        cat("\n... Done!\n")
    return(CVS)
}

get.directions <- function(BigData, windowSecs = 5) {
    if (windowSecs%%2 != 1)
        stop("windowSecs must be an odd number, to look forward and backward of the observation by equal amounts.")
    windowSecs <- (windowSecs - 1)/2

    inds <- sapply(seq_along(BigData), function(x) {
        if ((x - windowSecs > 0) & (x + windowSecs <= length(BigData))) {
            seq(x - windowSecs, x + windowSecs)
        } else NA
    }, simplify = F)  ##Generate indices for each eligible block

    changes <- sapply(inds, function(x) if (is.na(x[1]))
        NA else {
        compareDirs <- diff(as.numeric(as.factor(BigData[x])))  ##Checks for successive
        ## Differences by subtracting factor levels
        dirChange <- sum(ifelse(compareDirs == 0, 0, 1))  ##Totals the direction changes, after setting all nonzero differences to 1
    })

    return(changes)
}

## Function to apply a two-regression algorithm
apply.TwoRegression <- function(which.algorithm = data.frame(Wear.Location = "Hip", Algorithm = 1), alldata) {
    if (!exists("Algorithms", envir = globalenv())) {
        stop("Can't find the data from Algorithms.RData. Use load(file.choose()) to navigate to the file and load it.")
    }

    Site <- sapply(which.algorithm$Wear.Location, function(x) switch(x, Hip = "Hip", `Left Wrist` = "LW", `Right Wrist` = "RW",
        `Left Ankle` = "LA", `Right Ankle` = "RA"))

    matched_Algorithm <- Algorithms[[Site]]
    if (length(matched_Algorithm) == 0)
        stop("Didn't find a matching algorithm. This could take some work to figure out...")
    if (length(matched_Algorithm) != 7)
        stop("Found too many matching algorithms. This could take some work to figure out... Make sure there's only one wear
                                          location/algorithm passed to the function.")

    which.sed.cutpoint <- switch(which.algorithm$Algorithm, "accelSedCut", "VM_gyroSedCut", "VM_gyroSedCut")
    which.cwr.cutpoint <- switch(which.algorithm$Algorithm, "accelAmbulationCut", "VM_gyroAmbulationCut", "VM_gyroAmbulationCut")

    which.sed.variable <- switch(which.algorithm$Algorithm, "ENMO", "Gyroscope_VM_DegPerS", "Gyroscope_VM_DegPerS")
    which.cwr.variable <- switch(which.algorithm$Algorithm, "ENMO_CV10s", "GVM_CV10s", "GVM_CV10s")

    alldata$Classification <- ifelse(alldata[, which.sed.variable] <= matched_Algorithm[[which.sed.cutpoint]],
        "SED", ifelse(alldata[, which.cwr.variable] <= matched_Algorithm[[which.cwr.cutpoint]], "CWR", "ILA"))

    alldata$Orig_index <- seq_len(nrow(alldata))

    models <- switch(which.algorithm$Algorithm, "A1", "A2", "A3")

    SED <- within(subset(alldata, Classification == "SED"), {
        METs = 1.25
    })
    CWR <- within(subset(alldata, Classification == "CWR"), {
        METs = predict(matched_Algorithm[[models]]$CWR, newdata = subset(alldata, Classification == "CWR"))
    })
    ILA <- within(subset(alldata, Classification == "ILA"), {
        METs = predict(matched_Algorithm[[models]]$ILA, newdata = subset(alldata, Classification == "ILA"))
    })

    alldata <- rbind(SED, CWR, ILA)
    alldata <- alldata[order(alldata$Orig_index), ]

    return(alldata$METs)
}
