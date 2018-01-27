#' Get file metadata (sampling frequency and timestamps) for primary accelerometer
#'
#' @param file character scalar giving path to primary accelerometer file
#'
#' @keywords internal
get_file_meta <- function(file) {
  file.meta <-
    data.frame(data.table::fread(
      file,
      nrow = 10,
      header = F,
      sep = "\n"
    ))
  samp.freq <-
    file.meta[sapply(file.meta, function(x) {
      grepl("Hz", x, ignore.case = T)
      }),]
  samp.freq <-
    as.numeric(unlist(strsplit(samp.freq, " "))[which(grepl("Hz", unlist(strsplit(samp.freq, " ")), ignore.case = T)) - 1])

  start.time <-
    gsub("[[:alpha:] ,]", "", file.meta[sapply(file.meta, function(x) {
      grepl("start[. ]time", x,
        ignore.case = T)
      }), ]
      )
  start.date <-
    gsub("[[:alpha:] ,]", "", file.meta[sapply(file.meta, function(x) {
      grepl("start[. ]date", x,
        ignore.case = T)
      }), ])
  start <-
    as.POSIXlt(paste(start.date, start.time), format = "%m/%d/%Y %H:%M:%S")
  if(is.na(start)) message_update(3, is_message = TRUE)
  return(list(start = start, samp.freq = samp.freq))
}

#' Collapse primary accelerometer data
#'
#' @param AG a dataframe of raw primary accelerometer data
#'
#' @return
#' @keywords internal
AG_collapse <- function(AG, output.window, samp.freq) {
  blocksize <- samp.freq * output.window
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
getVM <- function(triaxial, verbose = T) {
    if (verbose) {
        vm_variables <- gsub("\"", "", substring(deparse(substitute(triaxial)), unlist(gregexpr("\"", deparse(substitute(triaxial))))[1],
            unlist(gregexpr("\"", deparse(substitute(triaxial))))[2]))

        message_update(1, vm_variables = vm_variables)
        # cat('\n Getting VM for variables searched on the following criteri(a/on):', gsub('\'','',
        # substring(deparse(substitute(triaxial)), unlist(gregexpr('\'', deparse(substitute(triaxial))))[1],
        # unlist(gregexpr('\'', deparse(substitute(triaxial))))[2])), '\n')
    }
    triaxial <- triaxial[, !grepl("VM", names(triaxial))]
    stopifnot(ncol(triaxial) == 3)
    apply(triaxial, 1, function(x) sqrt(sum(x^2)))
}

cv <- function(signal) if (mean(signal) == 0) 0 else sd(signal)/mean(signal) * 100

classify.magnetometer <- function(x = "Magnetometer X", y = "Magnetometer Y", z = "Magnetometer Z", orientation = "vertical") {

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
#' @param output.window The desired epoch length; defaults to one second
#'
#' @return A dataframe giving processed raw data from the primary accelerometer in the specified epoch length
#' @export
read.AG.raw <- function(file, output.window = 1) {
    timer <- proc.time()

    message_update(1)

    meta <- get_file_meta(file)

    AG <-
      data.table::fread(file, stringsAsFactors = F, showProgress = F, skip = 10)
    AG$Timestamp <-
      meta$start + ((seq(nrow(AG)) - 1) / meta$samp.freq)

    AG <- AG_collapse(AG, output.window, samp.freq)

    ## Get ENMO
    ENMO <- sqrt(AG$Gx^2 + AG$Gy^2 + AG$Gz^2) - 1
    ENMO[which(ENMO < 0)] <- 0
    ENMO2 <- cumsum(ENMO)
    ENMO3 <-
      diff(ENMO2[seq(1, length(ENMO), by = (samp.freq * output.window))]) /
      (samp.freq * output.window)

    final_length = min(c(length(ENMO3), nrow(data)))
    AG <- data.frame(AG$AG[1:final_length, ])
    ENMO3 <- ENMO3[1:final_length]
    AG$ENMO <- ENMO3 * 1000

    if(any(grepl('block', names(AG), ignore.case = TRUE)) {
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

## IMU data file reader function
read.IMU <- function(file, output.window.secs = 1) {
    timer <- proc.time()

    # Start Processing
    if (sum(grepl("dplyr", search())) != 1)
        library(dplyr, quietly = T, verbose = F, warn.conflicts = F)
    if (sum(grepl("data.table", search())) != 1)
        library(data.table, quietly = T, verbose = F, warn.conflicts = F)
    if (sum(grepl("svDialogs", search())) != 1)
        library(svDialogs, quietly = T, verbose = F, warn.conflicts = F)
    if (sum(grepl("seewave", search())) != 1)
        library(seewave, quietly = T, verbose = F, warn.conflicts = F)

    # Read the header in and prepare some meta-variables
    header <- read.csv(file, nrow = 20, stringsAsFactors = F, header = F)

    samp.rate <- unlist(strsplit(header[, 1], " "))
    samp.rate <- suppressWarnings(try(as.numeric(samp.rate[which(samp.rate == "Hz") - 1])))
    if (grepl("error", samp.rate, ignore.case = T)) {
        message("Unable to detect sampling rate. Defaulting to 100")
        samp.rate = 100
    }

    date.index <- which(grepl("start date", header[, 1], ignore.case = T))
    time.index <- which(grepl("start time", header[, 1], ignore.case = T))

    start.time <- as.POSIXlt(gsub("[[:alpha:] ]", "", paste(header[date.index, 1], header[time.index, 1])),
        format = "%m/%d/%Y%H:%M:%S")

    block.size <- samp.rate * output.window.secs

    # Read the data and groom it
    cat("\nProcessing", file, "...")
    data <- suppressWarnings(try(data.table::fread(file, stringsAsFactors = F, skip = 20, showProgress = F)))
    if (sum(unlist(sapply(data, function(x) sum(grepl("error", x, ignore.case = T))))) > 0) {
        message("Error in file formatting. Returning NULL.")
        return(NULL)
    }
    data <- setNames(data.frame(data), rev(header[rev(seq_len(nrow(header)))[1:ncol(data)], ]))

    data <- within(data, {
        filesource <- file
        dateProcessed <- Sys.time()
        Time <- start.time + (0:(nrow(data) - 1)/samp.rate)
    })

    # These two steps check to see if the data start on an exact second
    data$ms <- as.numeric(substring(data$Timestamp, regexpr(".[^.]*$", data$Timestamp)))
    data <- data[-c(1:(which(data$ms == 0)[1] - 1)), ]

    # Low-Pass filter the Gyroscope data at 35 Hz
    cat("\n\n-- Filtering Gyroscope...")
    data[, grepl("gyroscope", names(data), ignore.case = T)] <- sapply(data[, grepl("gyroscope", names(data),
        ignore.case = T)], function(x) bwfilter(wave = x, f = samp.rate, n = 2, to = 35))
    cat(" Done.\n")

    # Calculate vector magnitudes
    cat("\n-- Calculating Vector Magnitudes...")

    data$mean_Accel_VM <- getVM(data[, grepl("accelerometer", names(data), ignore.case = T)])

    data$Gyroscope_VM_DegPerS <- getVM(data[, grepl("gyroscope", names(data), ignore.case = T)])

    data$Magnetometer_VM_MicroT <- getVM(data[, grepl("magnetometer", names(data), ignore.case = T)])

    cat("\n     Vector magnitude calculation complete.\n")

    # Now for the reducing
    if (nrow(data)%%block.size != 0) {
        message("Number of rows not divisible by samp.rate*output.window\nTruncating data.")
        final_obs <- rev(seq_len(nrow(data)))[which(rev(seq_len(nrow(data)))[1:block.size]%%(block.size) ==
            0)[1]]
        data <- data[1:final_obs, ]
    }

    data$epoch <- rep(1:(nrow(data)/block.size), each = block.size)

    cat("\n-- Collapsing data. This could take awhile...")
    data <- data %>% group_by(epoch) %>% summarise(dateProcessed = first(dateProcessed), filesource = first(filesource),
        Timestamp = first(Timestamp), Time = first(Time), Gyroscope_VM_DegPerS = mean(Gyroscope_VM_DegPerS),
        mean_abs_Gyroscope_x_DegPerS = mean(abs(`Gyroscope X`)), mean_abs_Gyroscope_y_DegPerS = mean(abs(`Gyroscope Y`)),
        mean_abs_Gyroscope_z_DegPerS = mean(abs(`Gyroscope Z`)), mean_MagnetometerDirection = classify.magnetometer(mean(`Magnetometer X`),
            mean(`Magnetometer Y`), mean(`Magnetometer Z`)))

    cat(" Done!\n")
    data <- as.data.frame(data)

    first_variables <- c("filesource", "dateProcessed", "Time")

    data <- data[, c(first_variables, setdiff(names(data), first_variables))]

    data$epoch <- NULL

    duration <- unname((proc.time() - timer)[3])
    cat("\nFile processed. Processing took", round(duration/60, 2), "minutes.\n\n\n")

    return(data)
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
