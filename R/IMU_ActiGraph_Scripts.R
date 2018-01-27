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

#' Determine which variable(s) to calculate coefficient of variation on
#'
#' @param Algorithm the algorithm(s) selected to process the accelerometer/IMU data
#'
#' @keywords internal
get_cv_vars <- function(Algorithm, verbose = FALSE) {
  cvs <-
    unique(c("ENMO",
      "Gyroscope_VM_DegPerS",
      "Gyroscope_VM_DegPerS")[1:3 %in% Algorithm]
    )

  if(verbose) message_update(11, cvs = cvs)
  if(verbose) message_update(12)
  return(cvs)
}

#' Calculate coefficient of variation in sliding windows
#'
#' Calculates coefficient of variation using the approach of Crouter et al. (2010, \emph{Med Sci Sports Exerc})
#'
#' @param big_data a numeric vector on which to perform the calculation
#' @param window_secs size of the sliding window, in seconds
#' @param verbose a logical scalar: print progress updates?
#'
#' @return a numeric vector of values, giving the lowest coefficient of variation among the sliding windows that correspond to each epoch of data
#' @export
get_cvPER <- function(big_data, window_secs = 10, Algorithm, verbose = F) {
    if (verbose) message_update(13, window_secs = window_secs)

  inds <-
    sapply(seq(big_data),
          function(x) {
            sapply(window_secs:1, function(y) {
              ((x - y):(x - y + window_secs - 1)) + 1
            })
          }, simplify = F)

  CVS <-
    do.call(rbind,
      lapply(inds,
        function(x) {
            values <- sapply(data.frame(x), function(y) {
              Y <- y[y > 0 & y <= length(big_data)]
              if (length(y) != length(Y)) {
                data.frame(CV = NA)
              } else {
                data.frame(CV = cv(big_data[Y]))
              }
            }, simplify = F)

                CV <- sapply(do.call(rbind, values), min, na.rm = TRUE)
                return(CV)
    }
    ))

  stopifnot(ncol(CVS)==1 | is.vector(CVS))
  CVS <- as.vector(CVS)
  if (verbose) message_update(6)
  return(CVS)
}

#' Calculate direction changes per five seconds in sliding windows
#'
#' @inheritParams get_cvPER
#'
#' @return a numeric vector of values, giving the number of direction changes in the sliding window that corresponds to each epoch of data
#' @export
get.directions <- function(big_data, window_secs = 5) {
    if (window_secs%%2 != 1)
        stop("window_secs must be an odd number, to look forward and backward of the observation by equal amounts.")

    window_secs <- (window_secs - 1)/2

    inds <-
      sapply(seq_along(big_data), function(x) {
        if ((x - window_secs > 0) & (x + window_secs <= length(big_data))) {
          seq(x - window_secs, x + window_secs)
        } else
          NA
      }, simplify = F)

    changes <-
      sapply(inds, function(x) {
        if (is.na(x[1])) {
          NA
        } else {
          compareDirs <-
            diff(as.numeric(as.factor(big_data[x])))
          dirChange <-
            sum(ifelse(compareDirs == 0, 0, 1))
      }
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
