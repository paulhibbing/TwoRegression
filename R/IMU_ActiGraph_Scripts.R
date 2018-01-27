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
