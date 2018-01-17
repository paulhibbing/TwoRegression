## This first command just makes sure all the packages are installed that you need in order to use the
## prediction function
invisible(sapply(c("data.table", "dplyr", "seewave", "svDialogs"), function(x) {
    if (sum(grepl(x, row.names(installed.packages()))) == 0) install.packages(x)
}))

twoReg_process <- function(RAW, IMU = NULL, Wear.Location = c("Hip", "Left Wrist", "Right Wrist", "Left Ankle", 
    "Right Ankle"), PID, Algorithm = 1) {
    
    ## Read the data
    rawdata <- read.AG.raw(RAW)
    imudata <- read.IMU(IMU)
    
    ## Merge the data
    alldata <- merge(within(rawdata, {
        Timestamp = as.character(Timestamp)
    }), within(subset(imudata, select = -c(Timestamp)), {
        Time = as.character(Time)
    }), by.x = "Timestamp", by.y = "Time")
    
    names(alldata) <- gsub("\\.y$", "_IMU", gsub("\\.x$", "_PrimaryAccel", names(alldata)))
    
    ## Add the ID and order the variables
    alldata$PID <- PID
    firstVars <- c("PID", "filesource_PrimaryAccel", "dateProcessed_PrimaryAccel", "filesource_IMU", "dateProcessed_IMU", 
        "Timestamp", "dayofyear", "minofday")
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
    allProcesses <- expand.grid(Wear.Location = Wear.Location, Algorithm = Algorithm, stringsAsFactors = F)
    allPredictions <- setNames(data.frame(do.call(cbind, lapply(split(allProcesses, seq_len(nrow(allProcesses))), 
        apply.TwoRegression, alldata = alldata)), stringsAsFactors = F), unlist(lapply(split(allProcesses, 
        seq_len(nrow(allProcesses))), function(x) {
        paste(x$Wear.Location, gsub("^", "Agorithm", x$Algorithm), "METs", sep = "_")
    })))
    
    ## Final formatting and output of the data
    alldata <- cbind(alldata, allPredictions)
    
    cat("\n\n")
    print("All two-regression processing complete.")
    return(alldata)
}
