get_minofday_int <- function(timestamp, format = "%Y-%m-%d %H:%M:%S") {
    minofday <- as.numeric(strftime(as.POSIXlt(timestamp, format = format), format = "%H")) * 60 + as.numeric(strftime(as.POSIXlt(timestamp, 
        format = format), format = "%M"))
    return(minofday)
}

get_minofday_rat <- function(timestamp, format = "%Y-%m-%d %H:%M:%S", epoch = 1) {
    minofday <- as.numeric(strftime(as.POSIXlt(timestamp, format = format), format = "%H")) * 60 + as.numeric(strftime(as.POSIXlt(timestamp, 
        format = format), format = "%M"))
    minofday <- minofday + (as.numeric(strftime(as.POSIXlt(timestamp, format = format), format = "%S"))/60)
    return(minofday)
}

get_dayofyear <- function(timestamp, format = "%Y-%m-%d %H:%M:%S", epoch = 1) {
    dayofyear <- as.numeric(strftime(as.POSIXlt(timestamp, format = format), format = "%j"))
    return(dayofyear)
}

get_age <- function(BD) {
    # Get age in both years and months (months is useful for youth BMI Z-scores) BM = birth month, CM = current
    # month NOTE: the day of month is included as a 'decimal' for these purposes (e.g. the 5th is 0.05, and the
    # max is 0.31).
    
    test_date <- as.Date(svDialogs::dlgInput("Enter Test Date", Sys.Date())$res)
    age <- lubridate::interval(start = BD, end = test_date)/duration(num = 1, units = "years")
    age_mos <- lubridate::interval(start = BD, end = test_date)/duration(num = 1, units = "months")
    
    age_frame <- data.frame(test_date = test_date, Age = round(age, 1), Agemos = round(age_mos, 1))
    
    return(age_frame)
}

get_bd_wait <- function(BD) {
    if (grepl("error", try(as.Date(BD)), ignore.case = T)) {
        stop("Birthdate must be in format %Y-%m-%d")
    }
    
    ## BD = birthdate, CBD = current birthdate Translate birthdate to current year
    CBD <- as.Date(paste(strftime(Sys.Date(), "%Y"), paste(unlist(strsplit(as.character(BD), "-"))[-1], collapse = "-"), 
        sep = "-"))
    
    ## BY = birthyear, CY = current year
    BY <- as.numeric(strftime(BD, "%Y"))
    CY <- as.numeric(strftime(Sys.Date(), "%Y"))
    
    wait <- if (difftime(Sys.Date(), CBD) >= 0) 
        "Yes" else "No"
    
    return(wait)
}

get.excel.time <- function(numericTime) {
    as.POSIXct(numericTime * (60 * 60 * 24), origin = "1899-12-30", tz = "GMT")
}
