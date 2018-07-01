# Preliminaries -----------------------------------------------------------

  rm(list = ls())
  devtools::load_all()


# Functions ---------------------------------------------------------------

  crouter_2006 <- function (cpm, cv_10) {
    #cpm <- ag$Axis1
    ifelse(cpm <= 50,
      1,
      ifelse(cv_10 <= 10 & cv_10 != 0,
        2.379833 * exp(0.00013529 * cpm),
        2.330519 + (0.001646*cpm) - (1.2017e-7*(cpm^2)) + (3.3779*10e-12*(cpm^3))
      ))
  }

  crouter_2010 <- function (cp10, cv_10) {
    #cp10 <- ag$Axis1
    #cv_10 <- suppressWarnings(get_cv_sliding(ag$Axis1))
    ifelse(cp10 <= 8,
      1,
      ifelse(cv_10 <= 10,
        2.294275 * exp(0.00084679 * cp10),
        0.749395 + (0.716431 * log(cp10)) -
        (0.179874 * log((cp10 ^ 2))) +
        (0.033173 * log((cp10 ^ 3)))
    ))
  }

# 2006 Implementation -----------------------------------------------------

  ag <-
    data.table::fread(file.path("data-raw/Crouter 2RM",
      "NTPACYR2FLHI 512 (2011-04-05)1sec10secDataTable.csv"),
      stringsAsFactors = FALSE, skip = 1)
  ag <- data.frame(AGread:::rm_trail_na(ag))
  ag$Timestamp <-
    as.POSIXct(with(ag, paste(Date, Time)), "UTC", format = "%m/%d/%Y %H:%M:%S")

  Crouter_2006_METs <- crouter_2006(
    AGread::reintegrate(ag, 60, direction = "backwards")$Axis1,
    TwoRegression::get_cvPER(ag$Axis1, 6)
  )

  ag <-
    data.frame(
      AGread::reintegrate(ag, 60, direction = "backwards"),
      Crouter_2006_METs,
      stringsAsFactors = FALSE)

# 2010 Implementation -----------------------------------------------------

  ag$crouter_METs <-
    crouter_2010(ag$Axis1, suppressWarnings(get_cv_sliding(ag$Axis1)))

  ag <- get_blocks(ag, "Timestamp", 60, start_epoch, block_size, "backwards")

  Crouter_2010_METs <-
    tapply(ag$crouter_METs, ag$block_no, mean, na.rm = TRUE)

  ag <-
    data.frame(reintegrate(ag, 60, direction = "backwards"),
      Crouter_2010_METs,
      stringsAsFactors = FALSE)
