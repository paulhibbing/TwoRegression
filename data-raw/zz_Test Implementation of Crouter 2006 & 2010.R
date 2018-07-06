# Preliminaries -----------------------------------------------------------

  rm(list = ls())
  devtools::load_all()
  load("R/sysdata.rda")

# Implementation -----------------------------------------------------

  # Read File ####
    ag <-
      data.table::fread(file.path("data-raw/Crouter 2RM",
        "NTPACYR2FLHI 512 (2011-04-05)1sec10secDataTable.csv"),
        stringsAsFactors = FALSE, skip = 1)
    ag <- data.frame(AGread:::rm_trail_na(ag))
    ag$Timestamp <-
      as.POSIXct(with(ag, paste(Date, Time)),
        "UTC", format = "%m/%d/%Y %H:%M:%S")

  # 2006 METs ####
    AG <-
      AGread::reintegrate(ag, 60, direction = "backwards")
    AG$cv_10 <-
      get_cvPER(ag$Axis1, 6, TRUE, FALSE)[-1][seq(nrow(AG))]
    AG$Crouter_2006_METs <-
      predict(crouter06, AG, TRUE)$METs[-1][seq(nrow(AG))]

  # 2010 METs ####
    ag$cv_10 <- get_cvPER(ag$Axis1, 6, TRUE)
    Crouter_2010_METs <- predict(crouter10, ag, TRUE)$METs

    AG$Crouter_2010_METs <-
      tapply(Crouter_2010_METs,
        rep(seq(ceiling(
          length(Crouter_2010_METs) / 6
        )), each = 6)[seq(length(Crouter_2010_METs))],
        mean)[-1][seq(nrow(AG))]

  # 2012 VA METs ####
    Crouter_2012_METs_VA <- predict(crouter12_VA, ag, TRUE)$METs

    AG$Crouter_2012_METs_VA <-
      tapply(Crouter_2012_METs_VA,
        rep(seq(ceiling(
          length(Crouter_2012_METs_VA) / 6
        )), each = 6)[seq(length(Crouter_2012_METs_VA))],
        mean)[-1][seq(nrow(AG))]

  # 2012 VM METs ####
    ag$cv_10 <- get_cvPER(ag$Vector.Magnitude, 6, TRUE)
    Crouter_2012_METs_VM <- predict(crouter12_VM, ag, TRUE)$METs

    AG$Crouter_2012_METs_VM <-
      tapply(Crouter_2012_METs_VM,
        rep(seq(ceiling(
          length(Crouter_2012_METs_VM) / 6
        )), each = 6)[seq(length(Crouter_2012_METs_VM))],
        mean)[-1][seq(nrow(AG))]

  # Inspect ####
    View(AG)
