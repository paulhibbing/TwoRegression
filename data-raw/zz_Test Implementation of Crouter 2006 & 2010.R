# Preliminaries -----------------------------------------------------------

  rm(list = ls())
  devtools::load_all()


# Functions ---------------------------------------------------------------

  crouter_2006 <- function (cpm, cv_10, verbose = FALSE) {

    if (verbose) message_update(32, method = "Crouter 2006")

    ifelse(cpm <= 50,
      1,
      ifelse(cv_10 <= 10 & cv_10 != 0,
        2.379833 * exp(0.00013529 * cpm),
        2.330519 + (0.001646*cpm) - (1.2017e-7*(cpm^2)) + (3.3779*10e-12*(cpm^3))
      ))
  }

  crouter_2010 <- function (cp10, cv_10, verbose = FALSE) {

    if (verbose) message_update(32, method = "Crouter 2010")

    ifelse(cp10 <= 8,
      1,
      ifelse(cv_10 <= 10,
        2.294275 * exp(0.00084679 * cp10),
        0.749395 + (0.716431 * log(cp10)) -
        (0.179874 * log((cp10 ^ 2))) +
        (0.033173 * log((cp10 ^ 3)))
    ))
  }

  crouter_2012_VA <- function(cp10, cv_10, verbose = FALSE) {

    if (verbose) message_update(32, method = "Crouter 2012 VA")

    ifelse(
      cp10 <= 25,
      1,
      ifelse(
        cv_10 <= 35,
        1.982 * (exp(0.00101 * cp10)),
        2.842 + (0.00288 * cp10)
      )
    )
  }

  crouter_2012_VM <- function(cp10, cv_10, verbose = FALSE) {

    if (verbose) message_update(32, method = "Crouter 2012 VM")

    ifelse(
      cp10 <= 75,
      1,
      ifelse(
        cv_10 <= 25,
        0.0137 * exp(0.848 * (log(cp10))),
        1.219 -
          (0.145 * (log(cp10))) -
          (0.0586 * (log(cp10))^2) +
          (0.0229 * (log(cp10))^3)
      )
    )
  }

  crouter_2012 <- function(variable = c("VA", "VM"), cp10, cv_10, verbose = FALSE) {
    variable <- match.arg(variable)
    sapply(variable,
      function(x) {
        switch(
          x,
          "VA" = crouter_2012_VA(cp10, cv_10, verbose),
          "VM" = crouter_2012_VM(cp10, cv_10, verbose)
        )
      })
  }

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
    Crouter_2006_METs <- crouter_2006(
      AGread::reintegrate(ag, 60, direction = "backwards")$Axis1,
      get_cvPER(ag$Axis1, 6, TRUE, FALSE),
      TRUE
    )

  # 2010 METs ####
    Crouter_2010_METs <-
      crouter_2010(ag$Axis1, get_cvPER(ag$Axis1, 6, TRUE), TRUE)

    Crouter_2010_METs <-
      tapply(Crouter_2010_METs,
        rep(seq(ceiling(
          length(Crouter_2010_METs) / 6
        )), each = 6)[seq(length(Crouter_2010_METs))],
        mean)

  # 2012 VA METs ####
    Crouter_2012_METs_VA <-
      crouter_2012("VA", ag$Axis1, get_cvPER(ag$Axis1, 6, TRUE), TRUE)

    Crouter_2012_METs_VA <-
      tapply(Crouter_2012_METs_VA,
        rep(seq(ceiling(
          length(Crouter_2012_METs_VA) / 6
        )), each = 6)[seq(length(Crouter_2012_METs_VA))],
        mean)

  # 2012 VM METs ####
    Crouter_2012_METs_VM <-
      crouter_2012(
        "VM",
        ag$Vector.Magnitude,
        get_cvPER(ag$Vector.Magnitude, 6, TRUE),
        TRUE
    )

    Crouter_2012_METs_VM <-
      tapply(Crouter_2012_METs_VM,
        rep(seq(ceiling(
          length(Crouter_2012_METs_VM) / 6
        )), each = 6)[seq(length(Crouter_2012_METs_VM))],
        mean)


  # Combine ####
    indices <- seq(length(Crouter_2006_METs))
    ag <-
      data.frame(
        AGread::reintegrate(ag, 60, direction = "backwards"),
        Crouter_2006_METs,
        Crouter_2010_METs = Crouter_2010_METs[indices],
        Crouter_2012_METs_VA = Crouter_2012_METs_VA[indices],
        Crouter_2012_METs_VM = Crouter_2012_METs_VM[indices],
        stringsAsFactors = FALSE)
