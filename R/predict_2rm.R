#' Predict metabolic equivalents from a TwoRegression object
#'
#' @param object the TwoRegression object
#' @param newdata the data on which to predict metabolic equivalents (METs)
#' @param min_mets the minimum allowable value for MET predictions. Defaults to
#'   the value stored in \code{object$sed_METs}
#' @param max_mets the maximum allowable value for MET predictions. There is no
#'   value embedded in \code{object}. The default is 20
#' @param warn_high_low logical. Issue warnings about values less than
#'   \code{min_mets} or greater than \code{max_mets}?
#' @param verbose logical. Print processing updates?
#' @param ... further arguments passed to or from other methods
#'
#' @return A two-column data frame giving the activity classification
#'   (sedentary, walk/run, or intermittent activity) and the corresponding
#'   metabolic equivalent prediction
#'
#' @examples
#' data(all_data, package = "TwoRegression")
#' all_data$PID <-
#'   rep(
#'     c("Test1", "Test2"),
#'     each = ceiling(nrow(all_data) / 2))[seq(nrow(all_data))]
#'
#' train_data <- all_data[all_data$PID != "Test2", ]
#' test_data <- all_data[all_data$PID == "Test2", ]
#'
#' fake_sed <- c("Lying", "Sitting")
#' fake_lpa <- c("Sweeping", "Dusting")
#' fake_cwr <- c("Walking", "Running")
#' fake_ila <- c("Tennis", "Basketball")
#'
#' fake_activities <- c(fake_sed, fake_lpa, fake_cwr, fake_ila)
#'
#' train_data$Activity <-
#'   sample(fake_activities, nrow(train_data), TRUE)
#'
#' train_data$fake_METs <-
#'   ifelse(train_data$Activity %in% c(fake_sed, fake_lpa),
#'     runif(nrow(train_data), 1, 2),
#'     runif(nrow(train_data), 2.5, 8)
#'   )
#'
#' ex_2rm <- fit_2rm(
#'   data = train_data,
#'   activity_var = "Activity",
#'   sed_cp_activities = c(fake_sed, fake_lpa),
#'   sed_activities = fake_sed,
#'   sed_cp_var = "ENMO",
#'   sed_METs = 1.25,
#'   walkrun_activities = fake_cwr,
#'   walkrun_cp_var = "ENMO_CV10s",
#'   met_var = "fake_METs",
#'   walkrun_formula = "fake_METs ~ ENMO",
#'   intermittent_formula = "fake_METs ~ ENMO + I(ENMO^2) + I(ENMO^3)"
#' )
#'
#' predict(ex_2rm, test_data)
#'
#' @export
predict.TwoRegression <- function (
  object, newdata, min_mets = object$sed_METs,
  max_mets = 20, warn_high_low = TRUE, verbose = FALSE, ...
) {


  if (verbose) message_update(32, method = object$method)


  ## Make some behind-the-scenes tweaks to standardize this method for objects
  ## that were formed inside vs. outside of `fit_2rm`

    if (any(grepl("^walkrun", names(object)))) {
      names(object) %<>% gsub("^walkrun", "cwr", .)
    }

    if (any(grepl("^intermittent", names(object)))) {
      names(object) %<>% gsub("^intermittent", "ila", .)
    }


  ## Classify each observation and manually keep track of order

    sed_test <- newdata[, object$sed_variable] <= object$sed_cutpoint
    cwr_test <- newdata[, object$cwr_variable] <= object$cwr_cutpoint

    if (!object$CV_zero_cwr) {
      cwr_test <-
        newdata[, object$cwr_variable] <= object$cwr_cutpoint &
        newdata[, object$cwr_variable] != 0
    }

    newdata$Classification <-
      ifelse(cwr_test, "CWR", "ILA") %>%
      ifelse(sed_test, "SED", .)

    newdata$Orig_index <- seq(nrow(newdata))


  ## Make predictions after initializing a MET variable to NA

    newdata$METs <- NA


  ## Separate missing entries

    class_NA <- newdata[is.na(newdata$Classification),  ]
    newdata  <- newdata[!is.na(newdata$Classification), ]


  ## Predict sedentary METs

    SED <- newdata[newdata$Classification == "SED", ]
    if(nrow(SED) > 0) {
      SED$METs <- min_mets
    }


  ## Predict CWR METs

    CWR <- newdata[newdata$Classification == "CWR", ]
    if(nrow(CWR) > 0) {

      if (any("repro_TwoRegression" %in% class(object))) {
        CWR$METs <- object$cwr_model(CWR[ ,object$cwr_eq_vars])
      } else {
        CWR$METs <- predict(object$cwr_model, newdata = CWR)
      }

    }


  ## Predict ILA METs

    ILA <- newdata[newdata$Classification == "ILA", ]
    if(nrow(ILA) > 0) {

      if (any("repro_TwoRegression" %in% class(object))) {
        ILA$METs <- object$ila_model(ILA[ ,object$ila_eq_vars])
      } else {
        ILA$METs <- predict(object$ila_model, newdata = ILA)
      }

    }

    newdata <-
      rbind(class_NA, SED, CWR, ILA) %>%
      dplyr::arrange(Orig_index) %>%
      dplyr::select(-Orig_index) %>%
      dplyr::mutate(
        Classification = factor(
          Classification,
          c("SED", "CWR", "ILA"),
          c("SB", "walkrun", "intermittent")
        )
      )


  ## Test for missing MET values

    test_original <-
      names(newdata) %>%
      setdiff(c("Classification", "METs")) %>%
      newdata[ ,.] %>%
      apply(1, anyNA)

    test_new <-
      newdata[ ,c("Classification", "METs")] %>%
      apply(1, anyNA)

    stopifnot(all(
      ifelse(

        !test_new,    # If prediction is not missing
        TRUE,         # Give TRUE
        test_original # Otherwise give FALSE unless the
                    # original data were also missing
      )
    ))


  ## Test for MET values outside the specified range

    too_low <- newdata$METs < min_mets

    if (any(too_low)) {

      min_label <- if (min_mets == 1) "1 MET" else paste(min_mets, "METs")

      if (warn_high_low) warning(
        "Rounding up ", sum(too_low), " MET value(s) below the minimum (",
        min_label, ") for the ", sQuote(object$method),
        " model", call. = FALSE
      )

      newdata$METs %<>% pmax(min_mets)

    }


    too_high <- newdata$METs > max_mets

    if (any(too_high)) {

      max_label <- if (max_mets == 1) "1 MET" else paste(max_mets, "METs")

      if (warn_high_low) warning(
        "Rounding down ", sum(too_high), " MET value(s) above the maximum (",
        max_label, ") for the ", sQuote(object$method),
        " model", call. = FALSE
      )

      newdata$METs %<>% pmin(max_mets)

    }


  ## Finish up

    newdata


}
