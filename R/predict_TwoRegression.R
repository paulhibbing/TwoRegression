#' Predict metabolic equivalents from a TwoRegression object
#'
#' @param object the TwoRegression object
#' @param newdata the data on which to predict metabolic equivalents
#' @param verbose logical. Print processing updates?
#' @param ... further arguments passed to or from other methods
#'
#' @return A two-column data frame giving the activity classification
#'   (sedentary, walk/run, or intermittent activity) and the corresponding
#'   metabolic equivalent prediction
#' @export
#'
#' @examples
#' data(all_data, package = "TwoRegression")
#' newdata <- all_data
#'
#' predict(TwoRegression:::Algorithms$Hip$Hibbing18_Hip_A1, newdata)
predict.TwoRegression <- function (object, newdata, verbose = FALSE, ...) {

  if (verbose) message_update(32, method = object$method)

  # Classify each observation and manually keep track of order
  sed_test <- newdata[, object$sed_variable] <= object$sed_cutpoint
  cwr_test <- newdata[, object$cwr_variable] <= object$cwr_cutpoint
  if (!object$CV_zero_cwr) {
    cwr_test <-
      newdata[, object$cwr_variable] <= object$cwr_cutpoint &
      newdata[, object$cwr_variable] != 0
  }

  newdata$Classification <-
    ifelse(
      sed_test,
      "SED",
      ifelse(
        cwr_test,
        "CWR",
        "ILA"
      )
    )

  newdata$Orig_index <- seq(nrow(newdata))

  ##Make predictions after initializing a MET variable to NA
  newdata$METs <- NA

  ##Separate missing entries
  class_NA <- newdata[is.na(newdata$Classification),  ]
  newdata  <- newdata[!is.na(newdata$Classification), ]

  ##Predict sedentary METs
  SED <- newdata[newdata$Classification == "SED", ]
  if(nrow(SED) > 0) {
    SED$METs <- object$sed_METs
  }

  ##Predict CWR METs
  CWR <- newdata[newdata$Classification == "CWR", ]
  if(nrow(CWR) > 0) {
    if (any("repro_TwoRegression" %in% class(object))) {
      CWR$METs <-
        object$cwr_model(CWR[ ,object$cwr_eq_vars])
    } else {
      CWR$METs <-
        predict(object$cwr_model, newdata = CWR, verbose)
    }
  }

  ##Predict ILA METs
  ILA <- newdata[newdata$Classification == "ILA", ]
  if(nrow(ILA) > 0) {
    if (any("repro_TwoRegression" %in% class(object))) {
      ILA$METs <-
        object$ila_model(ILA[ ,object$ila_eq_vars])
    } else {
      ILA$METs <-
        predict(object$ila_model, newdata = ILA, verbose)
    }
  }

  newdata <- rbind(class_NA, SED, CWR, ILA)
  newdata <- newdata[order(newdata$Orig_index), ]

  # Test for missing MET values
  test_original <-
    apply(newdata[ ,setdiff(names(newdata),
      c("Classification", "METs"))],
      1, anyNA)
  test_new <-
    apply(newdata[ ,c("Classification", "METs")],
      1, anyNA)
  stopifnot(all(
    ifelse(!test_new, # If prediction is not missing
      TRUE, # Give TRUE
      test_original # Otherwise give FALSE unless the
                    # original data were also missing
    )
  ))
  return(newdata[ , c("Classification", "METs")])
}
