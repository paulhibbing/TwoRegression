#' Predict metabolic equivalents from a TwoRegression object
#'
#' @param object the TwoRegression object
#' @param newdata the data on which to predict metabolic equivalents
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
predict.TwoRegression <- function(object, newdata, ...) {

  # Classify each observation and manually keep track of order
  newdata$Classification <- ifelse(
    newdata[, object$sed_variable] <= object$sed_cutpoint,
    "SED", ifelse(newdata[, object$cwr_variable] <= object$sed_cutpoint,
      "CWR",
      "ILA")
  )

  newdata$Orig_index <- seq(nrow(newdata))

  ##Make predictions after initializing a MET variable to NA
  newdata$METs <- NA

  ##Predict sedentary METs
  SED <- newdata[newdata$Classification == "SED", ]
  if(nrow(SED) > 0) {
    SED$METs <- object$sed_METs
  }

  ##Predict CWR METs
  CWR <- newdata[newdata$Classification == "CWR", ]
  if(nrow(CWR) > 0) {
    CWR$METs <-
      predict(object$cwr_model, newdata = CWR)
  }

  ##Predict ILA METs
  ILA <- newdata[newdata$Classification == "ILA", ]
  if(nrow(ILA) > 0) {
    ILA$METs <-
      predict(object$ila_model, newdata = ILA)
  }

  newdata <- rbind(SED, CWR, ILA)
  newdata <- newdata[order(newdata$Orig_index), ]

  stopifnot(all(!is.na(newdata$METs)))
  return(newdata[ , c("Classification", "METs")])
}
