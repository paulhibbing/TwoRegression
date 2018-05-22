#' Summary method for TwoRegression objects
#'
#' @param object The TwoRegression object to summarize
#' @param ... Arguments passed to \code{\link{DualCP_LOSO}}
#'
#' @return and object of class \code{summary.TwoRegression}, containing
#'   information about the cut-points and diagnostics, regression model
#'   performance, and overall algorithm performance during
#'   leave-one-participant-out-cross-validation.
#' @export
#'
#' @examples
#'
summary.TwoRegression <- function(object, ...) {
  ## Initialize
  z <-
    list(
      cut_points = NULL,
      regression_models = NULL,
      leave_one_out = NULL,
      algorithm = NULL
    )

  ## Sedentary cut-point
  if (! is.null(object$sed_roc)) {
    z$cut_points$sedentary <-
      pROC::coords(object$sed_roc, "best",
        best.method = "closest.topleft")
  } else {
    if (! is.null(object$sed_cutpoint)) {
      z$cut_points$sedentary <-
        object$sed_cutpoint
    }
  }

  names(z$cut_points)[names(z$cut_points) == "sedentary"] <-
    paste("sedentary", object$sed_variable, sep = "_")

  ## Ambulation cut-point
  if (! is.null(object$walkrun_roc)) {
    z$cut_points$walkrun <-
      pROC::coords(object$walkrun_roc, "best",
        best.method = "closest.topleft")
  } else {
    if (! is.null(object$walkrun_cutpoint)) {
      z$cut_points$walkrun <-
        object$walkrun_cutpoint
    }
  }

  names(z$cut_points)[names(z$cut_points) == "walkrun"] <-
    paste("walk_run", object$sed_variable, sep = "_")

  ## Walk/run model
  if (! is.null(object$walkrun_model)) {
    z$regression_models$walk_run <-
      data.frame(
        formula = object$walkrun_formula,
        adj.r.squared = summary(object$walkrun_model)$adj.r.squared,
        see = summary(object$walkrun_model)$sigma,
        stringsAsFactors = FALSE
        )
  } else {
    z$regression_models$walk_run <-
      data.frame(
        formula = NA,
        adj.r.squared = NA,
        see = NA,
        stringsAsFactors = FALSE
      )
  }

  ## Intermittent model
  if (! is.null(object$intermittent_model)) {
    z$regression_models$intermittent_activity <-
      data.frame(
        formula = object$intermittent_formula,
        adj.r.squared = summary(object$intermittent_model)$adj.r.squared,
        see = summary(object$intermittent_model)$sigma,
        stringsAsFactors = FALSE
      )
  } else {
    z$regression_models$intermittent_model <-
      data.frame(
        formula = NA,
        adj.r.squared = NA,
        see = NA,
        stringsAsFactors = FALSE
      )
  }

  ## Leave one out
  if (! is.null(object$all_data)) {
    data_wPredictions <-
      DualCP_LOSO(object, data = object$all_data, ...)
    z$leave_one_out <-
      data.frame(
        RMSE = sqrt(mean(data_wPredictions$Error^2)),
        MAPE = mean((abs(data_wPredictions$Error) /
            data_wPredictions$Actual)*100)
      )

  } else {
    z$leave_one_out <- NA
  }

  z$algorithm <- get_2rm_formula(object)

  return(z)
}
