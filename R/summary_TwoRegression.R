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
#' data(all_data, package = "TwoRegression")
#' all_data$PID <-
#'   rep(
#'     c("Test1", "Test2"),
#'     each = ceiling(nrow(all_data) / 2))[seq(nrow(all_data))]
#'
#' fake_sed <- c("Lying", "Sitting")
#' fake_lpa <- c("Sweeping", "Dusting")
#' fake_cwr <- c("Walking", "Running")
#' fake_ila <- c("Tennis", "Basketball")
#'
#' fake_activities <- c(fake_sed, fake_lpa, fake_cwr, fake_ila)
#'
#' all_data$Activity <-
#'   sample(fake_activities, nrow(all_data), TRUE)
#'
#' all_data$fake_METs <-
#'   ifelse(all_data$Activity %in% c(fake_sed, fake_lpa),
#'     runif(nrow(all_data), 1, 2),
#'     runif(nrow(all_data), 2.5, 8)
#'   )
#'
#' new_model <-
#'   form_2rm(
#'     data = all_data,
#'     activity_var = "Activity",
#'     sed_cp_activities = c(fake_sed, fake_lpa),
#'     sed_activities = fake_sed,
#'     sed_cp_var = "ENMO",
#'     sed_METs = 1.25,
#'     walkrun_activities = fake_cwr,
#'     walkrun_cp_var = "ENMO_CV10s",
#'     met_var = "fake_METs",
#'     walkrun_formula = "fake_METs ~ ENMO",
#'     intermittent_formula = "fake_METs ~ ENMO + I(ENMO^2) + I(ENMO^3)"
#'   )
#'
#' summary(
#'   new_model,
#'   subject_var = "PID",
#'   MET_var = "fake_METs",
#'   activity_var = "Activity"
#' )
summary.TwoRegression <- function(object, ...) {

  if (any(
    c("repro_TwoRegression", "Hibbing18_TwoRegression") %in%
      class(object)
  )) {
    stop(paste(
      "No summary method available for algorithms",
      "formed outside of `TwoRegression`."
    ))
  }

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
