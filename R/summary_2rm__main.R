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
#' ex_2rm <- fit_2rm(
#'   data = all_data,
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
#' summary(
#'   ex_2rm,
#'   subject_var = "PID",
#'   MET_var = "fake_METs",
#'   activity_var = "Activity"
#' )
summary.TwoRegression <- function(object, ...) {

  z <-
    new_summary(object) %>%
    new_cut(object, "sed_roc", "sedentary") %>%
    new_cut(object, "walkrun_roc", "walk_run") %>%
    new_model(object, "walkrun_model", "walk_run") %>%
    new_model(object, "intermittent_model", "intermittent_activity")

  ## Leave one out
  if (! is.null(object$all_data)) {
    z$leave_one_out <-
      DualCP_LOSO(model = object, data = object$all_data, ...) %>%
      {data.frame(
        RMSE = sqrt(mean(.$Error^2)),
        MAPE = mean((abs(.$Error) / .$Actual)*100)
      )}

  }

  z$algorithm <- get_2rm_formula(object)

  z

}

#' @keywords internal
#' @rdname summary.TwoRegression
new_summary <- function(object) {

  test <-
    c("repro_TwoRegression", "Hibbing18_TwoRegression") %>%
    {any(. %in% class(object))}
  if (test)  stop(
    "No summary method available for algorithms ",
    "formed outside of `TwoRegression`."
  )

  list(
    cut_points = NULL,
    regression_models = NULL,
    leave_one_out = NA,
    algorithm = NULL
  )

}

#' @keywords internal
#' @param z An intermediate summary object for internal use
#' @param element character. Element name to search for in \code{z}
#' @param label character. A label for the cut point
#' @rdname summary.TwoRegression
new_cut <- function(
  z, object, element = c("sed_roc", "walkrun_roc"),
  label = c("sedentary", "walk_run")
) {

  element <- match.arg(element)
  label   <- match.arg(label)

  new_name <-
    object$sed_variable %>%
    paste(label, ., sep = "_")

  if (!is.null(object[[element]])) {
    z$cut_points[[new_name]] <- pROC::coords(
      object[[element]],
      "best",
      best.method = "closest.topleft",
      transpose = TRUE
    )
    return(z)
  }

  element <- gsub("_roc", "_cutpoint", element)
  if (! is.null(object[[element]])) {
    z$cut_points[[new_name]] <- object[[element]]
    return(z)
  }

  stop("Cut-point retrieval failed")

}

#' @keywords internal
#' @rdname summary.TwoRegression
new_model <- function(
  z, object, element = c("walkrun_model", "intermittent_model"),
  label = c("walk_run", "intermittent_activity")
) {

  element <- match.arg(element)
  label <- match.arg(label)

  form <- gsub("_model", "_formula", element)

  ## Walk/run model
  if (! is.null(object[[element]])) {
    z$regression_models[[label]] <- data.frame(
      formula = object[[form]],
      adj.r.squared = summary(object[[element]])$adj.r.squared,
      see = summary(object[[element]])$sigma,
      stringsAsFactors = FALSE
    )
  } else {
    z$regression_models$walk_run <- data.frame(
      formula = NA,
      adj.r.squared = NA,
      see = NA,
      stringsAsFactors = FALSE
    )
  }

  z

}
