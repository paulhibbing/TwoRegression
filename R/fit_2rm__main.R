#' Develop a two-regression algorithm
#'
#' @param data The data with which to develop the algorithm
#' @param activity_var Character scalar. Name of the variable defining which
#'   activity is being performed
#' @param sed_cp_activities Character vector. Activities to be included in the
#'   process of forming the sedentary classifier
#' @param sed_activities Character vector. Actual sedentary activities
#' @param sed_cp_var Character scalar. Name of the variable on which the
#'   sedentary cut-point is defined
#' @param sed_METs Numeric scalar. Metabolic equivalent value to apply to
#'   sedentary activities
#' @param walkrun_activities Character vector. Actual ambulatory activities
#' @param walkrun_cp_var Character scalar. Name of the variable on which the
#'   walk/run cut-point is defined
#' @param met_var Character scalar. Name of the variable giving actual energy
#'   expenditure (in metabolic equivalents)
#' @param walkrun_formula Character scalar. Formula to use for developing the
#'   walk/run regression model
#' @param intermittent_formula Character scalar. Formula to use for developing the
#'   intermittent activity regression model
#' @param method character scalar. Optional name for the model, potentially
#'   useful for printing.
#'
#' @return An object of class `TwoRegression`
#' @export
#'
#' @seealso
#' \code{\link{predict.TwoRegression}}
#' \code{\link{summary.TwoRegression}}
#' \code{\link{plot.TwoRegression}}
#'
#' @examples
#' data(all_data, package = "TwoRegression")
#' fake_sed <- c("Lying", "Sitting")
#' fake_lpa <- c("Sweeping", "Dusting")
#' fake_cwr <- c("Walking", "Running")
#' fake_ila <- c("Tennis", "Basketball")
#'
#' fake_activities <- c(fake_sed, fake_lpa, fake_cwr, fake_ila)
#'
#' all_data$Activity <- sample(fake_activities, nrow(all_data), TRUE)
#'
#' all_data$fake_METs <-
#'   ifelse(all_data$Activity %in% c(fake_sed, fake_lpa),
#'     runif(nrow(all_data), 1, 2),
#'     runif(nrow(all_data), 2.5, 8)
#'   )
#'
#' fit_2rm(
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
fit_2rm <- function(
  data, activity_var, sed_cp_activities, sed_activities, sed_cp_var,
  sed_METs, walkrun_activities, walkrun_cp_var, met_var, walkrun_formula,
  intermittent_formula, method = "`user_unspecified`"
) {

  # SB Cut-Point

    roc_sb <- get_cut_point(
      data, activity_var, sed_cp_activities,
      sed_activities, sed_cp_var
    )

    sb_cp <-
      roc_sb %>%
      pROC::coords(
        "best", best.method = 'closest.topleft', transpose = TRUE
      ) %>%
      .[1] %>%
      round(2)


  # walkrun Cut-Point

    roc_walkrun <- get_cut_point(
      data[data[ ,sed_cp_var] > sb_cp, ], activity_var,
      unique(data[ ,activity_var]), walkrun_activities,
      walkrun_cp_var
    )

    walkrun_cp <-
      roc_walkrun %>%
      pROC::coords(
        "best", best.method = 'closest.topleft', transpose = TRUE
      ) %>%
      .[1] %>%
      round(2)


  # walkrun Model

    is_walkrun_data <-
      (data[ ,sed_cp_var] > sb_cp) &
      (data[ ,walkrun_cp_var] <= walkrun_cp)

    walkrun_data <- data[is_walkrun_data, ]
    walkrun_model <- lm(walkrun_formula, data = walkrun_data)
    attr(walkrun_model$terms, ".Environment") <- NULL
    attr(walkrun_model$model, "terms") <- walkrun_model$terms


  # intermittent Model

    is_intermittent_data <-
      (data[ ,sed_cp_var] > sb_cp) &
      (data[ ,walkrun_cp_var] > walkrun_cp)

    intermittent_data <- data[is_intermittent_data, ]
    intermittent_model <- lm(intermittent_formula, data = intermittent_data)
    attr(intermittent_model$terms, ".Environment") <- NULL
    attr(intermittent_model$model, "terms") <- intermittent_model$terms


  # Fit the object

    list(
      sed_METs = sed_METs, sed_roc = roc_sb,
      sed_cutpoint = sb_cp, sed_variable = sed_cp_var,
      walkrun_roc = roc_walkrun, walkrun_cutpoint = walkrun_cp,
      walkrun_variable = walkrun_cp_var, walkrun_model = walkrun_model,
      walkrun_formula = walkrun_formula, walkrun_data = walkrun_data,
      intermittent_model = intermittent_model,
      intermittent_formula = intermittent_formula,
      intermittent_data = intermittent_data, all_data = data,
      CV_zero_cwr = TRUE, method = method
    ) %>%
    structure(class = "TwoRegression")

}


#' Check if an object has class TwoRegression
#'
#' @rdname fit_2rm
#'
#' @param x object to be tested
#'
#' @export
#'
is.TwoRegression <- function(x) {
  inherits(x, "TwoRegression")
}


#' Develop a cut-point as part of the process for developing a two-regression
#' algorithm
#'
#' @param data the data on which to perform the operation
#' @param activity_var character scalar. Name of the variable that indicates
#'   what activity each data point corresponds to.
#' @param included_activities character vector. All activities included in the
#'   data subset to be used for developing the cut-point
#' @param positive_activities character vector. Activities considered part of
#'   the positive class
#' @param cp_var Character scalar. Name of the variable on which the cut-point
#'   will be based.
#'
#' @return an object of class `roc`
#' @keywords internal
#'
get_cut_point <- function(
  data, activity_var, included_activities, positive_activities, cp_var
) {

  testset <-
    data[ ,activity_var] %>%
    {. %in% included_activities} %>%
    data[., ]

  testset$ROCindicator <-
    testset[ ,activity_var] %>%
    {. %in% positive_activities} %>%
    ifelse(0, 1)

  pROC::roc(
    ROCindicator ~ eval(parse(text = cp_var)),
    testset,
    quiet = TRUE
  )

}
