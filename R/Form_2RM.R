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
#' @param cwr_activities Character vector. Actual ambulatory activities
#' @param cwr_cp_var Character scalar. Name of the variable on which the
#'   walk/run cut-point is defined
#' @param met_var Character scalar. Name of the variable giving actual energy
#'   expenditure (in metabolic equivalents)
#' @param cwr_formula Character scalar. Formula to use for developing the
#'   walk/run regression model
#' @param ila_formula Character scalar. Formula to use for developing the
#'   intermittent activity regression model
#'
#' @return An object of class `TwoRegression`
#' @export
#'
#' @seealso \code{\link{predict.TwoRegression}}
#'
#' @examples
#' data(ag_metabolic_s1, package = "FLPAYr")
#' test_data <- subset(ag_metabolic_s1, site == "hip")
#' form_2rm(
#' data = test_data,
#' activity_var = "Behavior",
#' sed_cp_activities = c("Internet", "Reclining",
#' "Sweep", "Book", "Games", "Lying", "Dust"),
#' sed_activities = c("Internet", "Reclining", "Book", "Games", "Lying"),
#' sed_cp_var = "ENMO",
#' sed_METs = 1.25,
#' cwr_activities = c("Run", "Walk_Slow", "Walk_Brisk"),
#' cwr_cp_var = "ENMO_CV10s",
#' met_var = "MET_RMR",
#' cwr_formula = "MET_RMR ~ ENMO",
#' ila_formula = "MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3)"
#' )
form_2rm <- function(data, activity_var, sed_cp_activities, sed_activities, sed_cp_var, sed_METs, cwr_activities, cwr_cp_var, met_var, cwr_formula, ila_formula) {

  # data(ag_metabolic_s1, package = "FLPAYr")
  # data <- subset(ag_metabolic_s1, site == "hip")
  # activity_var <- "Behavior"
  # sed_cp_activities <- c("Internet", "Reclining",
  #   "Sweep", "Book", "Games", "Lying", "Dust")
  # sed_activities <- c("Internet", "Reclining", "Book", "Games", "Lying")
  # sed_cp_var <- "ENMO"
  # sed_METs <- 1.25
  # cwr_activities <- c("Run", "Walk_Slow", "Walk_Brisk")
  # cwr_cp_var <- "ENMO_CV10s"
  # met_var <- "MET_RMR"
  # cwr_formula <- "MET_RMR ~ ENMO"
  # ila_formula <- "MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3)"

  # SB Cut-Point
  roc_sb <-
    get_cut_point(data,
      activity_var,
      sed_cp_activities,
      sed_activities,
      sed_cp_var)
        ##AUC as.numeric(roc_sedentary$auc)
        ##Plot plot(roc_sedentary)
        ##Diagnostics pROC::coords(roc_sedentary, "best", best.method = "closest.topleft")
  sb_cp <- round(
    pROC::coords(roc_sb, "best",
      best.method = 'closest.topleft')[1],
    2)

  # CWR Cut-Point

  roc_cwr <-
    get_cut_point(data[data[ ,sed_cp_var] > sb_cp, ],
      activity_var,
      unique(data[ ,activity_var]),
      cwr_activities,
      cwr_cp_var)

  cwr_cp <- round(
    pROC::coords(roc_cwr, "best",
      best.method = 'closest.topleft')[1],
    2)

  # CWR Model

  is_cwr_data <-
    (data[ ,sed_cp_var] > sb_cp) & (data[ ,cwr_cp_var] <= cwr_cp)
  cwr_data <- data[is_cwr_data, ]
  cwr_model <- lm(cwr_formula, data = cwr_data)

  # ILA Model

  is_ila_data <-
    (data[ ,sed_cp_var] > sb_cp) & (data[ ,cwr_cp] > cwr_cp)
  ila_data <- data[is_ila_data, ]
  ila_model <- lm(ila_formula, data = ila_data)

  # Form object

  final_algorithm <- list(
    sed_cutpoint = sb_cp,
    cwr_cutpoint = cwr_cp,
    sed_variable = sed_cp_var,
    cwr_variable = cwr_cp_var,
    sed_METs = sed_METs,
    cwr_model = cwr_model,
    ila_model = ila_model,
    cwr_formula = cwr_formula,
    ila_formula = ila_formula
  )
  class(final_algorithm) <- "TwoRegression"

  return(final_algorithm)
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
get_cut_point <- function(data, activity_var,
  included_activities, positive_activities, cp_var) {

  # included_activities <- sed_cp_activities
  # positive_activities <- c("Internet", "Reclining", "Book", "Games", "Lying")
  # activity_var <- "Behavior"
  # cp_var <- "ENMO"

  testset <-
    data[data[ ,activity_var]%in%included_activities, ]

  testset$ROCindicator <-
    ifelse(testset[ ,activity_var]%in%positive_activities, 0, 1)

  roc_model <-
    pROC::roc(ROCindicator ~ eval(parse(text = cp_var)), testset)


  return(roc_model)
}
