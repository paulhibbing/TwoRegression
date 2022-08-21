#' Perform leave-one-participant-out-cross-validation on a two-regression
#' algorithm
#'
#' @param subject_var character. Variable name that distinguishes between
#'   participants
#' @param data the full data set to cross-validate
#' @param model a \code{TwoRegression} object formed with \code{\link{fit_2rm}}
#'   on which to perform the cross-validation
#' @param MET_var character. The outcome variable name (in metabolic equivalents)
#' @param activity_var character. The variable name for the activity being
#'   performed
#' @param verbose logical. Print updates?
#' @param trace logical. Print information about each iteration?
#'
#' @return A data frame with predictions obtained from
#'   leave-one-participant-out-cross-validation
#'
#' @note This function will not work for \code{TwoRegression} objects formed
#'   from previously-published research. The \code{TwoRegression} object needs
#'   to have more information than is available in those cases in order to
#'   perform cross-validation, and this is sensible, since there is no reason or
#'   way to re-perform cross-validation on an already-finalized algorithm.
#'
#' @keywords internal
DualCP_LOSO <- function(
  subject_var = "id", data, model, MET_var = "MET_RMR",
  activity_var = "Behavior", verbose = FALSE, trace = FALSE
){

  timer <- PAutilities::manage_procedure(
    "Start", "\nBeginning leave-one-out cross-validation",
    verbose = verbose
  )

  subject_var %>%
  data[ ,.] %>%
  unique(.) %>%
  lapply(
    fold,
    subject_var = subject_var,
    data = data,
    model = model,
    MET_var = MET_var,
    activity_var = activity_var,
    trace = trace
  ) %>%
  do.call(rbind, .) %T>%
  {PAutilities::manage_procedure(
    "End", timer = timer, verbose = verbose
  )}

}

#' @keywords internal
#' @param x character. The id to hold out
#' @rdname DualCP_LOSO
fold <- function(
  x, subject_var, data, model,
  MET_var, activity_var, trace
){

  cv_data <-
    data[ ,subject_var] %>%
    {. == x} %>%
    data[., ]

  fold_data <-
    data[ ,subject_var] %>%
    {. != x} %>%
    data[., ]

  if(trace) {
    cat('   Cross-validating on subject', x, '\n')
    cat('\tTest values:',nrow(cv_data),'\n')
    cat('\tTrain values:',nrow(fold_data), '\n')
  }

  fold_data$classification <- get_classifications(
    fold_data, model, FALSE
  )

  predictions <- get_cv_predictions(model, fold_data, cv_data)

  category <-
    c("Sedentary", "Ambulation", "Lifestyle") %>%
    get_classifications(cv_data, model, FALSE, .)

  Category <- paste(category, cv_data$Activity)

  data.frame(
    id = cv_data[ ,subject_var],
    Activity = cv_data[ ,activity_var],
    SedVar = cv_data[ ,model$sed_variable],
    AmbVar = cv_data[ ,model$walkrun_variable],
    Actual = cv_data[ ,MET_var],
    Predicted = predictions
  ) %>%
  cbind(., Error = .$Predicted - .$Actual) %>%
  cbind(., AbsPercErr = abs(.$Error/.$Actual) * 100) %>%
  data.frame(
    category = category,
    Category = Category,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

}

#' @keywords internal
#' @param fold_data the validation data set
#' @param cv_data the holdout (i.e., cross-validation) data set
#' @rdname DualCP_LOSO
get_cv_predictions <- function(model, fold_data, cv_data) {

  cvmodel1 <-
    model$walkrun_formula %>%
    get_fold_model(fold_data, "walkrun")

  cvmodel2 <-
    model$intermittent_formula %>%
    get_fold_model(fold_data, "intermittent")

  classifications <- get_classifications(cv_data, model)

  seq(classifications) %>%
  sapply(function(i) switch(
    classifications[i],
    model$sed_METs,
    predict(cvmodel1, cv_data[i, ]),
    predict(cvmodel2, cv_data[i, ])
  ))

}

#' @keywords internal
#' @param formula_string character. Formula to apply in call to \code{lm}
#' @param level character. Classification subset to include in call to \code{lm}
#' @rdname DualCP_LOSO
get_fold_model <- function(
  formula_string, fold_data, level = c("walkrun", "intermittent")
) {

  level <- match.arg(level)

  fold_data <-
    (fold_data$classification == level) %>%
    fold_data[., ]

  paste0(
    "lm(",
    formula_string, ",",
    "data = fold_data",
    ")"
  ) %>%
  {eval(parse(text = .))}

}

#' @keywords internal
#' @rdname DualCP_LOSO
get_classifications <- function(
  data, model, numeric = TRUE,
  labels = c("SB", "walkrun", "intermittent")
) {

  is_sb <-
    model$sed_variable %>%
    data[ ,.] %>%
    {. <= model$sed_cutpoint} %>%
    ifelse(1, 0)

  is_walkrun <-
    model$walkrun_variable %>%
    data[ ,.] %>%
    {. <= model$walkrun_cutpoint & !is_sb} %>%
    ifelse(2, 0)

  classifications <-
    cbind(is_sb, is_walkrun) %>%
    apply(1, sum) %>%
    ifelse(. == 0, 3, .)

  if (numeric) return(classifications)

  labels[classifications]

}
