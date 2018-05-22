#' Perform leave-one-participant-out-cross-validation on a two-regression
#' algorithm
#'
#' @param model A \code{TwoRegression} object formed with \code{\link{form_2rm}}
#'   on which to perform the cross-validation
#' @param subject_var The variable that distinguishes between participants
#'   (i.e., the fold-defining variable)
#' @param data The full data set to cross-validate
#' @param MET_var The outcome variable (in metabolic equivalents)
#' @param activity_var The activity being performed
#' @param verbose Logical. Print updates?
#' @param trace Logical. Print information about each iteration?
#'
#' @return A data frame with predictions obtained from
#'   leave-one-participant-out-cross-validation
#'
#' @note This function will not work for \code{TwoRegression} objects formed
#'   from previously-published research. The \code{TwoRegression} object needs
#'   to have more information than is avaintermittentble in those cases in order to
#'   perform cross-validation, and this is sensible, since there is no reason or
#'   way to re-perform cross-validation on an already-finalized algorithm.
#'
#' @keywords internal
#'
DualCP_LOSO <- function(model, subject_var = "id", data,
  MET_var = "MET_RMR", activity_var = "Behavior",
  verbose = FALSE, trace = FALSE){

  # subject_var <- "id"
  # verbose <- TRUE
  # trace <- TRUE
  # MET_var <- "MET_RMR"
  # activity_var <- "Behavior"

  temp_env <- new.env()
  ids <- unique(data[ ,subject_var])
  backup_data <- data

  assign('data',backup_data,envir = temp_env)
  LOSO_Data <-
    do.call(rbind,
      lapply(ids, function(x){
        # x <- ids[1]

        # Test set
        if(trace) cat('   Cross-validating on subject', x, '\n')
        cvdata <-
          backup_data[backup_data[ ,subject_var] == x, ]

        if(trace) cat('\tTest values:',nrow(cvdata),'\n')

        # Train set
        assign('data',
          backup_data[backup_data[ ,subject_var] != x, ],
          temp_env)

        if(trace) cat('\tTrain values:',nrow(temp_env$data), '\n')

        temp_env$data <- classify_validation_data(temp_env$data,
          model$sed_cutpoint,
          model$walkrun_cutpoint,
          model$sed_variable,
          model$walkrun_variable)

        # Models
        cvmodel1 <-
          lm(eval(parse(text = model$walkrun_formula)),
            data = temp_env$data[temp_env$data$classification == "walkrun",])
        cvmodel2 <-
          lm(eval(parse(text = model$intermittent_formula)),
            data =
              temp_env$data[temp_env$data$classification == "intermittent",])

        # Predictions
        Predicted <-
          with(cvdata, ifelse(eval(parse(text=model$sed_variable))<=model$sed_cutpoint,
            model$sed_METs,
            ifelse(eval(parse(text=model$walkrun_variable))<=model$walkrun_cutpoint,
              predict(cvmodel1, newdata = cvdata),
              predict(cvmodel2, newdata = cvdata))
          )
          )

      LOSO_Data <- data.frame(id = cvdata$id,
        Activity = cvdata[ ,activity_var],
        SedVar = cvdata[,model$sed_variable],
        AmbVar = cvdata[,model$walkrun_variable],
        Actual = cvdata[ ,MET_var], Predicted = Predicted)
      LOSO_Data$Error <- LOSO_Data$Predicted - LOSO_Data$Actual
      LOSO_Data$AbsPercErr <- abs(LOSO_Data$Error/LOSO_Data$Actual)*100
    return(LOSO_Data)
    }))

  LOSO_Data$category = ifelse(LOSO_Data$SedVar<=model$sed_cutpoint, 'Sedentary',
    ifelse(LOSO_Data$AmbVar<=model$walkrun_cutpoint, 'Ambulation', 'Lifestyle'))
  LOSO_Data$Category = with(LOSO_Data, paste(category, Activity))

  return(LOSO_Data)
}


#' Establish an annotated validation data set for
#' leave-one-participant-out-cross-validation
#'
#' @param data The data set to annotate
#' @inheritParams DualCP_LOSO
#'
#' @keywords internal
#'
classify_validation_data <- function(data, sed_cp = 0, ambulation_cp = 0,
  sed_cpvar = "ENMO", ambulation_cpvar = "ENMO_meancv"){
  data$classification <- NA
  data$classification <-
    ifelse(data[ ,sed_cpvar] <= sed_cp, "SB", data$classification)
  data$classification <-
    ifelse((data[ ,sed_cpvar] > sed_cp) &
        (data[ ,ambulation_cpvar] <= ambulation_cp), "walkrun", data$classification)
  data$classification <-
    ifelse((data[ ,sed_cpvar] > sed_cp) &
        (data[ ,ambulation_cpvar] > ambulation_cp), "intermittent", data$classification)
  stopifnot(!any(is.na(data$classification)))
  return(data)
}
