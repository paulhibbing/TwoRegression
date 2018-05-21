#' Perform leave-one-participant-out-cross-validation on a two-regression
#' algorithm
#'
#' @param model1 The continuous walk/run model
#' @param model2 The intermittent activity model
#' @param subject_var The variable that distinguishes between participants
#'   (i.e., the fold-defining variable)
#' @param data The full data set to cross-validate
#' @param sed_cp The algorithm's sedentary cut-point
#' @param ambulation_cp The algorithm's ambulation cut-point
#' @param sed_cpvar The variable on which the sedentary cut-point is based
#' @param ambulation_cpvar The variable on which the ambulation cut-point is
#'   based
#' @param MET_var The outcome variable (in metabolic equivalents)
#' @param activity_var The activity being performed
#' @param sed_METs The value (in metabolic equivalents) to assign for sedentary
#'   behaviors
#' @param verbose Logical. Print updates?
#' @param trace Logical. Print information about each iteration?
#'
#' @return A data frame with predictions obtained from
#'   leave-one-participant-out-cross-validation
#' @keywords internal
#'
DualCP_LOSO <- function(model1, model2, subject_var = "id", data,
  sed_cp = 0, ambulation_cp = 0, sed_cpvar = "ENMO",
  ambulation_cpvar = "ENMO_meancv", MET_var = "MET_RMR",
  activity_var = "Behavior", sed_METs = 1.25, verbose = FALSE,
  trace = FALSE){

  # model1 <- cwr_model
  # model2 <- ila_model
  # subject_var <- "id"
  # sed_cp <- sb_cp
  # ambulation_cp <- cwr_cp
  # sed_cpvar <- sed_cp_var
  # ambulation_cpvar <- cwr_cp_var
  # remove <- TRUE
  # sed_METs <- 1.25
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
          sed_cp,
          ambulation_cp,
          sed_cpvar,
          ambulation_cpvar)

        # Models
        cvmodel1 <-
          lm(eval(parse(text = model1$call$formula)),
            data = temp_env$data[temp_env$data$classification == "CWR",
              names(model1$model)])
        cvmodel2 <-
          lm(eval(parse(text = model2$call$formula)),
            data =
              temp_env$data[temp_env$data$classification == "ILA",
                names(model1$model)])

        # Predictions
        Predicted <-
          with(cvdata, ifelse(eval(parse(text=sed_cpvar))<=sed_cp,
            sed_METs,
            ifelse(eval(parse(text=ambulation_cpvar))<=ambulation_cp,
              predict(cvmodel1, newdata = cvdata),
              predict(cvmodel2, newdata = cvdata))
          )
          )
        return(within(data.frame(id = cvdata$id,
          Activity = cvdata[ ,activity_var],
          SedVar = cvdata[,sed_cpvar],
          AmbVar = cvdata[,ambulation_cpvar],
          Actual = cvdata[ ,MET_var], Predicted = Predicted),
          {Error = Predicted - Actual
          AbsPercErr = abs(Error/Actual)*100}))
      }))

  LOSO_Data$category = ifelse(LOSO_Data$SedVar<=sed_cp, 'Sedentary',
    ifelse(LOSO_Data$AmbVar<=ambulation_cp, 'Ambulation', 'Lifestyle'))
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
    ifelse(data[ ,sed_cp_var] <= sed_cp, "SB", data$classification)
  data$classification <-
    ifelse((data[ ,sed_cp_var] > sed_cp) &
        (data[ ,cwr_cp_var] <= cwr_cp), "CWR", data$classification)
  data$classification <-
    ifelse((data[ ,sed_cp_var] > sed_cp) &
        (data[ ,cwr_cp_var] > cwr_cp), "ILA", data$classification)
  stopifnot(!any(is.na(data$classification)))
  return(data)
}
