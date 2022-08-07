crouter_input_check <- function(AG, movement_var, time_var) {

  AG %T>%
  {stopifnot(
    movement_var %in% names(.),
    time_var %in% names(.),
    epoch_length_sec(. ,time_var) == 10
  )}

}

crouter_general_form <- function(
    AG, movement_var = "Axis1",
    time_var = "Timestamp", model
) {

  crouter_input_check(AG, movement_var, time_var) %>%
  dplyr::rename(
    !!as.name(model$sed_variable) := !!as.name(movement_var)
  ) %>%
  dplyr::mutate(
    cv_10 = cv_2rm(!!as.name(model$sed_variable), 6, "sliding")
  ) %>%
  predict(model, .) %>%
  ag_smooth(time_var) %>%
  dplyr::rename(
    !!as.name(movement_var) := !!as.name(model$sed_variable),
    mean_cv_10 = cv_10
  )

}
