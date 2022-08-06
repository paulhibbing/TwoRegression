crouter_input_check <- function(AG, movement_var, time_var) {

  AG %T>%
  {stopifnot(
    movement_var %in% names(.),
    time_var %in% names(.),
    PAutilities::epoch_length_sec(.[ ,time_var]) == 10
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
  dplyr::group_by(
    !!as.name(time_var) := lubridate::floor_date(
      !!as.name(time_var), "60 sec"
    )
  ) %>%
  dplyr::summarise(
    dplyr::across(
      where(function(x) !is.numeric(x)) & !Classification,
      dplyr::first
    ),
    dplyr::across(
      where(is.numeric) & !c(METs, cv_10),
      sum
    ),
    dplyr::across(Classification, function(x) data.frame(table(x))),
    dplyr::across(c(METs, cv_10), mean),
    dplyr::across(
      !Classification,
      ~ .x %T>% {stopifnot(dplyr::n_distinct(.) == 1)}
    ),
    .groups = "drop"
  ) %>%
  tidyr::unpack(Classification) %>%
  tidyr::pivot_wider(
    names_from = x,
    values_from = Freq,
    names_glue = "{x}_epochs"
  ) %>%
  dplyr::relocate(!METs) %>%
  dplyr::rename(
    !!as.name(movement_var) := !!as.name(model$sed_variable),
    mean_cv_10 = cv_10
  ) %>%
  data.frame(stringsAsFactors = FALSE)

}
