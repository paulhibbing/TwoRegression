#' Apply two-regression methods from publications by Crouter et al.
#'
#' @param AG data frame containing ActiGraph data (activity counts)
#' @param movement_var character scalar. Name of the movement variable (default
#'   is \code{"Axis1"})
#' @param time_var character scalar. Name of the timestamp variable (required to
#'   verify that input epoch length is 10 seconds)
#' @param model character scalar. One of \code{"VA"} (for the vertical axis,
#'   youth-specific model of Crouter et al., 2012) or \code{"VM"} (for the
#'   vector magnitude youth-specific model from the same paper)
#' @param check Logical. Should a warning be issued that will prompt you to
#'   check that the selected model matches the selected  movement variable?
#'
#' @return The original data appended with columns giving activity
#'   classification (sedentary, ambulatory, or intermittent) and energy
#'   expenditure (i.e, METs)
#'
#' @examples
#' data(count_data, package = "TwoRegression")
#' crouter_2006(count_data, "Axis1", "time")
#' crouter_2010(count_data, "Axis1", "time")
#' crouter_2012(count_data, "Axis1", "time", "VA", FALSE)
#' crouter_2012(count_data, "Vector.Magnitude", "time", "VM", FALSE)
#'
#' @seealso
#' \href{https://pubmed.ncbi.nlm.nih.gov/16322367/}{Crouter et al. (2006, \emph{J Appl Physiol})}
#' \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2891855/}{Crouter et al. (2010, \emph{Med Sci Sports Exerc})}
#' \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3324667/}{Crouter et al. (2012, \emph{Med Sci Sports Exerc})}
#'
#' @name crouter
#' @export
crouter_2006 <- function(
  AG, movement_var = "Axis1", time_var = "Timestamp"
) {

  crouter_input_check(AG, movement_var, time_var) %>%
  reintegrate(time_var, "60 sec") %>%
  data.frame(cv_10 = cv_2rm(AG[ ,movement_var], 6, "static")) %>%
  dplyr::rename(Axis1 = dplyr::all_of(movement_var)) %T>%
  {stopifnot(anyDuplicated(names(.)) == 0)} %>%
  predict(crouter06, .) %>%
  dplyr::rename(!!as.name(movement_var) := Axis1) %T>%
  {stopifnot(anyDuplicated(names(.)) == 0)}

}


#' @rdname crouter
#' @export
crouter_2010 <- function(
  AG, movement_var = "Axis1", time_var = "Timestamp"
) {

  crouter_general_form(AG, movement_var, time_var, crouter10)

  # crouter_input_check(AG, movement_var, time_var) %>%
  # dplyr::rename(Axis1 = dplyr::all_of(movement_var)) %>%
  # dplyr::mutate(cv_10 = cv_2rm(Axis1, 6, "sliding")) %>%
  # predict(crouter10, .) %>%
  # dplyr::group_by(
  #   !!as.name(time_var) := lubridate::floor_date(
  #     !!as.name(time_var), "60 sec"
  #   )
  # ) %>%
  # dplyr::summarise(
  #   dplyr::across(
  #     where(function(x) !is.numeric(x)) & !Classification,
  #     dplyr::first
  #   ),
  #   dplyr::across(
  #     where(is.numeric) & !c(METs, cv_10),
  #     sum
  #   ),
  #   dplyr::across(Classification, function(x) data.frame(table(x))),
  #   dplyr::across(c(METs, cv_10), mean),
  #   dplyr::across(
  #     !Classification,
  #     ~ .x %T>% {stopifnot(dplyr::n_distinct(.) == 1)}
  #   ),
  #   .groups = "drop"
  # ) %>%
  # tidyr::unpack(Classification) %>%
  # tidyr::pivot_wider(
  #   names_from = x,
  #   values_from = Freq,
  #   names_glue = "{x}_epochs"
  # ) %>%
  # dplyr::relocate(!METs) %>%
  # dplyr::rename(mean_cv_10 = cv_10) %>%
  # data.frame(stringsAsFactors = FALSE)

}


#' @rdname crouter
#' @export
crouter_2012 <- function(
  AG, movement_var = "Axis1",
  time_var = "Timestamp", model = c("VA", "VM"),
  check = TRUE
) {

  model <- match.arg(model)

  if (check) warning(
    "Applying Crouter's 2012 ",
    switch(model, "VA" = "'vertical axis' ", "VM" = "'vector magnitude' "),
    "2RM to data stored in the ", sQuote(movement_var), " column.",
    "\nThis warning issued for your review, to ensure the selections match.",
    "\nSet `check = FALSE` to suppress this warning.", call. = FALSE
  )

  switch(
    model,
    "VA" = crouter_general_form(AG, movement_var, time_var, crouter12_VA),
    "VM" = crouter_general_form(AG, movement_var, time_var, crouter12_VM),
    NULL
  )

}
