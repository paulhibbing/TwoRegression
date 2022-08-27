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
#' @rdname TwoRegression-Function
#' @keywords internal
crouter_2006 <- function(AG, movement_var, time_var, ...) {

  crouter_input_check(AG, movement_var, time_var) %>%
  reintegrate(time_var, "60 sec") %>%
  data.frame(cv_10 = cv_2rm(AG[ ,movement_var], 6, "static")) %>%
  dplyr::rename(Axis1 = dplyr::all_of(movement_var)) %T>%
  {stopifnot(anyDuplicated(names(.)) == 0)} %>%
  predict(crouter06, ., ...) %>%
  dplyr::rename(!!as.name(movement_var) := Axis1) %T>%
  {stopifnot(anyDuplicated(names(.)) == 0)}

}


#' @rdname TwoRegression-Function
#' @keywords internal
crouter_2010 <- function(AG, movement_var, time_var, ...) {

  crouter_general_form(AG, movement_var, time_var, crouter10, ...)

}


#' @rdname TwoRegression-Function
#' @keywords internal
crouter_2012 <- function(
  AG, movement_var, time_var,
  model, check = TRUE, ...
) {

  if (missing(model)) {
    warning(
      "Setting model to ", sQuote("VA"),
      " (Crouter 2012)", call. = FALSE
    )
    model <- "VA"
  } else {
    model <- match.arg(model, c("VA", "VM"))
  }

  if (check) warning(
    "Applying Crouter's 2012 ",
    switch(model, "VA" = "'vertical axis' ", "VM" = "'vector magnitude' "),
    "2RM to data stored in the ", sQuote(movement_var), " column.",
    "\nThis warning issued for your review, to ensure the selections match.",
    "\nSet `check = FALSE` to suppress this warning.", call. = FALSE
  )

  switch(
    model,
    "VA" = crouter_general_form(AG, movement_var, time_var, crouter12_VA, ...),
    "VM" = crouter_general_form(AG, movement_var, time_var, crouter12_VM, ...),
    NULL
  )

}
