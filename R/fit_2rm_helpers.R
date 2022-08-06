#' Convert a linear model to a text representation of the prediction equation
#'
#' @param model The linear model object
#'
#' @keywords internal
#'
get_lm_formula <- function(model) {

  if (is.na(model$coefficients["(Intercept)"]) |
      is.null(model$coefficients["(Intercept)"])) {
    model$coefficients["(Intercept)"] <-
      "(No Intercept)"
  }

  intercept <- model$coefficients["(Intercept)"]

  coeffs <- model$coefficients[names(model$coefficients) != "(Intercept)"]
  coeffs <-
    paste(sapply(coeffs, function(x) switch(sign(x) + 2, "-", "+", "+")),
      format(abs(coeffs), digits = 3, nsmall = 3, scientific = 5),
      paste("(", names(coeffs), ")", sep = "")
    )

  paste(
    format(intercept, digits = 3, nsmall = 3, scientific = 5),
    paste(coeffs, collapse = " ")
  )

}

#' Convert an object of class TwoRegression to a textual representation of the
#' algorithm
#'
#' @param object the TwoRegression object
#'
#' @keywords internal
#'
get_2rm_formula <- function(object) {

  step_1 <- paste(
    "If ", object$sed_variable, " <= ",
    object$sed_cutpoint, ": METS = ",
    object$sed_METs,
    sep = ""
  )

  step_2 <- paste(
    "If ", object$sed_variable, " > ",
    object$sed_cutpoint, " AND ",
    object$walkrun_variable, " <= ",
    object$walkrun_cutpoint,
    ": METs = ",
    get_lm_formula(object$walkrun_model),
    sep = ""
  )

  step_3 <- paste(
    "If ", object$sed_variable, " > ",
    object$sed_cutpoint, " AND ",
    object$walkrun_variable, " > ",
    object$walkrun_cutpoint,
    ": METs = ",
    get_lm_formula(object$intermittent_model),
    sep = ""
  )

  c(step_1, step_2, step_3)

}
