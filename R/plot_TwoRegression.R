library(reshape2)
library(dplyr)
library(ggplot2)
library(gridExtra)

.set_Theme <- theme_classic() +
  theme(axis.line.x = element_line(size = .5),
  axis.line.y = element_line(size = .5),
  axis.title.x = element_text(size = 14, face = 'bold'),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 14, face = 'bold'),
  axis.text.y = element_text(size = 12),
  plot.title = element_text(size = 16, face = 'bold'),
  strip.background = element_blank(),
  strip.text.x = element_blank(),
  strip.text.y = element_text(size = 12))


#' Create summary plots for TwoRegression objects
#'
#' Four plots are generated: a threshold plot for both cut-points, and a model
#' plot for both regression models
#'
#' @param object the TwoRegression object
#' @inheritParams form_2rm
#' @param sed_cpVar character scalar. The name of the variable on which the
#'   cut-point is based
#' @param MET_var character scalar. The name of the variable containing energy
#'   expenditure values, in metabolic equivalents
#' @param x_sed numeric scalar giving x coordinate for label placement in
#'   sedentary cut-point plot
#' @param y_sed numeric scalar giving y coordinate for label placement in
#'   sedentary cut-point plot
#' @param x_walkrun numeric scalar giving x coordinate for label placement in
#'   walk/run cut-point plot
#' @param y_walkrun numeric scalar giving y coordinate for label placement in
#'   walk/run cut-point plot
#' @param ... further arguments passed to plotting calls
#' @param print logical. Should the plot be arranged in a grid? If false, the
#'   panels will be returned in a list of \code{gg/ggplot} objects.
#'
#' @return A two-by-two grid of summary plots
#' @export
#'
#' @examples
#'
plot.TwoRegression <- function(object, sed_cp_activities,
  sed_activities, sed_cpVar = NULL, activity_var, met_var,
  walkrun_activities, walkrun_cpVar,
  x_sed = NULL, y_sed = NULL,
  x_walkrun = NULL, y_walkrun = NULL, print = TRUE, ...) {

  ## Sedentary cut-point plot
  plot1 <- plot_sed(object,
    sed_cp_activities = sed_cp_activities,
    sed_activities = sed_activities,
    activity_var = activity_var,
    sed_cpVar = object$sed_variable,
    met_var = met_var,
    x = x_sed, y = y_sed, ...) +
    ggtitle("Sedentary Cut-Point")

  ## Walk/run cut-point plot
  plot2 <- plot_walkrun(object,
    activity_var = activity_var,
    walkrun_activities = walkrun_activities,
    walkrun_cpVar = object$walkrun_variable,
    met_var = met_var,
    x = x_walkrun, y = y_walkrun, ...) +
    ggtitle("Walk/Run Cut-Point")

  ## Walk/run regression plot
  smooth_formula <-
    gsub(met_var, "y",
      gsub(object$sed_variable, "x",
        object$walkrun_formula))

  plot3 <- ggplot(object$walkrun_data,
    aes(eval(parse(text = object$sed_variable)),
      eval(parse(text = met_var)))) +
    geom_point() + .set_Theme +
    geom_smooth(method = class(object$walkrun_model),
      formula = smooth_formula, se = FALSE,
      size = 1.2) +
    scale_x_continuous(name = eval(object$sed_variable)) +
    scale_y_continuous(name = eval(met_var)) +
    ggtitle("Walk/Run Model")

  ## Intermittent activity regression plot
  smooth_formula <-
    gsub(met_var, "y",
      gsub(object$sed_variable, "x",
        object$intermittent_formula))

  plot4 <- ggplot(object$intermittent_data,
    aes(eval(parse(text = object$sed_variable)),
      eval(parse(text = met_var)))) +
    geom_point() + .set_Theme +
    geom_smooth(method = class(object$intermittent_model),
      formula = smooth_formula, se = FALSE,
      size = 1.2) +
    scale_x_continuous(name = eval(object$sed_variable)) +
    scale_y_continuous(name = eval(met_var)) +
    ggtitle("Intermittent Activity Model")

  if (print) return(grid.arrange(plot1, plot2, plot3, plot4))
  return(list(sed_cut_point = plot1,
    walkrun_cut_point = plot2,
    walkrun_regression = plot3,
    intermittent_regression = plot4)
  )
}
