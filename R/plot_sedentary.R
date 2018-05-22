#' Plot behaviors for visualizing a sedentary cut-point
#'
#' @param object TwoRegression object to plot
#' @param sed_cp_activities activities to include in a subset of \code{AG}
#' @inheritParams plot.TwoRegression
#' @param ... additional arguments passed to \code{\link{plotCP}}
#'
#' @keywords internal
#'
plot_sed <- function(object, sed_cp_activities, sed_activities,
  activity_var, sed_cpVar, met_var, ...){


  # sed_cp_activities <- c("Internet", "Reclining",
  #   "Sweep", "Book", "Games", "Lying", "Dust")
  # sed_activities <- c("Internet", "Reclining", "Book", "Games", "Lying")
  # activity_var <- "Behavior"
  # sed_cpVar <- "ENMO"
  # met_var <- "MET_RMR"

  AG <- object$all_data
  AG <- AG[AG[ ,activity_var] %in% sed_cp_activities, ]
  AG$Class <- factor(
    AG[ ,activity_var] %in% sed_activities,
    levels = c("TRUE", "FALSE"),
    labels = c('Sedentary','Non-Sedentary'))

  fig <-
    ggplot(AG, aes(eval(parse(text = sed_cpVar)),
      eval(parse(text = met_var)),
      shape = Class)) +
    geom_point(alpha = .8, size = 2.4) +
    .set_Theme +
    scale_shape_manual('', values = c(16,17)) +
    theme(legend.text = element_text(size = 16)) +
    scale_x_continuous(name = eval(sed_cpVar)) +
    scale_y_continuous(name = eval(met_var))

  fig <- plotCP(fig, object$sed_cutpoint, ...)

  return(fig)
}
