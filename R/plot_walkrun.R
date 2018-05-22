#' Plot behaviors for visualizing a walk/run cut-point
#'
#' @param object the \code{TwoRegression} object to plot from
#' @param walkrun_activities character vector giving values of
#'   \code{activity_var} that are walk/run activities
#' @inheritParams plot.TwoRegression
#'
#'
#' @keywords internal
#'
plot_walkrun <- function(object, activity_var,
  walkrun_activities, walkrun_cpVar, met_var, ...){

  # walkrun_activities <- c("Run", "Walk_Slow", "Walk_Brisk")
  # walkrun_cpVar <- "ENMO_CV10s"
  # met_var <- "MET_RMR"

  AG <- object$all_data
  sedVar <- object$sed_variable
  SED_CP <- object$sed_cutpoint

  AG <- subset(AG, eval(parse(text=sedVar))>SED_CP)

  AG$Class <-
    ifelse(AG[ ,activity_var] %in% walkrun_activities,
      'Walk/Run', 'Intermittent Activity')

  AG$Class <-
    factor(AG$Class,
      levels = c('Walk/Run','Intermittent Activity'))

  fig <-
    ggplot(AG, aes(eval(parse(text = walkrun_cpVar)),
      eval(parse(text = met_var)), shape = AG$Class)) +
    geom_point(alpha = .8, size = 1.4) +
    scale_shape_manual('', values = c(16,17)) +
    .set_Theme +
    theme(legend.text = element_text(size = 16)) +
    scale_x_continuous(name = eval(walkrun_cpVar)) +
    scale_y_continuous(name = eval(met_var))

  fig <- plotCP(fig, object$walkrun_cutpoint, ...)

  return(fig)
}
