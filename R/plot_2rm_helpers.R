# Sedentary Cut Point Plot ------------------------------------------------

#' Plot behaviors for visualizing a sedentary cut-point
#'
#' @param object TwoRegression object to plot
#' @param sed_cp_activities activities to include in a subset of \code{AG}
#' @inheritParams plot.TwoRegression
#' @param ... additional arguments passed to \code{\link{plotCP}}
#'
#' @keywords internal
#'
plot_sed <- function(
  object, sed_cp_activities, sed_activities,
  activity_var, sed_cpVar, met_var, ...
){

  AG <- object$all_data
  AG <- AG[AG[ ,activity_var] %in% sed_cp_activities, ]
  AG$Class <- factor(
    AG[ ,activity_var] %in% sed_activities,
    levels = c("TRUE", "FALSE"),
    labels = c('Sedentary','Non-Sedentary'))

  fig <-
    ggplot(
      AG,
      aes(
        eval(parse(text = sed_cpVar)),
        eval(parse(text = met_var)),
        shape = Class
      )
    ) +
    geom_point(alpha = .8, size = 2.4) +
    .set_Theme +
    scale_shape_manual('', values = c(16,17)) +
    theme(legend.text = element_text(size = 16)) +
    scale_x_continuous(name = eval(sed_cpVar)) +
    scale_y_continuous(name = eval(met_var))

  plotCP(fig, object$sed_cutpoint, ...)

}

# Walk/Run Cut Point Plot -------------------------------------------------

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
plot_walkrun <- function(
  object, activity_var, walkrun_activities,
  walkrun_cpVar, met_var, ...
){

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
    ggplot(
      AG,
      aes(
        eval(parse(text = walkrun_cpVar)),
        eval(parse(text = met_var)),
        shape = Class
      )
    ) +
    geom_point(alpha = .8, size = 1.4) +
    scale_shape_manual('', values = c(16,17)) +
    .set_Theme +
    theme(legend.text = element_text(size = 16)) +
    scale_x_continuous(name = eval(walkrun_cpVar)) +
    scale_y_continuous(name = eval(met_var))

  plotCP(fig, object$walkrun_cutpoint, ...)

}


# Generic Cut Point Visualizing Code---------------------------------------

#' Add a cut-point to a plot
#'
#' @param fig A partially-assembled figure
#' @param cutPoint The threshold value to insert into the figure
#' @param x numeric scalar giving x coordinate for label placement
#' @param y numeric scalar giving y coordinate for label placement
#'
#' @keywords internal
#'
plotCP <- function(fig, cutPoint, x = NULL, y = NULL){

  if (is.null(x)) x <- cutPoint + 3
  if (is.null(y)) y <- 6

  label <-
    paste('Threshold =', format(cutPoint, digits = 1, nsmall = 1))

  fig +
  geom_vline(xintercept = cutPoint, linewidth = 1.3) +
  geom_label(x = x, y = y, label = label, colour = 'black', size = 4) +
  expand_limits(y = y, x = x)

}
