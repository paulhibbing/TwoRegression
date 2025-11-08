.set_Theme <- theme_classic() +
  theme(axis.line.x = element_line(linewidth = .5),
  axis.line.y = element_line(linewidth = .5),
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
#' @param x passed from generic function but not used in the method
#' @param object the TwoRegression object
#' @inheritParams fit_2rm
#' @param walkrun_cpVar character scalar giving the name of the variable on
#'   which the walk/run cut-point is based
#' @param sed_cpVar character scalar. The name of the variable on which the
#'   cut-point is based
#' @param met_var character scalar. The name of the variable containing energy
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
#' data(all_data, package = "TwoRegression")
#' all_data$PID <-
#'   rep(
#'     c("Test1", "Test2"),
#'     each = ceiling(nrow(all_data) / 2))[seq(nrow(all_data))]
#'
#' fake_sed <- c("Lying", "Sitting")
#' fake_lpa <- c("Sweeping", "Dusting")
#' fake_cwr <- c("Walking", "Running")
#' fake_ila <- c("Tennis", "Basketball")
#'
#' fake_activities <- c(fake_sed, fake_lpa, fake_cwr, fake_ila)
#'
#' all_data$Activity <-
#'   sample(fake_activities, nrow(all_data), TRUE)
#'
#' all_data$fake_METs <-
#'   ifelse(all_data$Activity %in% c(fake_sed, fake_lpa),
#'     runif(nrow(all_data), 1, 2),
#'     runif(nrow(all_data), 2.5, 8)
#'   )
#'
#' ex_2rm <- fit_2rm(
#'   data = all_data,
#'   activity_var = "Activity",
#'   sed_cp_activities = c(fake_sed, fake_lpa),
#'   sed_activities = fake_sed,
#'   sed_cp_var = "ENMO",
#'   sed_METs = 1.25,
#'   walkrun_activities = fake_cwr,
#'   walkrun_cp_var = "ENMO_CV10s",
#'   met_var = "fake_METs",
#'   walkrun_formula = "fake_METs ~ ENMO",
#'   intermittent_formula = "fake_METs ~ ENMO + I(ENMO^2) + I(ENMO^3)"
#' )
#'
#' model_plot_list <- plot(
#'   object = ex_2rm,
#'   sed_cp_activities = c(fake_sed, fake_lpa),
#'   sed_activities = fake_sed,
#'   sed_cpVar = "ENMO",
#'   activity_var = "Activity",
#'   met_var = "fake_METs",
#'   walkrun_activities = fake_cwr,
#'   walkrun_cpVar = "ENMO_CV10s",
#'   print = FALSE
#' )
#'
#' \donttest{
#'   print(model_plot_list$sed_cut_point)
#'   print(model_plot_list$walkrun_cut_point)
#'   print(model_plot_list$walkrun_regression)
#'   print(model_plot_list$intermittent_regression)
#'
#'   plot(
#'     object = ex_2rm,
#'     sed_cp_activities = c(fake_sed, fake_lpa),
#'     sed_activities = fake_sed,
#'     sed_cpVar = "ENMO",
#'     activity_var = "Activity",
#'     met_var = "fake_METs",
#'     walkrun_activities = fake_cwr,
#'     walkrun_cpVar = "ENMO_CV10s",
#'     print = TRUE
#'   )
#' }
plot.TwoRegression <- function(x = NULL, object = NULL, sed_cp_activities,
  sed_activities, sed_cpVar = NULL, activity_var, met_var,
  walkrun_activities, walkrun_cpVar,
  x_sed = NULL, y_sed = NULL,
  x_walkrun = NULL, y_walkrun = NULL, print = TRUE, ...) {

  if (is.null(object)) {
    stop(paste(
      "You must explicitly specify `object = ` to",
      "use the print method for TwoRegression objects."
    ))
  }

  if (any(
    c("repro_TwoRegression", "Hibbing18_TwoRegression") %in%
      class(object)
  )) {
    stop(paste(
      "No print method available for algorithms",
      "formed outside of `TwoRegression`."
    ))
  }

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

  plot3 <-
    object$walkrun_data %>%
    ggplot(aes(
      eval(parse(text = object$sed_variable)),
      eval(parse(text = met_var))
    )) +
    geom_point() +
    .set_Theme +
    geom_smooth(
      method = class(object$walkrun_model),
      formula = smooth_formula,
      se = FALSE,
      linewidth = 1.2
    ) +
    scale_x_continuous(name = eval(object$sed_variable)) +
    scale_y_continuous(name = eval(met_var)) +
    ggtitle("Walk/Run Model")

  ## Intermittent activity regression plot
  smooth_formula <-
    gsub(met_var, "y",
      gsub(object$sed_variable, "x",
        object$intermittent_formula))

  plot4 <-
    object$intermittent_data %>%
    ggplot(aes(
      eval(parse(text = object$sed_variable)),
      eval(parse(text = met_var))
    )) +
    geom_point() +
    .set_Theme +
    geom_smooth(
      method = class(object$intermittent_model),
      formula = smooth_formula, se = FALSE,
      linewidth = 1.2
    ) +
    scale_x_continuous(name = eval(object$sed_variable)) +
    scale_y_continuous(name = eval(met_var)) +
    ggtitle("Intermittent Activity Model")

  if (print) {

    gridExtra::grid.arrange(plot1, plot2, plot3, plot4)

  } else {

    list(sed_cut_point = plot1,
      walkrun_cut_point = plot2,
      walkrun_regression = plot3,
      intermittent_regression = plot4
    )

  }

}
