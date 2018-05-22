test_2rm_grid <- function(x, test_data = globalenv()$ag_metabolic_s1,
                          master_grid = globalenv()$master_grid) {
  # x <- split(master_grid, seq(nrow(master_grid)))[[3]]
  # test_data <- ag_metabolic_s1

  n <- sapply(
      names(x),
      function(y) which(unlist(master_grid[ ,y]) == unlist(x[ ,y])),
      simplify = FALSE
  )
  n <- Reduce(intersect, n)
  cat("\nProcessing", n)

  test_data <- test_data[test_data[ ,"site"] %in% x$site, ]
  test_data <- test_data[test_data[ ,x$activity_var] %in% unlist(x$subset), ]

  object <- form_2rm(
    data = test_data,
    activity_var = x$activity_var,
    sed_cp_activities = unlist(x$sed_cp_activities),
    sed_activities = unlist(x$sed_activities),
    sed_cp_var = x$sed_cp_var,
    sed_METs = x$sed_METs,
    walkrun_activities = unlist(x$walkrun_activities),
    walkrun_cp_var = x$walkrun_cp_var,
    met_var = x$met_var,
    walkrun_formula = x$walkrun_formula,
    intermittent_formula = x$intermittent_formula
  )

  object_summary <- summary(object)

  data.frame(
    sed_cut_point_variable = x$sed_cp_var,
    walkrun_cut_point_variable = x$walkrun_cp_var,
    sed_cut_point = t(object_summary$cut_points[[1]]),
    walkrun_cut_point = t(object_summary$cut_points[[2]]),
    walkrun_regression = object_summary$regression_models[[1]],
    intermittent_activity_regression = object_summary$regression_models[[2]],
    leave_one_out = object_summary$leave_one_out,
    algorithm_textual = paste(object_summary$algorithm, collapse = "  ;  "),
    stringsAsFactors = FALSE
  )
}
