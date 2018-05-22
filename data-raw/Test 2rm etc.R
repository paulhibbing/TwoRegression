rm(list = ls())
data(ag_metabolic_s1, package = "FLPAYr")
test_data <- subset(ag_metabolic_s1, site == "hip")

object <- form_2rm(
  data = test_data,
  activity_var = "Behavior",
  sed_cp_activities = c("Internet", "Reclining",
    "Sweep", "Book", "Games", "Lying", "Dust"),
  sed_activities = c("Internet", "Reclining", "Book", "Games", "Lying"),
  sed_cp_var = "ENMO",
  sed_METs = 1.25,
  walkrun_activities = c("Run", "Walk_Slow", "Walk_Brisk"),
  walkrun_cp_var = "ENMO_CV10s",
  met_var = "MET_RMR",
  walkrun_formula = "MET_RMR ~ ENMO",
  intermittent_formula = "MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3)"
  )

model_summary <- summary(object)
model_plot <- plot(object = object,
  sed_cp_activities = c("Internet", "Reclining",
    "Sweep", "Book", "Games", "Lying", "Dust"),
  sed_activities = c("Internet", "Reclining", "Book", "Games", "Lying"),
  activity_var = "Behavior",
  sed_cpVar = "ENMO",
  met_var = "MET_RMR",
  walkrun_activities = c("Run", "Walk_Slow", "Walk_Brisk",
  x_walkrun = 35, y_walkrun = 9),
  print = FALSE)

# model1 = test_object$cwr_model
# model2 = test_object$intermittent_model
# subject_var = "id"
# data = ag_metabolic_s1
# sed_cp = test_object$sed_cutpoint
# ambulation_cp = test_object$cwr_cutpoint
# sed_cpvar = test_object$sed_variable
# ambulation_cpvar = test_object$cwr_variable
# MET_var = "MET_RMR"
# activity_var = "Behavior"
# sed_METs = test_object$sed_METs
# verbose = TRUE
# trace = TRUE


test_loso <- DualCP_LOSO(
  model = object,
  data = test_data,
  verbose = TRUE,
  trace = TRUE
  )

with(test_loso, sqrt(mean(Error^2)))
with(test_loso, mean(abs(Error)/Actual)) * 100
