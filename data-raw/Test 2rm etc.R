rm(list = ls())
data(ag_metabolic_s1, package = "FLPAYr")
test_data <- subset(ag_metabolic_s1, site == "hip")

test_object <- form_2rm(
  data = test_data,
  activity_var = "Behavior",
  sed_cp_activities = c("Internet", "Reclining",
    "Sweep", "Book", "Games", "Lying", "Dust"),
  sed_activities = c("Internet", "Reclining", "Book", "Games", "Lying"),
  sed_cp_var = "ENMO",
  sed_METs = 1.25,
  cwr_activities = c("Run", "Walk_Slow", "Walk_Brisk"),
  cwr_cp_var = "ENMO_CV10s",
  met_var = "MET_RMR",
  cwr_formula = "MET_RMR ~ ENMO",
  ila_formula = "MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3)"
  )


# model1 = test_object$cwr_model
# model2 = test_object$ila_model
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
  model = test_object,
  subject_var = "id",
  data = test_data,
  activity_var = "Behavior",
  verbose = TRUE,
  trace = TRUE
  )

with(test_loso, sqrt(mean(Error^2)))
with(test_loso, mean(abs(Error)/Actual)) * 100
