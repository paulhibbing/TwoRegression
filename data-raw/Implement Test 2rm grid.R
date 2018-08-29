# Load data and function --------------------------------------------------

  rm(list = ls())
  devtools::load_all()
  source("data-raw/test_2rm_grid.R")
  data(ag_metabolic_s1, package = "FLPAYr")
    #^ !!CAUTION: Verify local installation is up-to-date!

# Establish grid to test --------------------------------------------------

  site <- unique(ag_metabolic_s1$site)

  subset <- list(#unique(ag_metabolic_s1$Behavior),
                 unique(ag_metabolic_s1$Behavior)[
                   !unique(ag_metabolic_s1$Behavior) %in%
                     "Cycling"])

  sed_cp_activities <- c("Internet", "Reclining",
                        "Sweep", "Book", "Games", "Lying", "Dust")

  sed_activities <- c("Internet", "Reclining", "Book", "Games", "Lying")

  sed_cp_var <- "GVM"

  sed_METs <- 1.25

  walkrun_activities <- c("Run", "Walk_Slow", "Walk_Brisk")

  walkrun_cp_var <- "GVM_CV10s"

  met_var <- "MET_RMR"

  walkrun_formula <- c("MET_RMR ~ ENMO",
                       "MET_RMR ~ log(ENMO)",
                       "MET_RMR ~ GVM",
                       "MET_RMR ~ log(GVM)",

                       "MET_RMR ~ ENMO + GVM",
                       "MET_RMR ~ log(ENMO) + GVM",
                       "MET_RMR ~ ENMO + log(GVM)",
                       "MET_RMR ~ log(ENMO) + log(GVM)",

                       "MET_RMR ~ ENMO + Age",
                       "MET_RMR ~ log(ENMO) + Age",
                       "MET_RMR ~ GVM + Age",
                       "MET_RMR ~ log(GVM + Age)",

                       "MET_RMR ~ ENMO + GVM + Age",
                       "MET_RMR ~ log(ENMO) + GVM + Age",
                       "MET_RMR ~ ENMO + log(GVM) + Age",
                       "MET_RMR ~ log(ENMO) + log(GVM) + Age"
                       )

  intermittent_formula <-
    c("MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3)",
      "MET_RMR ~ I(log(ENMO))+I(log(ENMO^2))+I(log(ENMO^3))",
      "MET_RMR ~ I(GVM)+I(GVM^2)+I(GVM^3)",
      "MET_RMR ~ I(log(GVM))+I(log(GVM^2))+I(log(GVM^3))",
      "MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3)+I(GVM)+I(GVM^2)+I(GVM^3)",
      paste("MET_RMR ~ I(log(ENMO))+I(log(ENMO^2))+I(log(ENMO^3))",
            "I(GVM)+I(GVM^2)+I(GVM^3)", sep = "+"),
      paste("MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3)",
            "I(log(GVM))+I(log(GVM^2))+I(log(GVM^3))", sep = "+"),
      paste("MET_RMR ~ I(log(ENMO))+I(log(ENMO^2))+I(log(ENMO^3))",
            "I(log(GVM))+I(log(GVM^2))+I(log(GVM^3))", sep = "+"),

      "MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3) + Age",
      "MET_RMR ~ I(log(ENMO))+I(log(ENMO^2))+I(log(ENMO^3)) + Age",
      "MET_RMR ~ I(GVM)+I(GVM^2)+I(GVM^3) + Age",
      "MET_RMR ~ I(log(GVM))+I(log(GVM^2))+I(log(GVM^3)) + Age",
      "MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3)+I(GVM)+I(GVM^2)+I(GVM^3) + Age",
      paste("MET_RMR ~ I(log(ENMO))+I(log(ENMO^2))+I(log(ENMO^3))",
            "I(GVM)+I(GVM^2)+I(GVM^3) + Age", sep = "+"),
      paste("MET_RMR ~ I(ENMO)+I(ENMO^2)+I(ENMO^3)",
            "I(log(GVM))+I(log(GVM^2))+I(log(GVM^3)) + Age", sep = "+"),
      paste("MET_RMR ~ I(log(ENMO))+I(log(ENMO^2))+I(log(ENMO^3))",
            "I(log(GVM))+I(log(GVM^2))+I(log(GVM^3)) + Age", sep = "+")
    )

  activity_var <- "Behavior"

  master_grid <- expand.grid(site = site,
              subset = subset, sed_cp_activities = list(sed_cp_activities),
              activity_var = activity_var,
              sed_activities = list(sed_activities),
              sed_cp_var = sed_cp_var, sed_METs = sed_METs,
              walkrun_activities = list(walkrun_activities),
              walkrun_cp_var = walkrun_cp_var, met_var = met_var,
              walkrun_formula = walkrun_formula,
              intermittent_formula = intermittent_formula,
              stringsAsFactors = FALSE)

  rm(list = setdiff(ls(), c(
    "ag_metabolic_s1", "master_grid", "test_2rm_grid"
  )))

# Form all the algorithms -------------------------------------------------

# test_2rm_grid(split(master_grid, seq(nrow(master_grid)))[[500]])
models <- lapply(split(master_grid, seq(nrow(master_grid))), test_2rm_grid)
results <- do.call(rbind, models)

data.table::fwrite(results,
    file = "data-raw/2RM_Results.csv", row.names = FALSE)
