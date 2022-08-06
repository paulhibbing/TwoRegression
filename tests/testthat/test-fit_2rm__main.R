set.seed(307)

data(all_data, package = "TwoRegression")
fake_sed <- c("Lying", "Sitting")
fake_lpa <- c("Sweeping", "Dusting")
fake_cwr <- c("Walking", "Running")
fake_ila <- c("Tennis", "Basketball")

fake_activities <- c(fake_sed, fake_lpa, fake_cwr, fake_ila)

all_data$Activity <- sample(fake_activities, nrow(all_data), TRUE)

all_data$fake_METs <-
  ifelse(all_data$Activity %in% c(fake_sed, fake_lpa),
         runif(nrow(all_data), 1, 2),
         runif(nrow(all_data), 2.5, 8)
  )


test_that("fit_2rm works properly", {

  save_2rm <- function(code) {
    path <- tempfile(fileext = ".rds")
    saveRDS(code, path)
    path
  }

  testthat::expect_snapshot_file(
    save_2rm(
      fit_2rm(
        data = all_data,
        activity_var = "Activity",
        sed_cp_activities = c(fake_sed, fake_lpa),
        sed_activities = fake_sed,
        sed_cp_var = "ENMO",
        sed_METs = 1.25,
        walkrun_activities = fake_cwr,
        walkrun_cp_var = "ENMO_CV10s",
        met_var = "fake_METs",
        walkrun_formula = "fake_METs ~ ENMO",
        intermittent_formula = "fake_METs ~ ENMO + I(ENMO^2) + I(ENMO^3)"
      )
    ),
    "2rm.rds"
  )

})
