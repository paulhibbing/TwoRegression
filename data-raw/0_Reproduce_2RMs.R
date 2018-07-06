rm(list = ls())
source("data-raw/internal_add.R")

# Crouter 2006 ####

crouter06 <-
  list(
    sed_cutpoint = 50,
    cwr_cutpoint = 10,
    sed_variable = "Axis1",
    cwr_variable = "cv_10",
    sed_METs = 1,
    cwr_model = function(cpm) 2.379833 * exp(0.00013529 * cpm),
    ila_model = function(cpm) 2.330519 + (0.001646*cpm) -
      (1.2017e-7*(cpm^2)) + (3.3779*10e-12*(cpm^3)),
    CV_zero_cwr = FALSE,
    cwr_eq_vars = "Axis1",
    ila_eq_vars = "Axis1",
    method = "Crouter 2006"
  )
class(crouter06) <- c("TwoRegression", "repro_TwoRegression")

# Crouter 2010 ####
crouter10 <-
  list(
    sed_cutpoint = 8,
    cwr_cutpoint = 10,
    sed_variable = "Axis1",
    cwr_variable = "cv_10",
    sed_METs = 1,
    cwr_model = function(cp10) 2.294275 * exp(0.00084679 * cp10),
    ila_model = function(cp10) 0.749395 + (0.716431 * log(cp10)) -
      (0.179874 * log((cp10 ^ 2))) +
      (0.033173 * log((cp10 ^ 3))),
    CV_zero_cwr = TRUE,
    cwr_eq_vars = "Axis1",
    ila_eq_vars = "Axis1",
    method = "Crouter 2010"
  )
class(crouter10) <- c("TwoRegression", "repro_TwoRegression")

# Crouter 2012 VA ####
crouter12_VA <-
  list(
    sed_cutpoint = 25,
    cwr_cutpoint = 35,
    sed_variable = "Axis1",
    cwr_variable = "cv_10",
    sed_METs = 1,
    cwr_model = function(cp10) 1.982 * (exp(0.00101 * cp10)),
    ila_model = function(cp10) 2.842 + (0.00288 * cp10),
    CV_zero_cwr = TRUE,
    cwr_eq_vars = "Axis1",
    ila_eq_vars = "Axis1",
    method = "Crouter 2012 VA"
  )
class(crouter12_VA) <- c("TwoRegression", "repro_TwoRegression")

# Crouter 2012 VM ####
crouter12_VM <-
  list(
    sed_cutpoint = 75,
    cwr_cutpoint = 25,
    sed_variable = "Vector.Magnitude",
    cwr_variable = "cv_10",
    sed_METs = 1,
    cwr_model = function(cp10) 0.0137 * exp(0.848 * (log(cp10))),
    ila_model = function(cp10) 1.219 -
      (0.145 * (log(cp10))) -
      (0.0586 * (log(cp10))^2) +
      (0.0229 * (log(cp10))^3),
    CV_zero_cwr = TRUE,
    cwr_eq_vars = "Vector.Magnitude",
    ila_eq_vars = "Vector.Magnitude",
    method = "Crouter 2012 VM"
  )
class(crouter12_VM) <- c("TwoRegression", "repro_TwoRegression")

# Save ####
internal_add(crouter06)
internal_add(crouter10)
internal_add(crouter12_VA)
internal_add(crouter12_VM)
