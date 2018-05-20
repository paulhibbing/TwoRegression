rm(list = ls())
load("R/sysdata.rda")

# Convert algorithms to TwoRegression objects
Algorithms <- setNames(
  lapply(seq(Algorithms),
    function(x) {
      # x <- 1
      cat("\nProcessing", names(Algorithms)[x])
      eventual_names <- paste(
        "Hibbing18",
        names(Algorithms)[x],
        c("A1", "A2", "A3"),
        sep = "_")

      a1 <-
        list(
          sed_cutpoint = Algorithms[[x]]$accelSedCut,
          cwr_cutpoint = Algorithms[[x]]$accelAmbulationCut,
          sed_variable = "ENMO",
          cwr_variable = "ENMO_CV10s",
          sed_METs = 1.25,
          cwr_model = Algorithms[[x]]$A1$CWR,
          ila_model = Algorithms[[x]]$A1$ILA
        )
      class(a1) <- "TwoRegression"

      a2 <-
        list(
          sed_cutpoint = Algorithms[[x]]$VM_gyroSedCut,
          cwr_cutpoint = Algorithms[[x]]$VM_gyroAmbulationCut,
          sed_variable = "Gyroscope_VM_DegPerS",
          cwr_variable = "GVM_CV10s",
          sed_METs = 1.25,
          cwr_model = Algorithms[[x]]$A2$CWR,
          ila_model = Algorithms[[x]]$A2$ILA
        )
      class(a2) <- "TwoRegression"


      a3 <-
        list(
          sed_cutpoint = Algorithms[[x]]$VM_gyroSedCut,
          cwr_cutpoint = Algorithms[[x]]$VM_gyroAmbulationCut,
          sed_variable = "Gyroscope_VM_DegPerS",
          cwr_variable = "GVM_CV10s",
          sed_METs = 1.25,
          cwr_model = Algorithms[[x]]$A3$CWR,
          ila_model = Algorithms[[x]]$A3$ILA
        )
      class(a3) <- "TwoRegression"

      setNames(list(a1, a2, a3), eventual_names)
    }),
  names(Algorithms)
  )

# lapply(Algorithms, list2env, envir = globalenv()
# devtools::use_data(Algorithms, overwrite = TRUE, internal = TRUE)
