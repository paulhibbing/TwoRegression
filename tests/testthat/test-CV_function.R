testthat::test_that("Revised CV function gives identical results to previous", {
  utils::data(raw_for_cv, package = "TwoRegression")
  old_CV <- get_cvPER_old(raw_for_cv$ENMO)
  new_CV <- get_CVPER(raw_for_cv$ENMO)
  testthat::expect_equal(old_CV, new_CV)
})
