library(TwoRegression)
context("Hibbing 2018 algorithm")

testthat::test_that("User algorithm mis-specifications are correctly identified", {
  testthat::expect_message(algorithm_verify(NULL, 3), "No IMU file detected, yet Algorithm is not set to 1. Setting to 1.")
  testthat::expect_message(algorithm_verify(NULL, 1:4), "No IMU file detected, yet Algorithm is not set to 1. Setting to 1.")
  testthat::expect_message(algorithm_verify(NULL, 1:4), "Only algorithms 1-3 exist. Removing selections outside that range.")
  testthat::expect_message(algorithm_verify(NULL, 4), "No valid algorithms specified. Setting Algorithm to 1.")

  testthat::expect_identical(algorithm_verify("Something", 1:3), 1:3)
})

testthat::test_that("IMU files are ignored when Algorithm = 1, unless IMU_ignore_A1 = TRUE", {
  testthat::expect_null(imu_verify(NULL, 1, TRUE))
  testthat::expect_identical(imu_verify("Test", 1, FALSE), "Test")
  testthat::expect_message(imu_verify("Test", 1, TRUE), "IMU file provided, but Algorithm 1 selected. Ignoring IMU file. Set IMU_ignore_A1 = FALSE to override.")
})

testthat::test_that("One or more valid wear locations are selected", {
  testthat::expect_identical(testthat::capture_messages(attachment_verify("Thigh")),
    c(testthat::capture_messages(message_update(25, is_message = TRUE)),
      testthat::capture_messages(message_update(26, is_message = TRUE))))

  testthat::expect_identical(suppressMessages(attachment_verify("Thigh")), "Hip")

  testthat::expect_identical(testthat::capture_messages(attachment_verify(c("Thigh", "Hip"))),
    testthat::capture_messages(message_update(25, is_message = TRUE)))

  testthat::expect_identical(suppressMessages(attachment_verify(c("Thigh", "Left Wrist"))),
    "Left Wrist")

  testthat::expect_identical(attachment_verify(c("Hip", "Left Wrist")),
    c("Hip", "Left Wrist"))
})

testthat::test_that("Master function successfully reads in data", {
  RAW <- system.file("extdata", "TestID_LW_RAW.csv", package = "TwoRegression")
  IMU <- system.file("extdata", "TestID_LW_IMU.csv", package = "TwoRegression")

  testthat::expect_true(!any(RAW=="", IMU==""))

  tmp <- tempfile()
  testthat::expect_equal_to_reference(hibbing18_twoReg_process(RAW, IMU, c("Left Wrist", "Left Ankle"), "Test", 1:3), tmp)
})
