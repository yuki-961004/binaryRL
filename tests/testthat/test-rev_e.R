# TD
testthat::test_that("rev_e() works with valid input", {
  filepath <- testthat::test_path("testdata", "result_comparison.csv")
  comparison <- utils::read.csv(filepath)
  
  review <- binaryRL::rev_e(
    data = binaryRL::Mason_2024_Exp2,
    result = comparison, 
    model = binaryRL::TD,
    model_name = "TD", 
    param_prefix = "param_",
    n_trials = 360
  )
  
  testthat::expect_type(review, "list")
})

# RSTD
testthat::test_that("rev_e() works with valid input", {
  filepath <- testthat::test_path("testdata", "result_comparison.csv")
  comparison <- utils::read.csv(filepath)
  
  review <- binaryRL::rev_e(
    data = binaryRL::Mason_2024_Exp2,
    result = comparison, 
    model = binaryRL::RSTD,
    model_name = "RSTD", 
    param_prefix = "param_",
    n_trials = 360
  )
  
  testthat::expect_type(review, "list")
})

# Utility
testthat::test_that("rev_e() works with valid input", {
  filepath <- testthat::test_path("testdata", "result_comparison.csv")
  comparison <- utils::read.csv(filepath)
  
  review <- binaryRL::rev_e(
    data = binaryRL::Mason_2024_Exp2,
    result = comparison, 
    model = binaryRL::Utility,
    model_name = "Utility", 
    param_prefix = "param_",
    n_trials = 360
  )
  
  testthat::expect_type(review, "list")
})
