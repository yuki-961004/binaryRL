# TD
testthat::test_that("rev_e() works with valid input", {
  
  filepath <- testthat::test_path("testdata", "result_comparison.csv")
  comparison <- utils::read.csv(filepath)
  data <- subset(binaryRL::Mason_2024_G2, Subject %in% 1:10)
  
  replay <- binaryRL::rpl_e(
    data = data,
    result = comparison, 
    model = binaryRL::TD,
    model_name = "TD", 
    param_prefix = "param_",
  )
  
  testthat::expect_type(replay, "list")
})

# RSTD
testthat::test_that("rev_e() works with valid input", {
  
  filepath <- testthat::test_path("testdata", "result_comparison.csv")
  comparison <- utils::read.csv(filepath)
  data <- subset(binaryRL::Mason_2024_G2, Subject %in% 1:10)
  
  replay <- binaryRL::rpl_e(
    data = data,
    result = comparison, 
    model = binaryRL::RSTD,
    model_name = "RSTD", 
    param_prefix = "param_",
  )
  
  testthat::expect_type(replay, "list")
})

# Utility
testthat::test_that("rev_e() works with valid input", {
  
  filepath <- testthat::test_path("testdata", "result_comparison.csv")
  comparison <- utils::read.csv(filepath)
  data <- subset(binaryRL::Mason_2024_G2, Subject %in% 1:10)
  
  replay <- binaryRL::rpl_e(
    data = data,
    result = comparison, 
    model = binaryRL::Utility,
    model_name = "Utility", 
    param_prefix = "param_",
  )
  
  testthat::expect_type(replay, "list")
})
