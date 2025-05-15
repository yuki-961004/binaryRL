testthat::test_that("run_m() works with valid input", {
  library(binaryRL)

  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    initial_value = 36,
    epsilon = 0.1
  )
  
  summary(binaryRL.res)

  testthat::expect_type(res, "list")
})
