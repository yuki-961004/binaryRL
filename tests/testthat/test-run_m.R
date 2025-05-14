testthat::test_that("run_m() works with valid input", {
  data <- binaryRL::Mason_2024_Exp2
  
  res <- binaryRL::run_m(
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360
  )
  
  summary(res)
})
