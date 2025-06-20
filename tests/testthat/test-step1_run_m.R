# epsilon-first
testthat::test_that("run_m() works with valid input", {
  
  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    mode = "simulate",
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    initial_value = 36,
    
    threshold = 20,
    epsilon = NA,
    lambda = NA
  )
  
  summary(binaryRL.res)

  testthat::expect_type(binaryRL.res, "list")
})

# epsilon-greedy
testthat::test_that("run_m() works with valid input", {
  
  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    mode = "simulate",
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    initial_value = 36,
    
    threshold = 1,
    epsilon = 0.1,
    lambda = NA
  )
  
  summary(binaryRL.res)
  
  testthat::expect_type(binaryRL.res, "list")
})

# epsilon-decreasing
testthat::test_that("run_m() works with valid input", {

  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    mode = "simulate",
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    
    threshold = 1,
    epsilon = NA,
    lambda = 0.5
  )
  
  summary(binaryRL.res)
  
  testthat::expect_type(binaryRL.res, "list")
})
