# initial value
testthat::test_that("run_m() works with valid input", {
  
  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    mode = "replay",
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    
    initial_value = 100,
    threshold = 1,
    epsilon = NA,
    lambda = NA,
    pi = NA,
    tau = NA
  )
  
  summary(binaryRL.res)
  
  testthat::expect_type(binaryRL.res, "list")
})

# epsilon-first
testthat::test_that("run_m() works with valid input", {
  
  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    mode = "replay",
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    
    initial_value = NA,
    threshold = 20,
    epsilon = NA,
    lambda = NA,
    pi = NA,
    tau = NA
  )
  
  summary(binaryRL.res)

  testthat::expect_type(binaryRL.res, "list")
})

# epsilon-greedy
testthat::test_that("run_m() works with valid input", {
  
  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    mode = "replay",
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    
    initial_value = NA,
    threshold = 1,
    epsilon = 0.5,
    lambda = NA,
    pi = NA,
    tau = NA
  )
  
  summary(binaryRL.res)
  
  testthat::expect_type(binaryRL.res, "list")
})

# epsilon-decreasing
testthat::test_that("run_m() works with valid input", {

  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    mode = "replay",
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    
    initial_value = NA,
    threshold = 1,
    epsilon = NA,
    lambda = 0.1,
    pi = NA,
    tau = NA
  )
  
  summary(binaryRL.res)
  
  testthat::expect_type(binaryRL.res, "list")
})

# UCB
testthat::test_that("run_m() works with valid input", {
  
  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    mode = "replay",
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    
    initial_value = NA,
    threshold = 1,
    epsilon = NA,
    lambda = NA,
    pi = 0.1,
    tau = NA
  )
  
  summary(binaryRL.res)
  
  testthat::expect_type(binaryRL.res, "list")
})

# soft-max
testthat::test_that("run_m() works with valid input", {
  
  data <- binaryRL::Mason_2024_Exp2
  
  binaryRL.res <- binaryRL::run_m(
    mode = "replay",
    data = data,
    id = 1,
    eta = c(0.321, 0.765),
    n_params = 2, 
    n_trials = 360,
    
    initial_value = NA,
    threshold = 1,
    epsilon = NA,
    lambda = NA,
    pi = NA,
    tau = 0.1
  )
  
  summary(binaryRL.res)
  
  testthat::expect_type(binaryRL.res, "list")
})