# L-BFGS-B (nc = 1)
testthat::test_that("rcv_d() works with valid input", {
  
  recovery <- binaryRL::rcv_d(
    data = binaryRL::Mason_2024_Exp2,
    model_names = c("TD", "RSTD", "Utility"),
    simulate_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    simulate_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    simulate_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
    fit_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    fit_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    fit_upper = list(c(1, 5), c(1, 1, 5), c(1, 1, 5)),
    priors = list(
      list(
        eta = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        tau = function(x) { stats::dexp(x, rate = 1, log = TRUE) }
      ), 
      list(
        eta = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        eta = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        tau = function(x) { stats::dexp(x, rate = 1, log = TRUE) }
      ), 
      list(
        eta = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        gamma = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        tau = function(x) { stats::dexp(x, rate = 1, log = TRUE) }
      )
    ),
    estimate = "MLE",
    tolerance = 0.001,
    iteration_s = 2,
    iteration_f = 2,
    nc = 1,
    algorithm = "L-BFGS-B"
  )
  
  testthat::expect_type(recovery, "list")
})


# L-BFGS-B (nc > 1)
testthat::test_that("rcv_d() works with valid input", {
  
  recovery <- binaryRL::rcv_d(
    data = binaryRL::Mason_2024_Exp2,
    model_names = c("TD", "RSTD", "Utility"),
    simulate_models = list(binaryRL::TD),
    simulate_lower = list(c(0, 0)),
    simulate_upper = list(c(1, 1)),
    fit_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    fit_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    fit_upper = list(c(1, 5), c(1, 1, 5), c(1, 1, 5)),
    priors = list(
      list(
        eta = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        tau = function(x) { stats::dexp(x, rate = 1, log = TRUE) }
      ), 
      list(
        eta = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        eta = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        tau = function(x) { stats::dexp(x, rate = 1, log = TRUE) }
      ), 
      list(
        eta = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        gamma = function(x) { stats::dunif(x, min = 0, max = 1, log = TRUE) }, 
        tau = function(x) { stats::dexp(x, rate = 1, log = TRUE) }
      )
    ),
    estimate = "MAP",
    tolerance = 0.001,
    iteration_s = 2,
    iteration_f = 2,
    nc = 2,
    algorithm = "L-BFGS-B"
  )
  
  testthat::expect_type(recovery, "list")
})
