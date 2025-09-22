# NLOPT (nc = 1)
testthat::test_that("rcv_d() works with valid input", {
  
  recovery <- binaryRL::rcv_d(
    data = binaryRL::Mason_2024_G2,
    estimate = "MLE",
    policy = "off",
    model_names = c("TD", "RSTD", "Utility"),
    simulate_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    fit_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 5), c(1, 1, 5), c(1, 1, 5)),
    iteration_s = 2,
    iteration_f = 2,
    nc = 1,
    algorithm = c("NLOPT_GN_MLSL", "NLOPT_LN_BOBYQA")
  )
  
  testthat::expect_type(recovery, "list")
})


# NLOPT (nc > 1)
testthat::test_that("rcv_d() works with valid input", {
  
  recovery <- binaryRL::rcv_d(
    data = binaryRL::Mason_2024_G2,
    estimate = "MAP",
    policy = "off",
    model_names = c("TD", "RSTD", "Utility"),
    simulate_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    rfun = list(
      list(
        eta = function() { stats::runif(n = 1, min = 0, max = 1) },
        tau = function() { stats::rexp(n = 1, rate = 1) }
      ),
      list(
        eta = function() { stats::runif(n = 1, min = 0, max = 1) },
        eta = function() { stats::runif(n = 1, min = 0, max = 1) },
        tau = function() { stats::rexp(n = 1, rate = 1) }
      ),
      list(
        eta = function() { stats::runif(n = 1, min = 0, max = 1) },
        gamma = function() { stats::runif(n = 1, min = 0, max = 1) },
        tau = function() { stats::rexp(n = 1, rate = 1) }
      )
    ),
    fit_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    dfun = list(
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
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 5), c(1, 1, 5), c(1, 1, 5)),
    iteration_s = 4,
    iteration_f = c(2, 2),
    nc = 4,
    algorithm = c("NLOPT_GN_MLSL", "NLOPT_LN_BOBYQA")
  )
  
  testthat::expect_type(recovery, "list")
})
