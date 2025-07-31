# L-BFGS-B (nc = 1)
testthat::test_that("fit_p() works with valid input", {
  
  comparison <- binaryRL::fit_p(
    data = binaryRL::Mason_2024_Exp2,
    id = c(1:2),
    fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    model_name = c("TD", "RSTD", "Utility"),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
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
    iteration = 2,
    nc = 1,
    algorithm = "L-BFGS-B"
  )
  
  testthat::expect_type(comparison, "list")
})

# L-BFGS-B (nc > 1)
testthat::test_that("fit_p() works with valid input", {
  
  comparison <- binaryRL::fit_p(
    data = binaryRL::Mason_2024_Exp2,
    id = c(1:2),
    fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    model_name = c("TD", "RSTD", "Utility"),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
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
    iteration = 2,
    nc = 2,
    algorithm = "L-BFGS-B" 
  )
  
  testthat::expect_type(comparison, "list")
})
