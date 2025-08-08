# NLOPT (nc = 1)
testthat::test_that("fit_p() works with valid input", {
  
  comparison <- binaryRL::fit_p(
    estimate = "MLE",
    policy = "on",
    
    data = binaryRL::Mason_2024_Exp2,
    id = c(1:2),
    
    fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    model_name = c("TD", "RSTD", "Utility"),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
    priors = NULL,
    
    iteration_i = 2,
    iteration_g = NA,
    
    nc = 1,
    algorithm = c("NLOPT_GN_MLSL", "NLOPT_LN_BOBYQA")
  )
  
  testthat::expect_type(comparison, "list")
})

# NLOPT (nc > 1)
# MAP without HB
testthat::test_that("fit_p() works with valid input", {
  
  comparison <- binaryRL::fit_p(
    estimate = "MAP",
    policy = "off",
    
    data = binaryRL::Mason_2024_Exp2,
    id = c(1:4),
    
    fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    model_name = c("TD", "RSTD", "Utility"),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
    priors = list(
      list(
        eta = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)}, 
        tau = function(x) {stats::dexp(x, rate = 1, log = TRUE)}
      ), 
      list(
        eta = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)}, 
        eta = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)}, 
        tau = function(x) {stats::dexp(x, rate = 1, log = TRUE)}
      ), 
      list(
        eta = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)}, 
        gamma = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)} , 
        tau = function(x) {stats::dexp(x, rate = 1, log = TRUE)}
      )
    ),
    
    iteration_i = 5,
    iteration_g = 0,
    
    nc = 4,
    algorithm = c("NLOPT_GN_MLSL", "NLOPT_LN_BOBYQA")
  )
  
  testthat::expect_type(comparison, "list")
})

# MAP with HB
testthat::test_that("fit_p() works with valid input", {
  
  comparison <- binaryRL::fit_p(
    estimate = "MAP",
    policy = "off",
    
    data = binaryRL::Mason_2024_Exp2,
    id = c(1:4),
    
    fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    model_name = c("TD", "RSTD", "Utility"),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
    priors = list(
      list(
        eta = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)}, 
        tau = function(x) {stats::dexp(x, rate = 1, log = TRUE)}
      ), 
      list(
        eta = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)}, 
        eta = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)}, 
        tau = function(x) {stats::dexp(x, rate = 1, log = TRUE)}
      ), 
      list(
        eta = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)}, 
        gamma = function(x) {stats::dunif(x, min = 0, max = 1, log = TRUE)} , 
        tau = function(x) {stats::dexp(x, rate = 1, log = TRUE)}
      )
    ),
    
    iteration_i = 5,
    iteration_g = 2,
    
    nc = 4,
    algorithm = c("NLOPT_GN_MLSL", "NLOPT_LN_BOBYQA")
  )
  
  testthat::expect_type(comparison, "list")
})
