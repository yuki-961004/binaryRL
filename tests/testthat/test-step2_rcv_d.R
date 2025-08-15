# NLOPT (nc = 1)
testthat::test_that("rcv_d() works with valid input", {
  
  recovery <- binaryRL::rcv_d(
    data = binaryRL::Mason_2024_G2,
    model_names = c("TD", "RSTD", "Utility"),
    simulate_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    simulate_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    simulate_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
    fit_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    fit_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    fit_upper = list(c(1, 5), c(1, 1, 5), c(1, 1, 5)),
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
    model_names = c("TD", "RSTD", "Utility"),
    simulate_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    simulate_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    simulate_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
    fit_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    fit_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    fit_upper = list(c(1, 5), c(1, 1, 5), c(1, 1, 5)),
    iteration_s = 4,
    iteration_f = 5,
    nc = 4,
    algorithm = c("NLOPT_GN_MLSL", "NLOPT_LN_BOBYQA")
  )
  
  testthat::expect_type(recovery, "list")
})
