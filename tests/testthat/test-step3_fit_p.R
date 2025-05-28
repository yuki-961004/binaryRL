# L-BFGS-B (nc = 1)
testthat::test_that("fit_p() works with valid input", {
  comparison <- binaryRL::fit_p(
    data = binaryRL::Mason_2024_Exp2,
    n_trials = 360,
    id = c(1:2),
    fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    model_name = c("TD", "RSTD", "Utility"),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
    iteration = 2,
    seed = 123,
    nc = 1,
    algorithm = "L-BFGS-B"
  )
  
  testthat::expect_type(comparison, "list")
})

# L-BFGS-B (nc > 1)
testthat::test_that("fit_p() works with valid input", {
  comparison <- binaryRL::fit_p(
    data = binaryRL::Mason_2024_Exp2,
    n_trials = 360,
    id = c(1:4),
    fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    model_name = c("TD", "RSTD", "Utility"),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
    iteration = 2,
    seed = 123,
    nc = 4,
    algorithm = "L-BFGS-B" 
  )
  
  testthat::expect_type(comparison, "list")
})
