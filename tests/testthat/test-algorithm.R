# GenSA
testthat::test_that("GenSA() works with valid input", {
  binaryRL.res <- binaryRL::optimize_para(
    data = binaryRL::Mason_2024_Exp2,
    id = 1,
    obj_func = binaryRL::RSTD,
    n_params = 3,
    n_trials = 360,
    lower = c(0, 0, 0),
    upper = c(1, 1, 1),
    iteration = 5,
    seed = 123,
    algorithm = "GenSA"     # Simulated Annealing (GenSA::GenSA)
  )
  
  testthat::expect_type(binaryRL.res, "list")
})

# GA
testthat::test_that("GA() works with valid input", {
  binaryRL.res <- binaryRL::optimize_para(
    data = binaryRL::Mason_2024_Exp2,
    id = 1,
    obj_func = binaryRL::RSTD,
    n_params = 3,
    n_trials = 360,
    lower = c(0, 0, 0),
    upper = c(1, 1, 1),
    iteration = 5,
    seed = 123,
    algorithm = "GA"        # Genetic Algorithm (GA::ga)
  )
  
  testthat::expect_type(binaryRL.res, "list")
})

# DEoptim
testthat::test_that("DEoptim() works with valid input", {
  binaryRL.res <- binaryRL::optimize_para(
    data = binaryRL::Mason_2024_Exp2,
    id = 1,
    obj_func = binaryRL::RSTD,
    n_params = 3,
    n_trials = 360,
    lower = c(0, 0, 0),
    upper = c(1, 1, 1),
    iteration = 5,
    seed = 123,
    algorithm = "DEoptim"   # Differential Evolution (DEoptim::DEoptim)
  )
  
  testthat::expect_type(binaryRL.res, "list")
})

# PSO
testthat::test_that("PSO() works with valid input", {
  binaryRL.res <- binaryRL::optimize_para(
    data = binaryRL::Mason_2024_Exp2,
    id = 1,
    obj_func = binaryRL::RSTD,
    n_params = 3,
    n_trials = 360,
    lower = c(0, 0, 0),
    upper = c(1, 1, 1),
    iteration = 5,
    seed = 123,
    algorithm = "PSO"       # Particle Swarm Optimization (pso::psoptim)
  )
  
  testthat::expect_type(binaryRL.res, "list")
})

# Bayesian
testthat::test_that("Bayesian() works with valid input", {
  binaryRL.res <- binaryRL::optimize_para(
    data = binaryRL::Mason_2024_Exp2,
    id = 1,
    obj_func = binaryRL::RSTD,
    n_params = 3,
    n_trials = 360,
    lower = c(0, 0, 0),
    upper = c(1, 1, 1),
    iteration = 10,
    seed = 123,
    algorithm = "Bayesian"  # Bayesian Optimization (mlrMBO::mbo)
  )
  
  testthat::expect_type(binaryRL.res, "list")
})

# CMA-ES
testthat::test_that("CMA-ES() works with valid input", {
  binaryRL.res <- binaryRL::optimize_para(
    data = binaryRL::Mason_2024_Exp2,
    id = 1,
    obj_func = binaryRL::RSTD,
    n_params = 3,
    n_trials = 360,
    lower = c(0, 0, 0),
    upper = c(1, 1, 1),
    iteration = 5,
    seed = 123,
    algorithm = "CMA-ES"    # Covariance Matrix Adapting (`cmaes::cma_es`)
  )
  
  testthat::expect_type(binaryRL.res, "list")
})

# NLopt
testthat::test_that("NLopt() works with valid input", {
  binaryRL.res <- binaryRL::optimize_para(
    data = binaryRL::Mason_2024_Exp2,
    id = 1,
    obj_func = binaryRL::RSTD,
    n_params = 3,
    n_trials = 360,
    lower = c(0, 0, 0),
    upper = c(1, 1, 1),
    iteration = 10,
    seed = 123,
    algorithm = "NLOPT_GN_MLSL" # Nonlinear Optimization (nloptr::nloptr)
  )
  
  testthat::expect_type(binaryRL.res, "list")
})