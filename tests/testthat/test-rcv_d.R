# L-BFGS-B (nc = 1)
testthat::test_that("rcv_d() works with valid input", {
  recovery <- binaryRL::rcv_d(
    data = binaryRL::Mason_2024_Exp2,
    id = 1,
    n_trials = 360,
    model_names = c("TD", "RSTD", "Utility"),
    simulate_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    simulate_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    simulate_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
    fit_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    fit_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    fit_upper = list(c(1, 5), c(1, 1, 5), c(1, 1, 5)),
    initial_params = NA,
    initial_size = 50,
    seed = 123,
    iteration_s = 4,
    iteration_f = 4,
    nc = 1,
    algorithm = "L-BFGS-B"  # Gradient-Based (stats::optim)
    #algorithm = "GenSA"     # Simulated Annealing (GenSA::GenSA)
    #algorithm = "GA"        # Genetic Algorithm (GA::ga)
    #algorithm = "DEoptim"   # Differential Evolution (DEoptim::DEoptim)
    #algorithm = "PSO"       # Particle Swarm Optimization (pso::psoptim)
    #algorithm = "Bayesian"  # Bayesian Optimization (mlrMBO::mbo)
    #algorithm = "CMA-ES"    # Covariance Matrix Adapting (`cmaes::cma_es`)
  )
  
  testthat::expect_type(recovery, "list")
})


# L-BFGS-B (nc > 1)
testthat::test_that("rcv_d() works with valid input", {
  recovery <- binaryRL::rcv_d(
    data = binaryRL::Mason_2024_Exp2,
    id = 1,
    n_trials = 360,
    model_names = c("TD", "RSTD", "Utility"),
    simulate_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    simulate_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    simulate_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
    fit_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    fit_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    fit_upper = list(c(1, 5), c(1, 1, 5), c(1, 1, 5)),
    initial_params = NA,
    initial_size = 50,
    seed = 123,
    iteration_s = 4,
    iteration_f = 4,
    nc = 4,
    algorithm = "L-BFGS-B"  # Gradient-Based (stats::optim)
    #algorithm = "GenSA"     # Simulated Annealing (GenSA::GenSA)
    #algorithm = "GA"        # Genetic Algorithm (GA::ga)
    #algorithm = "DEoptim"   # Differential Evolution (DEoptim::DEoptim)
    #algorithm = "PSO"       # Particle Swarm Optimization (pso::psoptim)
    #algorithm = "Bayesian"  # Bayesian Optimization (mlrMBO::mbo)
    #algorithm = "CMA-ES"    # Covariance Matrix Adapting (`cmaes::cma_es`)
  )
  
  testthat::expect_type(recovery, "list")
})
