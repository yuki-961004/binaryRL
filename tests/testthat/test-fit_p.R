# L-BFGS-B (nc = 1)
testthat::test_that("fit_p() works with valid input", {
  comparison <- binaryRL::fit_p(
    data = binaryRL::Mason_2024_Exp2,
    n_trials = 360,
    id = c(1:2),
    #id = unique(binaryRL::Mason_2024_Exp2$Subject),
    fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    model_name = c("TD", "RSTD", "Utility"),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
    iteration = 2,
    seed = 123,
    nc = 1,
    algorithm = "L-BFGS-B"  # Gradient-Based (stats::optim)
    #algorithm = "GenSA"     # Simulated Annealing (GenSA::GenSA)
    #algorithm = "GA"        # Genetic Algorithm (GA::ga)
    #algorithm = "DEoptim"   # Differential Evolution (DEoptim::DEoptim)
    #algorithm = "PSO"       # Particle Swarm Optimization (pso::psoptim)
    #algorithm = "Bayesian"  # Bayesian Optimization (mlrMBO::mbo)
    #algorithm = "CMA-ES"    # Covariance Matrix Adapting (`cmaes::cma_es`)
  )
  
  testthat::expect_type(comparison, "list")
})

# L-BFGS-B (nc > 1)
testthat::test_that("fit_p() works with valid input", {
  comparison <- binaryRL::fit_p(
    data = binaryRL::Mason_2024_Exp2,
    n_trials = 360,
    id = c(1:4),
    #id = unique(binaryRL::Mason_2024_Exp2$Subject),
    fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
    model_name = c("TD", "RSTD", "Utility"),
    lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
    upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
    iteration = 2,
    seed = 123,
    nc = 4,
    algorithm = "L-BFGS-B"  # Gradient-Based (stats::optim)
    #algorithm = "GenSA"     # Simulated Annealing (GenSA::GenSA)
    #algorithm = "GA"        # Genetic Algorithm (GA::ga)
    #algorithm = "DEoptim"   # Differential Evolution (DEoptim::DEoptim)
    #algorithm = "PSO"       # Particle Swarm Optimization (pso::psoptim)
    #algorithm = "Bayesian"  # Bayesian Optimization (mlrMBO::mbo)
    #algorithm = "CMA-ES"    # Covariance Matrix Adapting (`cmaes::cma_es`)
  )
  
  testthat::expect_type(comparison, "list")
})
