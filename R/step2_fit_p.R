#' Fit parameters
#' 
#' @description
#' This function optimizes free parameters of reinforcement learning 
#'  models built with the `run_m` function. After constructing a 
#'  reinforcement learning model (a function with only ONE argument, 
#'  `params`), the `fit_p` function searches for the optimal values of 
#'  these free parameters.
#'
#'  The package provides four optimization algorithms: 
#' 
#'    1. L-BFGS-B (from `stats::optim`); 
#'    2. Simulated Annealing (`GenSA::GenSA`); 
#'    3. Genetic Algorithm (`GA::ga`); 
#'    4. Differential Evolution (`DEoptim::DEoptim`); 
#'    5. Bayesian Optimization (`mlrMBO::mbo`); 
#'    6. Particle Swarm Optimization (`pso::psoptim`); 
#'    7. Covariance Matrix Adapting Evolutionary Strategy (`cmaes::cma_es`); 
#' 
#'  For more information, please refer to the GitHub repository:
#'  https://github.com/yuki-961004/binaryRL
#' 
#' @param data [data.frame] raw data. 
#'  This data should include the following mandatory columns: 
#'  - "sub", "time_line", "L_choice", "R_choice", "L_reward", "R_reward". 
#'  
#' @param id [vector] which subject is going to be analyzed.
#'  is being analyzed. The value should correspond to an entry in the "sub" 
#'  column, which must contain the subject IDs. 
#'  e.g., `id = c(1:40)`
#'  
#' @param model [list] A collection of functions applied to fit models to the data.
#' 
#' @param model_name [list] the name of fit modals
#' 
#' @param n_trials [integer] number of total trials
#' 
#' @param lower [list] The lower bounds for model fit models
#' 
#' @param upper [list] The upper bounds for model fit models
#' 
#' @param initial_params [vector] Initial values for the free parameters. 
#'  These need to be set only when using L-BFGS-B. Other algorithms 
#'  automatically generate initial values.
#'  for `L-BFGS-B`, `GenSA`, set `initial = c(0, 0, ...)`
#'  
#' @param initial_size [integer] Initial values for the free parameters. 
#'  These need to be set only when using L-BFGS-B. Other algorithms 
#'  automatically generate initial values.
#'  for `Bayesian`, `GA`, set `initial = 50`
#'  
#' @param iteration [integer] the number of iteration
#' 
#' @param seed [integer] random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  default: `seed = 123` 
#'  
#' @param algorithm [character] Choose a algorithm package from 
#'  `L-BFGS-B`, `GenSA`, `GA`, `DEoptim`, `Bayesian`, `PSO`, `CMA-ES`
#'
#' @return binaryRL results for all subjects with all models
#' @export
#'

fit_p <- function(
  data,
  id = c(1:40),
  n_trials,
  model = list(TD.fit, RSTD.fit, Utility.fit),
  model_name = c("TD", "RSTD", "Utility"),
  lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
  upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
  initial_params = NA,
  initial_size = 50,
  iteration = 10,
  seed = 123,
  algorithm
){
  model_comparison <- list()
  model_result <- list()
  
  for (i in 1:length(model)){
    
    for (j in 1:length(id)) {
      
      n_params <- length(lower[[i]])
      
      binaryRL_res <- binaryRL::optimize_para(
        data = data,
        id = id[j],
        n_params = n_params,
        n_trials = n_trials,
        obj_func = model[[i]],
        lower = lower[[i]],
        upper = upper[[i]],
        iteration = iteration,
        seed = seed,
        initial_params = initial_params,
        initial_size = initial_size,
        algorithm = algorithm 
      )
      
      model_result[[j]] <- data.frame(
        fit_model = model_name[i],
        Subject = id[j],
        ACC = binaryRL_res$acc,
        LogL = -binaryRL_res$ll,
        AIC = binaryRL_res$aic,
        BIC = binaryRL_res$bic
      )
      
      for (k in 1:n_params) {
        model_result[[j]][1, k + 6] <- binaryRL_res$output[k]
        names(model_result[[j]])[k + 6] <- paste0("param_", k)
      }
    }
    model_comparison[[i]] <- model_result
  }
  
  result <- model_comparison
  
  return(result)
}