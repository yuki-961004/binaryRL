#' Parameter and Model Recovery
#' 
#' @description
#' This function fits multiple sets of simulated data using a loop.  
#'  You need to provide a list of simulation functions, fitting functions,  
#'  and parameter bounds. If you prefer to handle the process manually,  
#'  you can use the internal functions `simulate_list` and `recovery_data`.
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
#' @param n_trials [integer] number of total trials
#'  
#' @param simulate_models [list] A collection of functions used to generate simulated data.
#' @param simulate_lower [list] The lower bounds for simulate models
#' @param simulate_upper [list] The upper bounds for simulate models
#' 
#' @param fit_models [list] A collection of functions applied to fit models to the data.
#' @param fit_lower [list] The lower bounds for model fit models
#' @param fit_upper [list] The upper bounds for model fit models
#' 
#' @param model_names [list] the names of fit modals
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
#' @param iteration_s [integer] the number of iteration in simulation (simulate)
#' 
#' @param iteration_f [integer] the number of iteration in algorithm (fit)
#' 
#' @param seed [integer] random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  default: `seed = 123` 
#'  
#' @param nc [integer] Number of CPU cores to use for parallel computation.
#'  
#' @param algorithm [character] Choose a algorithm package from 
#'  `L-BFGS-B`, `GenSA`, `GA`, `DEoptim`, `Bayesian`, `PSO`, `CMA-ES`
#'
#' @returns a list containing all recovery data
#' @export
#'
rcv_d <- function(
  data,
  id = 1,
  n_trials = 288,
  simulate_models = list(TD, RSTD, Utility),
  simulate_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
  simulate_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
  fit_models = list(TD, RSTD, Utility),
  fit_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
  fit_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
  model_names = c("TD", "RSTD", "Utility"),
  initial_params = NA,
  initial_size = 50,
  iteration_s = 10,
  iteration_f = 10,
  seed = 1,
  nc = 4,
  algorithm
){
  n_round_s <- length(simulate_models)
  n_round_f <- length(fit_models)
  
  list_recovery <- list()
  
  df_recovery <- list()
  
  for (i in 1:n_round_s){
    np <- length(simulate_lower[[i]])
    nt <- n_trials
    
    list_simulated <- simulate_list(
      data = data,
      id = id,
      obj_func = simulate_models[[i]],
      n_params = np, 
      n_trials = nt,
      lower = simulate_lower[[i]],
      upper = simulate_upper[[i]],
      seed = seed,
      iteration = iteration_s
    )
    
    names(list_simulated) <- rep(model_names[i], length(list_simulated))
    
    for (j in 1:n_round_f){
      np <- length(fit_lower[[j]])
      nt <- n_trials
      
      list_recovery[[j]] <- recovery_data(
        list = list_simulated,
        id = id,
        fit_model = fit_models[[j]],
        model_name = model_names[j],
        n_params = np, 
        n_trials = nt,
        lower = fit_lower[[j]],
        upper = fit_upper[[j]],
        initial_params = initial_params,
        initial_size = initial_size,
        iteration = iteration_f,
        nc = nc,
        algorithm = algorithm
      )
      
      list_recovery[[j]]$simulate_model <- model_names[i]
      list_recovery[[j]]$iteration <- 1:nrow(list_recovery[[j]])
    }
    
    df_recovery[[i]] <- list_recovery
  }

  result <- df_recovery
  
  return(result)
}
