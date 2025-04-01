#' Title
#'
#' @param data [data.frame] raw data. 
#'  This data should include the following mandatory columns: 
#'  - "sub", "time_line", "L_choice", "R_choice", "L_reward", "R_reward". 
#'  
#' @param n_trials [integer] number of toltal trials
#'  
#' @param simulate_models [list] A collection of functions used to generate simulated data.
#' @param simulate_lower [list] The lower bounds for simulate models
#' @param simulate_upper [list] The upper bounds for simulate models
#' 
#' @param fit_models [list] A collection of functions applied to fit models to the data.
#' @param fit_lower [list] The lower bounds for model fit models
#' @param fit_upper [list] The upper bounds for model fit models
#' 
#' @param model_names [character] the name of fit modal
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
#' @param iteration_s [integer] the number of iteration in simulation
#' 
#' @param iteration_f [integer] the number of iteration in algorithm
#' 
#' @param seed [integer] random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  default: `seed = 123` 
#'  
#' @param algorithm [character] Choose a algorithm package from 
#'  `L-BFGS-B`, `GenSA`, `GA`, `DEoptim`, `Bayesian`, `PSO`, `CMA-ES`
#'
#' @returns data frame of recovery
#' @export
#'
rcv_d <- function(
  data,
  n_trials = 288,
  simulate_models = list(TD.simulate, RSTD.simulate, Utility.simulate),
  simulate_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
  simulate_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
  fit_models = list(TD.fit, RSTD.fit, Utility.fit),
  fit_lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
  fit_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
  model_names = c("TD", "RSTD", "Utility"),
  initial_params = NA,
  initial_size = 50,
  iteration_s = 10,
  iteration_f = 10,
  seed = 1,
  algorithm
){
  n_round_s <- length(simulate_models)
  n_round_f <- length(fit_models)
  
  list_recovery <- list()
  
  df_recovery <- list()
  
  for (i in 1:n_round_s){
    np <- formals(simulate_models[[i]])$n_params
    nt <- n_trials
    
    list_simulated <- simulate_list(
      data = data,
      simulate_model = simulate_models[[i]],
      n_params = np, 
      n_trials = nt,
      lower = simulate_lower[[i]],
      upper = simulate_upper[[i]],
      seed = seed,
      iteration = iteration_s
    )
    
    for (j in 1:n_round_f){
      np <- length(fit_lower[[j]])
      nt <- n_trials
      
      list_recovery[[j]] <- recovery_data(
        list = list_simulated,
        fit_model = fit_models[[j]],
        model_name = model_names[j],
        n_params = np, 
        n_trials = nt,
        lower = fit_lower[[j]],
        upper = fit_upper[[j]],
        initial_params = initial_params,
        initial_size = initial_size,
        iteration = iteration_f,
        algorithm = algorithm
      )
      
      list_recovery[[j]]$simulate_model <- model_names[i]
      list_recovery[[j]]$iteration <- 1:length(list_recovery[[j]]$iteration)
    }
    
    df_recovery[[i]] <- list_recovery
  }

  result <- df_recovery
  
  return(result)
}
