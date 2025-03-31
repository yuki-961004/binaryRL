#' Title
#'
#' @param data [data.frame] raw data. 
#'  This data should include the following mandatory columns: 
#'  - "sub", "time_line", "L_choice", "R_choice", "L_reward", "R_reward". 
#'  
#' @param n_params [vector] number of free parameters in each model
#' 
#' @param n_trials [integer] number of toltal trials
#'  
#' @param simulate_models [list] functions of simulate models
#' 
#' @param fit_models [list] functions of fit models
#' 
#' @param model_names [charactor]the name of fit modal
#' 
#' @param recovery [vector] recovery parameter or model
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
  n_params = c(2, 3, 3),
  n_trials = 288,
  simulate_models = list(TD.simulate, RSTD.simulate, Utility.simulate),
  fit_models = list(TD.fit, RSTD.fit, Utility.fit),
  model_names = c("TD", "RSTD", "Utility"),
  recovery = c("parameter", "model"),
  initial_params = NA,
  initial_size = 50,
  iteration_s = 10,
  iteration_f = 10,
  seed = 123,
  algorithm
){
  n_round_s <- length(simulate_models)
  n_round_f <- length(fit_models)
  
  list_recovery <- list()
  
  df_recovery <- list()
  
  for (s in 1:n_round_s){
    np <- n_params[s]
    nt <- n_trials
    
    list_simulated <- binaryRL::simulate_list(
      data = data,
      simulate_model = simulate_models[[s]],
      n_params = np, 
      n_trials = nt,
      lower = rep(0, np),
      upper = rep(1, np),
      seed = seed,
      iteration = iteration_s
    )
    
    for (f in 1:n_round_f){
      np <- n_params[f]
      nt <- n_trials
      
      list_recovery[[f]] <- binaryRL::recovery_data(
        list = list_simulated,
        fit_model = fit_models[[f]],
        model_name = model_names[f],
        n_params = np, 
        n_trials = nt,
        lower = rep(0, np),
        upper = rep(1, np),
        initial_params = initial_params,
        initial_size = initial_size,
        iteration = iteration_f,
        algorithm = algorithm
      )
    }
    
    df_recovery[[s]] <- do.call(rbind, list_recovery)
    df_recovery[[s]]$simulate_model <- model_names[s]
  }

  result <- do.call(rbind, df_recovery)
  
  return(result)
}
