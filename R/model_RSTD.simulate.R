#' RSTD model for simulate
#'
#' @param params [vector] free parameters
#' 
#' @param data [data.frame] raw data. 
#'  This data should include the following mandatory columns: 
#'  - "sub", "time_line", "L_choice", "R_choice", "L_reward", "R_reward". 
#'  
#' @param n_params [integer] The number of free parameters in your model. 
#' 
#' @param n_trials [integer] The total number of trials in your experiment.
#'
#' @returns binaryRL_res
#' @export
#'

RSTD.simulate <- function(params, data, n_params = 3, n_trials = 288){
  
  data <- data
  
  res <- binaryRL::run_m(
    back = TRUE, 
    data = data,                   
    id = 1,                        
    eta = c(params[1], params[2]), 
    tau = c(params[3]),
    n_params = n_params,                   
    n_trials = n_trials                  
  )
  
  return(res)
}
