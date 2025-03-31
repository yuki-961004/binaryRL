#' TD model for simulate
#'
#' @param params free parameters
#' @param data raw data
#' @param n_trials number of total trials
#' @param n_params number of total params
#'
#' @returns binaryRL_res
#' @export
#'

TD.simulate <- function(params, data, n_params = 2, n_trials = 288){
  
  data <- data
  
  res <- binaryRL::run_m(
    back = TRUE, 
    data = data,                   
    id = 1,                        
    eta = c(params[1]), 
    tau = c(params[2]),
    n_params = n_params,                   
    n_trials = n_trials                  
  )

  return(res)
}
