#' TD model for fit
#'
#' @param params [vector] algorithm packages accept only one argument
#'
#' @returns negative log likelihood
#' @export
#'

TD.fit <- function(params){
  
  data <- get(x = "fit_data", envir = fit_env)
  id <- get(x = "fit_id", envir = fit_env)
  n_params <- get(x = "fit_n_params", envir = fit_env)
  n_trials <- get(x = "fit_n_trials", envir = fit_env)
  
  res <- binaryRL::run_m(
    data = data,                   
    id = id,                        
    eta = c(params[1]), 
    tau = c(params[2]),
    n_params = n_params,                   
    n_trials = n_trials                  
  )
  
  assign(x = "binaryRL_res", value = res, envir = fit_env)
  
  return(-res$ll)
}
