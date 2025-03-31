#' RSTD model for fit
#'
#' @param params params
#'
#' @returns -logL
#' @export
#'

RSTD.fit <- function(params){
  
  data <- get(x = "fit_data", envir = fit_env)
  id <- get(x = "fit_id", envir = fit_env)
  n_params <- get(x = "fit_n_params", envir = fit_env)
  n_trials <- get(x = "fit_n_trials", envir = fit_env)
  
  res <- binaryRL::run_m(
    data = data,                   
    id = id,                        
    eta = c(params[1], params[2]), 
    tau = c(params[3]),
    n_params = n_params,                   
    n_trials = n_trials                  
  )
  
  assign(x = "binaryRL_res", value = res, envir = fit_env)
  
  return(-res$ll)
}
