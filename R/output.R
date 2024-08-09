#' summary ga_result
#'
#' @param ga_result ga_result
#' @param obj_func obj_func in your global environment
#' @param n_trials number of trials in your experiment
#' @param params_name name of your parameters
#'
#' @return output
#' @export
#'
output <- function(
    ga_result, 
    obj_func,
    n_trials,
    params_name
){
  n_params <- ncol(ga_result@solution)
  n_trials <- n_trials
  Log_Likelihood <- ga_result@fitnessValue
  
  params <- as.vector(ga_result@solution)
  acc <- capture.output(obj_func(params = params))[1]
  
  cat("Number of Parameters:", n_params, "\n")
  cat("Number of Trials:", n_trials, "\n", "\n")
  
  cat(acc, "\n")
  cat("Log-Likelihood:", Log_Likelihood, "\n")
  cat("AIC:", 2 * n_params - 2 * Log_Likelihood, "\n")
  cat("BIC:", n_params * log(n_trials) - 2 * Log_Likelihood, "\n", "\n")
  
  for (i in 1:ncol(ga_result@solution)) {
    cat(params_name[i], ga_result@solution[1,i], "\n")
  }
  
}