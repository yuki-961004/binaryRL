#' simulate_l
#' 
#' @description
#' 
#' This function generates simulated datasets using a
#' user-defined objective function. You can specify the number of
#' iterations to control how many datasets are generated.
#' These datasets can be used for parameter recovery and model
#' recovery. 
#' For more information, please refer to the GitHub repository:
#' https://github.com/yuki-961004/binaryRL
#' 
#' @param obj_func object function
#' 
#' @param n_params number of model free parameters
#' 
#' @param lower lower bounds of parameters
#' 
#' @param upper upper bounds of parameters
#' 
#' @param iteration iteration
#'
#' @returns list
#' @export
#'
simulate_l <- function(
    obj_func, 
    n_params, 
    lower, 
    upper,
    iteration = 10
) {
  list_simulated <- list()
  
  for (i in 1:iteration) {
    params <- c()
    
    for (j in 1:n_params) {
      set.seed(n_params * i + j) # 确保每次迭代种子都不同
      params[j] <- runif(n = 1, min = lower[j], max = upper[j])
    }
    
    list_simulated[[i]] <- obj_func(params = params)
    list_simulated[[i]]$input <- params
  }
  
  return(list_simulated)
}
