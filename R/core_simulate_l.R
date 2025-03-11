#' simulate_l
#'
#' @param obj_func object function
#' @param n_params number of model free parameters
#' @param iteration iteration
#'
#' @returns list
#' @export
#'
simulate_l <- function(obj_func, n_params, iteration = 10) {
  list_simulated <- list()
  
  for (i in 1:iteration) {
    params <- c()
    
    for (j in 1:n_params) {
      set.seed(n_params * i + j) # 确保每次迭代种子都不同
      params[j] <- runif(n = 1, min = 0, max = 1)
    }
    
    list_simulated[[i]] <- obj_func(params = params)
    list_simulated[[i]]$input <- params
  }
  
  return(list_simulated)
}
