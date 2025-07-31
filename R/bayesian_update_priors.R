bayesian_update_priors <- function(
    model_result, priors, n_params, param_prefix
){
  for (k in 1:n_params) {
    
    col_name <- paste0(param_prefix, k)
    
    if (k == n_params) {
      # softmax参数的先验概率拟合为指数分布
      priors[[k]] <- function(x) { 
        stats::dexp(
          x, log = TRUE,
          rate = 1 / (mean(model_result[[col_name]]) + 1e-5)
        ) 
      }
    } else {
      # 其他修改先验概率拟合为正态分布
      priors[[k]] <- function(x) { 
        stats::dnorm(
          x, log = TRUE,
          mean = mean(model_result[[col_name]]), 
          sd = (stats::sd(model_result[[col_name]]) + 1e-5)
        ) 
      }
    }
  }
  
  return(priors)
}