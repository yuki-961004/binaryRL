#' fit_p
#'
#' @param data A data frame containing the raw data. 
#' This data should include the following mandatory columns: 
#' - "sub", "time_line", "L_choice", "R_choice", "choose", "L_reward", "R_reward". 
#' The following arguments allow you to customize the column names used for processing
#' 
#' @param obj_func object function, be sure that you write 'binaryRL_res <<- res'
#' 
#' @param algorithm Choose a algorithm pacakge from `L-BFGS-B`, `GA`, `GenSA`, `DEoptim`
#' 
#' @param initial initial value for searching parameters, only needed in stats
#' 
#' @param lower lower bounds of parameters
#' @param upper upper bounds of parameters
#' 
#' @param iteration iteration
#' 
#' @param seed A numeric value to set the random seed. 
#' This ensures that the results are reproducible and remain the same each time the function is run.
#' Provide the value as a number. 
#' default: `seed = 123` 
#'
#' @returns binaryRL_res
#' @export
#'
fit_p <- function(
    data,
    obj_func,
    initial,
    lower,
    upper,
    algorithm,
    iteration = 10,
    seed = 123
){
  set.seed(123)
  
  result <- switch(algorithm,
   "L-BFGS-B" = {
     stats::optim(
       par = initial,
       method = "L-BFGS-B",
       fn = obj_func,
       lower = lower,
       upper = upper,
       control = list(maxit = iteration)
     )
   },
   "GenSA" = {
     GenSA::GenSA(
       fn = obj_func,
       lower = lower,
       upper = upper,
       control = list(
         maxit = iteration,
         seed = 123
       )
     )
   },
   "GA" = {
     GA::ga(
       type = "real-valued",
       fitness = function(x) obj_func(x),
       lower = lower,
       upper = upper,
       maxiter = iteration,
       parallel = TRUE
     )
   },
   "DEoptim" = {
     DEoptim::DEoptim(
       fn = obj_func,
       lower = lower,
       upper = upper,
       control = DEoptim::DEoptim.control(
         itermax = iteration,
         parallelType = c("parallel"),
         packages = c("binaryRL"),
         parVar = c("data")
       )
     )
   },
   { # 默认情况（如果 algorithm 不匹配任何已知值）
     stop("Choose a algorithm from `L-BFGS-B`, `GA`, `GenSA`, `DEoptim`")
   }
  )
  
  switch(algorithm,
   "L-BFGS-B" = {
     obj_func(params = as.vector(result$par))
     binaryRL_res$output <- as.vector(result$par)
   },
   "GA" = {
     obj_func(params = as.vector(result@solution))
     binaryRL_res$output <- as.vector(result@solution)
   },
   "GenSA" = {
     obj_func(params = as.vector(result$par))
     binaryRL_res$output <-as.vector(result$par)
   },
   "DEoptim" = {
     obj_func(params = as.vector(result$optim$bestmem))
     binaryRL_res$output <-as.vector(result$optim$bestmem)
   },
   {
     stop("Choose a algorithm from `L-BFGS-B`, `GA`, `GenSA`, `DEoptim`") 
   }
  )
  
  summary(binaryRL_res)
  return(binaryRL_res)
}