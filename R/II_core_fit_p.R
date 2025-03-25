#' fit_p
#' 
#' @description
#' This function optimizes free parameters of reinforcement learning 
#' models built with the `run_m` function. After constructing a 
#' reinforcement learning model (a function with only ONE argument, 
#' `params`), the `fit_p` function searches for the optimal values of 
#' these free parameters.
#'
#' The package provides four optimization algorithms:
#'
#' * L-BFGS-B (`stats::optim`)
#' * Simulated Annealing (`GenSA::GenSA`)
#' * Genetic Algorithm (`GA::ga`)
#' * Differential Evolution (`DEoptim::DEoptim`)
#'
#' We recommend Differential Evolution (`DEoptim::DEoptim`) for its speed.
#' 
#' For more information, please refer to the GitHub repository:
#' https://github.com/yuki-961004/binaryRL
#' 
#' @param data [data.frame] raw data. 
#'  This data should include the following mandatory columns: 
#'  - "sub", "time_line", "L_choice", "R_choice", "L_reward", "R_reward". 
#' 
#' @param obj_func [function] a function with only ONE argument `params`. 
#'  Additionally, it is important to note that the data needs to be retrieved 
#'  from parent.frame() and the results passed back to parent.frame(). 
#'  This function returns the log likelihood (logL).
#' 
#' @param initial [vector] Initial values for the free parameters. 
#'  These need to be set only when using L-BFGS-B. Other algorithms 
#'  automatically generate initial values.
#'  for `L-BFGS-B`, `GenSA`, set `initial = c(0, 0, ...)`
#'  for `Bayesian`, `GA`, set `initial = 50`
#' 
#' @param lower [vector] lower bounds of free parameters
#' 
#' @param upper [vector] upper bounds of free parameters
#' 
#' @param iteration [integer] the number of iteration
#' 
#' @param seed [integer] random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  default: `seed = 123` 
#'  
#' @param algorithm [character] Choose a algorithm package from 
#'  `L-BFGS-B`, `GenSA`, `GA`, `DEoptim`, `Bayesian`
#'
#' @returns the result of binaryRL with optimal parameters
#' @export
#'
fit_p <- function(
    data,
    obj_func,
    initial = NULL,
    lower,
    upper,
    iteration = 10,
    seed = 123,
    algorithm
){
  data <- data
  
  set.seed(123)
  
  if (algorithm == "Bayesian"){
    # 贝叶斯模型前置准备 #
    n_params <- length(lower) # 假设 lower 是一个包含参数下界的向量
    
    # 动态生成参数列表
    param_list <- lapply(1:n_params, function(i) {
      ParamHelpers::makeNumericParam(
        id = paste0("param_", i),  # 生成参数 id，如 "param_1", "param_2" 等
        lower = lower[i],          # 从 lower 向量中获取下界
        upper = upper[i]           # 从 upper 向量中获取上界 
      )
    })
    
    # 创建一个mlrMBO接受的函数
    bys_func <- smoof::makeSingleObjectiveFunction(
      fn = obj_func,
      par.set = ParamHelpers::makeParamSet(params = param_list) 
    )
    # 贝叶斯模型前置结束 #
  }
  
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
       par = initial,
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
       popSize = initial,
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
   "Bayesian" = {
     mlrMBO::mbo(
       fun = bys_func, 
       design = ParamHelpers::generateDesign(
         n = initial, 
         par.set = ParamHelpers::getParamSet(bys_func), 
         fun = lhs::maximinLHS
       ), 
       control = mlrMBO::setMBOControlTermination(
         control = mlrMBO::makeMBOControl(),
         iters = iteration
       )
     )
   },
   { # 默认情况（如果 algorithm 不匹配任何已知值）
     stop("
          Choose a algorithm from 
          `L-BFGS-B`, `GenSA`, 
          `GA`, `DEoptim`,
          `Bayesian`
        ")
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
     binaryRL_res$output <- as.vector(result$par)
   },
   "DEoptim" = {
     obj_func(params = as.vector(result$optim$bestmem))
     binaryRL_res$output <- as.vector(result$optim$bestmem)
   },
   "Bayesian" = {
     obj_func(params = as.vector(as.numeric(result$final.opt.state$opt.result$mbo.result$x)))
     binaryRL_res$output <- as.vector(as.numeric(result$final.opt.state$opt.result$mbo.result$x))
   },
   {
     stop("
          Choose a algorithm from 
          `L-BFGS-B`, `GenSA`, 
          `GA`, `DEoptim`,
          `Bayesian`
        ")
   }
  )
  
  summary(binaryRL_res)
  return(binaryRL_res)
}