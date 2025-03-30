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
#' @param initial_params [vector] Initial values for the free parameters. 
#'  These need to be set only when using L-BFGS-B. Other algorithms 
#'  automatically generate initial values.
#'  for `L-BFGS-B`, `GenSA`, set `initial = c(0, 0, ...)`
#'  
#' @param initial_size [integer] Initial values for the free parameters. 
#'  These need to be set only when using L-BFGS-B. Other algorithms 
#'  automatically generate initial values.
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
#'  `L-BFGS-B`, `GenSA`, `GA`, `DEoptim`, `Bayesian`, `PSO`, `CMA-ES`
#' 
#' @returns the result of binaryRL with optimal parameters
#' @export
#'

fit_p <- function(
    data,
    obj_func,
    initial_params = NA,
    initial_size = 50,
    lower,
    upper,
    iteration = 10,
    seed = 123,
    algorithm
){
  assign(x = "fit_data", value = data, envir = globalenv())
  
  set.seed(seed)
  
  # 提取参数的数量
  if (length(lower) == length(upper)) {
    n_params <- length(lower)
  } else {
    stop("The lengths of 'lower' and 'upper' must be equal.")
  }
  
  # 设定初始值
  if (is.na(initial_params)){
    initial_params <- c(rep(1e-5, n_params))
  }
  
  result <- switch(
    algorithm,
    "L-BFGS-B" = {
      stats::optim(
        par = initial_params,
        method = "L-BFGS-B",
        fn = obj_func,
        lower = lower,
        upper = upper,
        control = list(
          maxit = iteration
        )
      )
    },
    "GenSA" = {
      # 检查所依赖的算法包是否安装
      if (!requireNamespace("GenSA", quietly = TRUE)) {
        stop(
          "The 'GenSA' package is required for this algorithm.\n 
          Please install it using install.packages('GenSA')."
        )
      }
      
      GenSA::GenSA(
        fn = obj_func,
        par = initial_params,
        lower = lower,
        upper = upper,
        control = list(
        maxit = iteration,
          seed = seed
        )
      )
    },
    "GA" = {
      # 检查所依赖的算法包是否安装
      if (!requireNamespace("GA", quietly = TRUE)) {
        stop(
          "The 'GA' package is required for this algorithm.\n 
          Please install it using install.packages('GA')."
        )
      }
      
      GA::ga(
        type = "real-valued",
        fitness = function(x) obj_func(x),
        popSize = initial_size,
        lower = lower,
        upper = upper,
        maxiter = iteration,
        parallel = TRUE
      )
    },
    "DEoptim" = {
      # 检查所依赖的算法包是否安装
      if (!requireNamespace("DEoptim", quietly = TRUE)) {
        stop(
          "The 'DEoptim' package is required for this algorithm.\n 
          Please install it using install.packages('DEoptim')."
        )
      }
      
      DEoptim::DEoptim(
        fn = obj_func,
        lower = lower,
        upper = upper,
        control = DEoptim::DEoptim.control(
          NP = initial_size,
          itermax = iteration,
          parallelType = c("parallel"),
          packages = c("binaryRL"),
          parVar = c("fit_data")
        )
      )
    },
    "Bayesian" = {
      # 检查所依赖的算法包是否安装
      required_pkgs <- c("mlrMBO", "ParamHelpers", "smoof")
      missing_pkgs <- required_pkgs[!sapply(
        required_pkgs, requireNamespace, quietly = TRUE
      )]
      
      if (length(missing_pkgs) > 0) {
        stop(
          "The following packages are required for this algorithm.\n",
          paste(missing_pkgs, collapse = ", "), "\n",
          "Please install them using install.packages(c(", 
          paste0("'", missing_pkgs, "'", collapse = ", "), "))."
        )
      }
      
      param_list <- lapply(
        1:n_params, function(i) {
          ParamHelpers::makeNumericParam(
            id = paste0("param_", i),
            lower = lower[i],
            upper = upper[i]
          )
        }
      )
    
      bys_func <- smoof::makeSingleObjectiveFunction(
        fn = obj_func,
        par.set = ParamHelpers::makeParamSet(params = param_list)
      )
    
      mlrMBO::mbo(
        fun = bys_func, 
        design = ParamHelpers::generateDesign(
          n = initial_size, 
          par.set = ParamHelpers::getParamSet(bys_func), 
          fun = lhs::maximinLHS
        ), 
        control = mlrMBO::setMBOControlTermination(
          control = mlrMBO::makeMBOControl(),
          iters = iteration
        )
      )
    },
    "PSO" = {
      # 检查所依赖的算法包是否安装
      if (!requireNamespace("pso", quietly = TRUE)) {
        stop(
          "The 'PSO' package is required for this algorithm.\n 
          Please install it using install.packages('pso')."
        )
      }
      
      pso::psoptim(
        par = initial_params,
        fn = obj_func,
        lower = lower,
        upper = upper,
        control = list(
          maxit = iteration
        )
      )
    },
    "CMA-ES" = {
      # 检查所依赖的算法包是否安装
      if (!requireNamespace("cmaes", quietly = TRUE)) {
        stop(
          "The 'CMA-ES' package is required for this algorithm.\n 
          Please install it using install.packages('cmaes')."
        )
      }
      
      cmaes::cma_es(
        par = initial_params,
        fn = obj_func,
        lower = lower,
        upper = upper,
        control = list(
          maxit = iteration
        )
      )
    },
   { # 默认情况（如果 algorithm 不匹配任何已知值）
      stop("
        Choose a algorithm from 
        `L-BFGS-B`, `GenSA`, 
        `GA`, `DEoptim`,
        `Bayesian`, `PSO`,
        `CMA-ES`
      ")
    }
  )

  switch(
    algorithm,
    "L-BFGS-B" = {
     fit_params <- as.vector(result$par)
    },
    "GA" = {
      fit_params <- as.vector(result@solution)
    },
    "GenSA" = {
      fit_params <- as.vector(result$par)
    },
    "DEoptim" = {
      fit_params <- as.vector(result$optim$bestmem)
    },
    "Bayesian" = {
      fit_params <- as.vector(
        as.numeric(result$final.opt.state$opt.result$mbo.result$x)
      )
    },
    "PSO" = {
      fit_params <- as.vector(result$par)
    },
    "CMA-ES" = {
      fit_params <- as.vector(result$par)
    },
    {
    stop("
        Choose a algorithm from 
        `L-BFGS-B`, `GenSA`, 
        `GA`, `DEoptim`,
        `Bayesian`, `PSO`,
        `CMA-ES`
      ")
    }
  )
  
  obj_func(params = fit_params)
  binaryRL_res$output <- fit_params
  
  on.exit(remove("fit_data", envir = .GlobalEnv))
  summary(binaryRL_res)
  return(binaryRL_res)
}
