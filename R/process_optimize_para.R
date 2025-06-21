#' Process: Optimizing Parameters
#' 
#' @description
#' This is an internal helper function for `fit_p`. Its primary purpose
#'  is to provide a unified interface for users to interact with various
#'  optimization algorithm packages. It adapts the inputs and outputs
#'  to be compatible with eight distinct algorithms, ensuring a seamless
#'  experience regardless of the underlying solver used.
#'
#'  The function provides several optimization algorithms:
#'   \itemize{
#'     \item 1. L-BFGS-B (from `stats::optim`);
#'     \item 2. Simulated Annealing (`GenSA`);
#'     \item 3. Genetic Algorithm (`GA`);
#'     \item 4. Differential Evolution (`DEoptim`);
#'     \item 5. Particle Swarm Optimization (`pso`);
#'     \item 6. Bayesian Optimization (`mlrMBO`);
#'     \item 7. Covariance Matrix Adapting Evolutionary Strategy (`cmaes`);
#'     \item 8. Nonlinear Optimization (`nloptr`)
#'   }
#'
#'  For more information, please refer to the GitHub repository:
#'  https://github.com/yuki-961004/binaryRL
#' 
#' @param data [data.frame] 
#' This data should include the following mandatory columns: 
#'  \itemize{
#'    \item "sub"
#'    \item "time_line" (e.g., "Block", "Trial")
#'    \item "L_choice"
#'    \item "R_choice"
#'    \item "L_reward"
#'    \item "R_reward"
#'    \item "sub_choose"
#'  }
#'  
#' @param id [character]
#' Specifies the ID of the subject whose optimal parameters will be fitted.
#'  This parameter accepts either string or numeric values. The provided
#'  ID must correspond to an existing subject identifier within the raw
#'  dataset provided to the function.
#' 
#' @param obj_func [function]
#' The objective function that the optimization algorithm package accepts.
#'  This function must strictly take only one argument, `params` (a vector
#'  of model parameters). Its output must be a single numeric value
#'  representing the loss function to be minimized. For more detailed
#'  requirements and examples, please refer to the relevant documentation 
#'  (
#'     \code{\link[binaryRL]{TD}}, 
#'     \code{\link[binaryRL]{RSTD}}, 
#'     \code{\link[binaryRL]{Utility}}
#'  ).
#'  
#' @param n_params [integer] 
#' The number of free parameters in your model. 
#' 
#' @param n_trials [integer] 
#' The total number of trials in your experiment.
#' 
#' @param lower [vector] 
#' Lower bounds of free parameters
#' 
#' @param upper [vector] 
#' Upper bounds of free parameters
#' 
#' @param initial_params [vector]
#' Initial values for the free parameters that the optimization algorithm will
#'  search from. These are primarily relevant when using algorithms that require
#'  an explicit starting point, such as \code{L-BFGS-B}. If not specified,
#'  the function will automatically generate initial values close to zero.
#'  
#'  \code{default: initial_params = NA}.
#'
#' @param initial_size [integer]
#' This parameter corresponds to the \strong{population size} in genetic 
#'  algorithms (\code{GA}). It specifies the number of initial candidate
#'  solutions that the algorithm starts with for its evolutionary search.
#'  This parameter is only required for optimization algorithms that operate on
#'  a population, such as `GA` or `DEoptim`. 
#'  
#'  \code{default: initial_size = 50}.
#'  
#' @param iteration [integer] 
#' The number of iterations the optimization algorithm will perform
#'  when searching for the best-fitting parameters during the fitting
#'  phase. A higher number of iterations may increase the likelihood of 
#'  finding a global optimum but also increases computation time.
#' 
#' @param seed [integer] 
#' Random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  
#'  \code{default: seed = 123}
#'  
#' @param algorithm [character] 
#' Choose an algorithm package from
#'  `L-BFGS-B`, `GenSA`, `GA`, `DEoptim`, `PSO`, `Bayesian`, `CMA-ES`.
#'  
#' In addition, any algorithm from the `nloptr` package is also
#'  supported. If your chosen `nloptr` algorithm requires a local search,
#'  you need to input a character vector. The first element represents
#'  the algorithm used for global search, and the second element represents
#'  the algorithm used for local search.
#' 
#' @returns the result of binaryRL with optimal parameters
#' 
#' @examples
#' \dontrun{
#' list_simulated <- binaryRL::simulate_list(
#'   data = binaryRL::Mason_2024_Exp2,
#'   obj_func = binaryRL::RSTD,
#'   n_params = 3,
#'   n_trials = 360,
#'   lower = c(0, 0, 1),
#'   upper = c(1, 1, 1),
#'   iteration = 100
#' )
#'
#' df_recovery <- binaryRL::recovery_data(
#'   list = list_simulated,
#'   fit_model = binaryRL::RSTD,
#'   model_name = "RSTD",
#'   n_params = 3,
#'   n_trials = 360,
#'   lower = c(0, 0, 1),
#'   upper = c(1, 1, 5),
#'   iteration = 100,
#'   nc = 1,
#'   algorithm = "L-BFGS-B"
#' )
#' }
#' 
optimize_para <- function(
    data,
    id,
    obj_func,
    n_params,
    n_trials,
    lower,
    upper,
    initial_params = NA,
    initial_size = 50,
    iteration = 10,
    seed = 123,
    algorithm
){
  # 创建临时环境
  binaryRL.env <- new.env()
  mode <- "fit"
  # 将data传入到临时环境
  assign(x = "mode", value = mode, envir = binaryRL.env)
  assign(x = "data", value = data, envir = binaryRL.env)
  assign(x = "id", value = id, envir = binaryRL.env)
  assign(x = "n_params", value = n_params, envir = binaryRL.env)
  assign(x = "n_trials", value = n_trials, envir = binaryRL.env)
  # 让obj_func的环境绑定在fit_env中
  environment(obj_func) <- binaryRL.env
  
  # 设定初始值
  if (is.na(initial_params)){
    initial_params <- c(rep(1e-5, n_params))
  }
  
  set.seed(seed)
  
  if (algorithm[[1]] == "L-BFGS-B") {
    result <- stats::optim(
      par = initial_params,
      method = "L-BFGS-B",
      fn = obj_func,
      lower = lower,
      upper = upper,
      control = list(
        maxit = iteration
      )
    )
  } 
  else if (algorithm[[1]] == "GenSA") {
    check_dependency("GenSA", algorithm_name = "Simulated Annealing")
    
    result <- GenSA::GenSA(
      fn = obj_func,
      par = initial_params,
      lower = lower,
      upper = upper,
      control = list(
        maxit = iteration,
        seed = seed
      )
    )
  } 
  else if (algorithm[[1]] == "GA") {
    check_dependency("GA", algorithm_name = "Genetic Algorithm")
    
    result <- GA::ga(
      type = "real-valued",
      fitness = function(x) -obj_func(x),
      popSize = initial_size,
      lower = lower,
      upper = upper,
      maxiter = iteration,
      monitor = FALSE
      #parallel = TRUE
    )
  } 
  else if (algorithm[[1]] == "DEoptim") {
    check_dependency("DEoptim", algorithm_name = "Differential Evolution")
    
    result <- DEoptim::DEoptim(
      fn = obj_func,
      lower = lower,
      upper = upper,
      control = DEoptim::DEoptim.control(
        NP = initial_size,
        itermax = iteration,
        trace = FALSE
        #parallelType = "parallel"
        #packages = "binaryRL"
      )
    )
  }
  else if (algorithm[[1]] == "Bayesian") {
    required_pkgs <- c(
      "mlrMBO", "mlr", "ParamHelpers", "smoof", "lhs",
      "DiceKriging", "rgenoud"
    )
    check_dependency(required_pkgs, algorithm_name = "Bayesian Optimization")
    
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
    
    suppressWarnings(
      result <- mlrMBO::mbo(
        fun = bys_func, 
        design = ParamHelpers::generateDesign(
          n = initial_size, 
          par.set = ParamHelpers::getParamSet(bys_func), 
          fun = lhs::maximinLHS
        ), 
        control = mlrMBO::setMBOControlInfill(
          mlrMBO::setMBOControlTermination(
            control = mlrMBO::makeMBOControl(),
            iters = iteration
          ),
          opt.focussearch.maxit = 10
        ),
        show.info = FALSE
      )
    )
  }
  else if (algorithm[[1]] == "PSO") {
    check_dependency("pso", algorithm_name = "Particle Swarm Optimization")
    
    result <- pso::psoptim(
      par = initial_params,
      fn = obj_func,
      lower = lower,
      upper = upper,
      control = list(
        maxit = iteration,
        trace = 0
      )
    )
  }
  else if (algorithm[[1]] == "CMA-ES") {
    check_dependency("cmaes", algorithm_name = "Covariance Matrix Adapting")
    
    result <- cmaes::cma_es(
      par = initial_params,
      fn = obj_func,
      lower = lower,
      upper = upper,
      control = list(
        maxit = iteration
      )
    )
  }
  else if (startsWith(algorithm[[1]], "NLOPT_")) {
    check_dependency("nloptr", algorithm_name = "Nonlinear Optimization")
    
    if (length(algorithm) > 1) {
      local_opts <- list(
        algorithm = algorithm[[2]], 
        xtol_rel = 1.0e-8            
      )
    } else {
      local_opts <- NULL
    }
    
    result <- nloptr::nloptr(
      x0 = initial_params,
      eval_f = obj_func,
      lb = lower,
      ub = upper,
      opts = list(
        algorithm = algorithm[[1]], 
        local_opts = local_opts,
        maxeval = iteration
      )
    )
  }
  else {
    stop("
      Choose a algorithm from 
      `L-BFGS-B`, `GenSA`, 
      `GA`, `DEoptim`,
      `Bayesian`, `PSO`,
      `CMA-ES`, `NLOPT_`
    ")
  }
  

  if (algorithm[[1]] == "L-BFGS-B") {
    fit_params <- as.vector(result$par)
  }
  else if (algorithm[[1]] == "GenSA") {
    fit_params <- as.vector(result$par)
  }
  else if (algorithm[[1]] == "GA") {
    fit_params <- as.vector(result@solution[1,])
  }
  else if (algorithm[[1]] == "DEoptim") {
    fit_params <- as.vector(result$optim$bestmem)
  }
  else if (algorithm[[1]] == "Bayesian") {
    fit_params <- as.vector(
      as.numeric(result$final.opt.state$opt.result$mbo.result$x)
    )
  }
  else if (algorithm[[1]] == "PSO") {
    fit_params <- as.vector(result$par)
  }
  else if (algorithm[[1]] == "CMA-ES") {
    fit_params <- as.vector(result$par)
  }
  else if (startsWith(algorithm[[1]], "NLOPT_")) {
    fit_params <- as.vector(result$solution)
  }
  else {
    stop("
      Choose a algorithm from 
      `L-BFGS-B`, `GenSA`, 
      `GA`, `DEoptim`,
      `Bayesian`, `PSO`,
      `CMA-ES`, `NLOPT_`
    ")
  }
  
  # 用找到的最佳参数带回到obj_func中
  obj_func(params = fit_params)
  # obj_func会给binaryRL.env传入一个binaryRL.res
  binaryRL.env$binaryRL.res$output <- fit_params
  binaryRL.env$binaryRL.res$algorithm <- result

  return(binaryRL.env$binaryRL.res)
}
