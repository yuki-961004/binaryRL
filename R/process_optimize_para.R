#' Process: Optimizing Parameters
#' 
#' @description
#' This is an internal helper function for \code{fit_p}. Its primary purpose
#'  is to provide a unified interface for users to interact with various
#'  optimization algorithm packages. It adapts the inputs and outputs
#'  to be compatible with eight distinct algorithms, ensuring a seamless
#'  experience regardless of the underlying solver used.
#'
#'  The function provides several optimization algorithms:
#'   \itemize{
#'     \item 1. L-BFGS-B (from \code{stats::optim})
#'     \item 2. Simulated Annealing (\code{GenSA::GenSA})
#'     \item 3. Genetic Algorithm (\code{GA::ga})
#'     \item 4. Differential Evolution (\code{DEoptim::DEoptim})
#'     \item 5. Particle Swarm Optimization (\code{pso::psoptim})
#'     \item 6. Bayesian Optimization (\code{mlrMBO::mbo})
#'     \item 7. Covariance Matrix Adapting Evolutionary Strategy (\code{cmaes::cma_es})
#'     \item 8. Nonlinear Optimization (\code{nloptr::nloptr})
#'   }
#'   
#'  For more information, please refer to the homepage of this package:
#'  \url{https://yuki-961004.github.io/binaryRL/}
#'  
#' @param estimate [character] 
#' 
#'   Estimation method. Can be either \code{"MLE"} or \code{"MAP"}.
#'   \itemize{
#'     \item{\code{"MLE"}: (Default) Maximum Likelihood Estimation. This
#'       method finds the parameter values that maximize the log-likelihood
#'       of the data. A higher log-likelihood indicates that the parameters
#'       provide a better explanation for the observed human behavior. In
#'       other words, data simulated using these parameters would most
#'       closely resemble the actual human data. This method does not
#'       consider any prior information about the parameters.}
#'       
#'     \item{\code{"MAP"}: Maximum A Posteriori Estimation. This method
#'       finds the parameter values that maximize the posterior probability.
#'       It is an iterative process based on the Expectation-Maximization
#'       (EM) framework.
#'       \itemize{
#'         \item{\strong{Initialization}: The process begins by assuming a
#'           uniform distribution as the prior for each parameter, making the
#'           initial log-prior zero. The first optimization is thus
#'           equivalent to MLE.}
#'         \item{\strong{Iteration}: After finding the best parameters for
#'           all subjects, the algorithm assesses the actual distribution of
#'           each parameter and fits a normal distribution to it. This
#'           fitted distribution becomes the new empirical prior.}
#'         \item{\strong{Re-estimation}: The parameters are then re-optimized
#'           to maximize the updated posterior probability.}
#'         \item{\strong{Convergence}: This cycle repeats until the posterior
#'           probability converges or the maximum number of iterations
#'           (specified by \code{iteration_g}) is reached.}
#'       }
#'       Using this method requires that the \code{priors} argument
#'       be specified to define the initial prior distributions.
#'     }
#'    }
#' 
#' default: \code{estimate = "MLE"}
#'  
#' @param policy [character]
#' 
#' Specifies the learning policy to be used.
#' This determines how the model updates action values based on observed or
#'   simulated choices. It can be either \code{"off"} or \code{"on"}.
#'   
#'  \itemize{
#'   \item {
#'    \strong{Off-Policy (Q-learning): }
#'    This is the most common approach for modeling
#'     reinforcement learning in Two-Alternative Forced Choice (TAFC) tasks.
#'     In this mode, the model's goal is to learn the underlying value of
#'     each option by observing the human participant's behavior. It achieves
#'     this by consistently updating the value of the option that the
#'     human actually chose. The focus is on understanding the value 
#'     representation that likely drove the participant's decisions.
#'   }
#'   \item {
#'    \strong{Off-Policy (SARSA): }
#'    In this mode, the target policy and the behavior policy are identical. 
#'     The model first computes the selection probability for each option based 
#'     on their current values. Critically, it then uses these probabilities to 
#'     sample its own action. The value update is then performed on the action 
#'     that the model itself selected. This approach focuses more on directly 
#'     mimicking the stochastic choice patterns of the agent, rather than just 
#'     learning the underlying values from a fixed sequence of actions.
#'   }
#'  }
#'  
#' default: \code{policy = "off"}
#'  
#' @param data [data.frame] 
#' 
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
#' 
#' Specifies the ID of the subject whose optimal parameters will be fitted.
#'  This parameter accepts either string or numeric values. The provided
#'  ID must correspond to an existing subject identifier within the raw
#'  dataset provided to the function.
#' 
#' @param n_trials [integer] 
#' 
#' The total number of trials in your experiment.
#' 
#' @param n_params [integer] 
#' 
#' The number of free parameters in your model. 
#' 
#' @param obj_func [function]
#' 
#' The objective function that the optimization algorithm package accepts.
#'  This function must strictly take only one argument, \code{fit_p} (a vector
#'  of model parameters). Its output must be a single numeric value
#'  representing the loss function to be minimized. For more detailed
#'  requirements and examples, please refer to the relevant documentation 
#'  (
#'     \code{\link[binaryRL]{TD}}, 
#'     \code{\link[binaryRL]{RSTD}}, 
#'     \code{\link[binaryRL]{Utility}}
#'  ).
#' 
#' @param lower [vector] 
#' 
#' Lower bounds of free parameters
#' 
#' @param upper [vector] 
#' 
#' Upper bounds of free parameters
#' 
#' @param priors [list]
#'   A list specifying the prior distributions for the model parameters.
#'   This argument is mandatory when using \code{estimate = "MAP"}.
#'   There are two primary scenarios for its use:
#'
#'   \strong{1. Static MAP Estimation (Non-Hierarchical)}
#'   This approach is used when you have a strong, pre-defined belief about
#'   the parameter priors and do not want the model to update them iteratively.
#'   \describe{
#'     \item{Configuration:}{
#'       \itemize{
#'         \item{Set \code{estimate = "MAP"}.}
#'         \item{Provide a \code{list} defining your confident prior
#'           distributions.}
#'         \item{Keep \code{iteration_g = 0} (the default).}
#'       }
#'     }
#'     \item{Behavior:}{ The algorithm maximizes the posterior
#'       probability based solely on your specified priors. It will not
#'       use the EM (Expectation-Maximization) framework to learn new
#'       priors from the data.}
#'   }
#'
#'   \strong{2. Hierarchical Bayesian Estimation via EM}
#'   This approach is used to let the model learn the group-level (hierarchical)
#'   prior distributions directly from the data.
#'   \describe{
#'     \item{Configuration:}{
#'       \itemize{
#'         \item{Set \code{estimate = "MAP"}.}
#'         \item{Specify a weak or non-informative initial prior, such as a
#'           uniform distribution for all parameters.}
#'         \item{Set \code{iteration_g} to a value greater than 0.}
#'       }
#'     }
#'     \item{Behavior:}{ With a uniform prior, the initial
#'       log-posterior equals the log-likelihood, making the first estimation
#'       step equivalent to MLE. The algorithm then initiates the EM procedure:
#'       it iteratively assesses the actual parameter distribution across all
#'       subjects and updates the group-level priors. This cycle continues
#'       until the posterior converges or \code{iteration_g} is reached.}
#'   }
#' 
#'  default: \code{priors = NULL}
#' 
#' @param initial_params [vector]
#' 
#' Initial values for the free parameters that the optimization algorithm will
#'  search from. These are primarily relevant when using algorithms that require
#'  an explicit starting point, such as \code{L-BFGS-B}. If not specified,
#'  the function will automatically generate initial values close to zero.
#'  
#'  default: \code{initial_params = NA}.
#'
#' @param initial_size [integer]
#' 
#' This parameter corresponds to the population size in genetic 
#'  algorithms (\code{GA}). It specifies the number of initial candidate
#'  solutions that the algorithm starts with for its evolutionary search.
#'  This parameter is only required for optimization algorithms that operate on
#'  a population, such as `GA` or `DEoptim`. 
#'  
#'  default: \code{initial_size = 50}.
#'  
#' @param iteration [integer] 
#' 
#' The number of iterations the optimization algorithm will perform
#'  when searching for the best-fitting parameters during the fitting
#'  phase. A higher number of iterations may increase the likelihood of 
#'  finding a global optimum but also increases computation time.
#' 
#' @param seed [integer] 
#' 
#' Random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  
#' default: \code{seed = 123}
#'  
#' @param algorithm [character] 
#' 
#' Choose an algorithm package from
#'  \code{L-BFGS-B}, \code{GenSA},\code{GA},\code{DEoptim},\code{PSO},
#'  \code{Bayesian}, \code{CMA-ES}.
#'  
#' In addition, any algorithm from the \code{nloptr} package is also
#'  supported. If your chosen \code{nloptr} algorithm requires a local search,
#'  you need to input a character vector. The first element represents
#'  the algorithm used for global search, and the second element represents
#'  the algorithm used for local search.
#' 
#' @returns the result of binaryRL with optimal parameters
#' 
#' @examples
#' \dontrun{
#' binaryRL.res <- binaryRL::optimize_para(
#'   data = binaryRL::Mason_2024_G2,
#'   id = 1,
#'   obj_func = binaryRL::RSTD,
#'   n_params = 3,
#'   n_trials = 360,
#'   lower = c(0, 0, 0),
#'   upper = c(1, 1, 1),
#'   iteration = 10,
#'   seed = 123,
#'   algorithm = "L-BFGS-B"   # Gradient-Based (stats)
#'   #algorithm = "GenSA"    # Simulated Annealing (GenSA)
#'   #algorithm = "GA"       # Genetic Algorithm (GA)
#'   #algorithm = "DEoptim"  # Differential Evolution (DEoptim)
#'   #algorithm = "PSO"      # Particle Swarm Optimization (pso)
#'   #algorithm = "Bayesian" # Bayesian Optimization (mlrMBO)
#'   #algorithm = "CMA-ES"   # Covariance Matrix Adapting (cmaes)
#'   #algorithm = c("NLOPT_GN_MLSL", "NLOPT_LN_BOBYQA")
#' )
#' summary(binaryRL.res)
#' }
#' 
optimize_para <- function(
  policy = "off",
  estimate = "MLE",
  
  data,
  id,
  n_trials,
  n_params,
  
  obj_func,
  lower,
  upper,
  priors = NULL,
  
  initial_params = NA,
  initial_size = 50,
  
  iteration = 10,
  seed = 123,
  algorithm
){
  # 创建临时环境
  binaryRL.env <- new.env()
  
  # 给临时环境创建全局变量
  binaryRL.env$mode <- "fit"
  binaryRL.env$policy <- policy
  
  binaryRL.env$estimate <- estimate
  binaryRL.env$priors <- priors
  
  binaryRL.env$data <- data
  binaryRL.env$id <- id
  binaryRL.env$n_params <- n_params
  binaryRL.env$n_trials <- n_trials
  
  # 让obj_func的环境绑定在fit_env中
  environment(obj_func) <- binaryRL.env
  
  # 设定初始值
  if (length(initial_params) == 1 && is.na(initial_params)){
    initial_params <- lower + 1e-2
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
