#' Step 3: Optimizing parameters to fit real data
#' 
#' @description
#' This function optimizes free parameters of reinforcement learning 
#'  models built with the `run_m` function. After constructing a 
#'  reinforcement learning model (a function with only ONE argument, 
#'  `params`), the `fit_p` function searches for the optimal values of 
#'  these free parameters.
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
#' @param data [data.frame] raw data. 
#'  This data should include the following mandatory columns: 
#'   \itemize{
#'     \item "sub"
#'     \item "time_line" (e.g., "Block", "Trial")
#'     \item "L_choice"
#'     \item "R_choice"
#'     \item "L_reward"
#'     \item "R_reward"
#'     \item "sub_choose"
#'   }
#'  
#' @param id [vector] which subject is going to be analyzed.
#'  is being analyzed. The value should correspond to an entry in the "sub" 
#'  column, which must contain the subject IDs. 
#'  e.g., `id = unique(data$Subject)` 
#'  
#' @param fit_model [list] A collection of functions applied to fit models to the data.
#' 
#' @param funcs [vector] A character vector containing the names of all 
#'  user-defined functions required for the computation.
#' 
#' @param model_name [list] the name of fit modals
#' 
#' @param n_trials [integer] number of total trials
#' 
#' @param lower [list] The lower bounds for model fit models
#' 
#' @param upper [list] The upper bounds for model fit models
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
#' @param iteration [integer] the number of iteration
#' 
#' @param seed [integer] random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  default: `seed = 123` 
#'  
#' @param nc [integer] Number of CPU cores to use for parallel computation.
#' 
#' @param algorithm [character] Choose an algorithm package from
#'  `L-BFGS-B`, `GenSA`, `GA`, `DEoptim`, `PSO`, `Bayesian`, `CMA-ES`.
#'  In addition, any algorithm from the `nloptr` package is also
#'  supported. If your chosen `nloptr` algorithm requires a local search,
#'  you need to input a character vector. The first element represents
#'  the algorithm used for global search, and the second element represents
#'  the algorithm used for local search.
#'
#' @return The optimal parameters found by the algorithm for each subject,
#'  along with the model fit calculated using these parameters.
#'  This is returned as an object of class \code{binaryRL} containing results
#'  for all subjects with all models.
#' 
#' @export
#'

fit_p <- function(
  data,
  id = c(1:40),
  n_trials,
  fit_model = list(TD, RSTD, Utility),
  funcs = NULL,
  model_name = c("TD", "RSTD", "Utility"),
  lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
  upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
  initial_params = NA,
  initial_size = 50,
  iteration = 10,
  seed = 123,
  nc = 1,
  algorithm
){
  model_comparison <- list()
  model_result <- list()
  
  # Check for internally parallel algorithms
  if (nc == 1) {
    
    for (i in 1:length(fit_model)){
      
      message(paste0(
        "\n", 
        "Fitting Model: ", model_name[i], 
        "\n"
      ))
      
      n_subjects <- length(id)
      
      # 进度条
      progressr::handlers(progressr::handler_txtprogressbar)
      
      progressr::with_progress({
        
        p <- progressr::progressor(steps = n_subjects)
        
        for (j in 1:n_subjects) {
          
          p()
          
          n_params <- length(lower[[i]])
          
          binaryRL_res <- binaryRL::optimize_para(
            data = data,
            id = id[j],
            n_params = n_params,
            n_trials = n_trials,
            obj_func = fit_model[[i]],
            lower = lower[[i]],
            upper = upper[[i]],
            iteration = iteration,
            seed = seed,
            initial_params = initial_params,
            initial_size = initial_size,
            algorithm = algorithm 
          )
          
          model_result[[j]] <- data.frame(
            fit_model = model_name[i],
            Subject = id[j],
            ACC = binaryRL_res$acc,
            LogL = -binaryRL_res$ll,
            AIC = binaryRL_res$aic,
            BIC = binaryRL_res$bic
          )
          
          for (k in 1:n_params) {
            model_result[[j]][1, k + 6] <- binaryRL_res$output[k]
            names(model_result[[j]])[k + 6] <- paste0("param_", k)
          }
        }
      })
      model_comparison[[i]] <- model_result
    }
  }
  else {
    
    for (i in 1:length(fit_model)){
      
      message(paste0(
        "\n", 
        "Fitting Model: ", model_name[i], 
        "\n"
      ))
      
      future::plan(future::multisession, workers = nc)
      doFuture::registerDoFuture()
      
      n_subjects <- length(id)
      
      # 进度条
      progressr::handlers(progressr::handler_txtprogressbar)
      
      progressr::with_progress({
        
        p <- progressr::progressor(steps = n_subjects)
        
        doRNG::registerDoRNG(seed = seed)
        
        model_result <- foreach::foreach(
          j = 1:n_subjects, .combine = rbind,
          .packages = c("binaryRL"),
          .export = funcs
        ) %dorng% {
          n_params <- length(lower[[i]])
          
          binaryRL_res <- binaryRL::optimize_para(
            data = data,
            id = id[j],
            n_params = n_params,
            n_trials = n_trials,
            obj_func = fit_model[[i]],
            lower = lower[[i]],
            upper = upper[[i]],
            iteration = iteration,
            seed = seed,
            initial_params = initial_params,
            initial_size = initial_size,
            algorithm = algorithm
          )
          
          result_j <- data.frame(
            fit_model = model_name[i],
            Subject = id[j],
            ACC = binaryRL_res$acc,
            LogL = -binaryRL_res$ll,
            AIC = binaryRL_res$aic,
            BIC = binaryRL_res$bic
          )
          
          for (k in 1:n_params) {
            result_j[1, k + 6] <- binaryRL_res$output[k]
            names(result_j)[k + 6] <- paste0("param_", k)
          }
          
          # 在foreach循环内更新进度条
          p() 
          return(result_j)
        }
      })
      
      # 將結果包在一個 list 裡面，保持結構一致性
      model_comparison[[i]] <- list(model_result) 
    }
  }

  result <- model_comparison
  
  return(result)
}