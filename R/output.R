#' summary ga_result
#'
#' @param ga_result ga_result
#' @param obj_func obj_func in your global environment
#' @param n_trials number of trials in your experiment
#' @param params_name name of your parameters
#' @param sort vars need to be sorted
#' @param dig Digits after decimal point
#'
#' @return output
#' @export
#'
output <- function(
    ga_result = ga_result,
    obj_func = obj_func,
    n_trials,
    params_name,
    sort = c("epsilon"),
    digits = 5
){
  ################################ [model fit] ###################################
  n_params <- ncol(ga_result@solution)
  n_trials <- n_trials
  acc <- as.numeric(
    capture.output(obj_func(params = as.vector(ga_result@solution)))[1]
  )
  Log_Likelihood <- round(ga_result@fitnessValue, digits = digits)
  AIC <- round(2 * n_params - 2 * Log_Likelihood, digits = digits)
  BIC <- round(n_params * log(n_trials) - 2 * Log_Likelihood, digits = digits)
  
  # 创建一个包含指标名称的向量
  model_fit_name <- c(
    "Number of Parameters", "Number of Trials", 
    "Accuracy", "Log-Likelihood", 
    "AIC", "BIC"
  )
  
  # 创建一个包含指标值的向量
  model_fit_value <- c(
    n_params, n_trials, 
    acc, Log_Likelihood, 
    AIC, BIC
  )
  
  # 将两个向量组合成 data.frame
  model_fit <- data.frame(name = model_fit_name, value = model_fit_value)
  
  ################################ [best params] #################################
  # 在程序内对ε和β进行了排序, 但是在params输入时并没有排序
  # output需要重新排序后输出
  # 创建一个空的 data.frame
  best_params <- data.frame(name = character(), value = numeric())
  
  # 将结果添加到 data.frame
  for (i in 1:ncol(ga_result@solution)) {
    best_params <- rbind(
      best_params, 
      data.frame(name = params_name[i], value = ga_result@solution[1,i])
    )
  }
  
  
  for (sort_var in sort) {
    target_rows <- grep(sort_var, best_params$name)
    target_values <- best_params[target_rows, "value"]
    target_values_sorted <- sort(target_values)
    best_params[target_rows, "value"] <- target_values_sorted
  }
  
  rownames(best_params) <- NULL
  best_params$value <- round(best_params$value, digits = digits)
  
  res <- list(model_fit, best_params)
  # 查看结果
  return(res)
}