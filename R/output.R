#' summary ga_result
#'
#' @param ga_result ga_result
#' @param obj_func obj_func in your global environment
#' @param n_trials number of trials in your experiment
#' @param params_name name of your parameters
#'
#' @return output
#' @export
#'
output <- function(
    ga_result, 
    obj_func,
    n_trials,
    params_name
){
  n_params <- ncol(ga_result@solution)
  n_trials <- n_trials
  Log_Likelihood <- ga_result@fitnessValue
  
  params <- as.vector(ga_result@solution)
  acc <- capture.output(obj_func(params = params))[1]
  
  cat("Number of Parameters:", n_params, "\n")
  cat("Number of Trials:", n_trials, "\n", "\n")
  
  cat(acc, "\n")
  cat("Log-Likelihood:", Log_Likelihood, "\n")
  cat("AIC:", 2 * n_params - 2 * Log_Likelihood, "\n")
  cat("BIC:", n_params * log(n_trials) - 2 * Log_Likelihood, "\n", "\n")
  
  # 在程序内对ε和β进行了排序, 但是在params输入时并没有排序
  # output需要重新排序后输出
  # 创建一个空的 data.frame
  result_df <- data.frame(name = character(), value = numeric())
  
  # 将结果添加到 data.frame
  for (i in 1:ncol(ga_result@solution)) {
    result_df <- rbind(result_df, data.frame(name = params_name[i], value = ga_result@solution[1,i]))
  }
  
  # 筛选包含 "ε" 的行并排序
  epsilon_rows <- grep("ε", result_df$name)
  epsilon_values <- result_df[epsilon_rows, "value"]
  epsilon_values_sorted <- sort(epsilon_values)
  result_df[epsilon_rows, "value"] <- epsilon_values_sorted
  
  # 筛选包含 "β" 的行并排序
  beta_rows <- grep("β", result_df$name)
  beta_values <- result_df[beta_rows, "value"]
  beta_values_sorted <- sort(beta_values)
  result_df[beta_rows, "value"] <- beta_values_sorted
  
  rownames(result_df) <- NULL
  # 查看结果
  return(result_df)
}