#' summary
#'
#' @param object binaryRl_res
#' @param ... others
#'
#' @return summary
#' @export
#'
summary <- function(object, ...) {
  cat("Results of the Reinforcement Learning Model:\n")
  
  object$data <- NULL
  
  cat("\nParameters:\n")
  
  cat("  ", "\u03BB: ", round(object$params$lambda, 3), "\n")
  cat("  ", "\u03B3: ", round(object$params$gamma, 3), "\n")
  cat("  ", "\u03B7: ", round(object$params$eta, 3), "\n")
  cat("  ", "\u03B5: ", round(object$params$epsilon, 3), "\n")
  cat("  ", "\u03C4: ", round(object$params$tau, 3), "\n")
  
  
  cat("\nModel Fit:\n")
  
  cat("  ", "Accuracy: ", object$acc, "%\n")
  cat("  ", "LogL: ", object$ll, "\n")
  cat("  ", "AIC: ", object$aic, "\n")
  cat("  ", "BIC: ", object$bic, "\n")
  
  # 获取所有参数的名称
  param_names <- names(object$params)
  
  # 计算最大参数值的个数
  max_values <- max(sapply(object$params, length))
  
  # 初始化一个空的 list 用来存储数据
  param_list <- list()
  
  # 填充参数名
  param_list$Parameter <- param_names
  
  # 根据最大值数目动态创建 Value 列
  for (i in 1:max_values) {
    param_list[[paste("Value", i, sep = "")]] <- sapply(object$params, function(param) {
      if (length(param) >= i) {
        return(round(param[i], 5))  # 取第 i 个值并保留5位小数
      } else {
        return(NA)  # 如果参数没有第 i 个值，填充 NA
      }
    })
  }
  
  # 将 param_list 转换为数据框
  params_df <- as.data.frame(param_list)
  rownames(params_df) <- NULL
  
  model_fit_df <- data.frame(
    Metric = c("Accuracy", "LogL", "AIC", "BIC"),
    Value = c(object$acc, object$ll, object$aic, object$bic)
  )
  
  output_df <- list(params_df, model_fit_df)
  
  return(output_df)
}