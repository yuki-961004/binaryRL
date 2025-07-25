#' S3method summary
#'
#' @param object binaryRL result
#' @param ... others
#'
#' @return summary
#' @export
#' 
summary.binaryRL <- function(object, ...) {

  object$data <- NULL
  
  # EE trade-off
  # positive(negative) Q1
  Q1 <- object$params$Q1
  # epsilon
  threshold <- object$params$threshold
  epsilon <- round(object$params$epsilon, 3)
  lambda <- round(object$params$lambda, 3)
  # UCB
  pi <- round(object$params$pi, 3)
  # softmax
  tau <- round(object$params$tau, 3)
  # value function
  gamma <- round(object$params$gamma, 3)
  eta <- round(object$params$eta, 3)
  # extra
  alpha <- round(object$params$alpha, 3)
  beta <- round(object$params$beta, 3)
  
  # 是否设定了初始值
  if (is.na(Q1)) {
    Q1 <- "Initial reward received"
  }
  
  # 是哪种epsilon
  if (threshold == 1 & is.na(epsilon) & is.na(lambda)) {
    EE_tradeof <- "off"
  }
  else if (threshold > 1 & is.na(epsilon) & is.na(lambda)) {
    EE_tradeof <- "\u03B5-first"
  }
  else if (threshold == 1 & !(is.na(epsilon)) & is.na(lambda)) {
    EE_tradeof <- "\u03B5-greedy"
  }
  else if (threshold == 1 & is.na(epsilon) & !(is.na(lambda))) {
    EE_tradeof <- "\u03B5-decreasing"
  }
  else {
    EE_tradeof <- "unknown"
  }
  
  # UCB
  if (is.na(pi) | pi == 0) {
    UCB <- "off"
  }
  else {
    UCB <- "on"
  }
  
  # soft-max
  if (is.na(tau)) {
    softmax <- "off"
  } 
  else {
    softmax <- "on"
  }
  
  message(
    "Results of ", object$name, " Model: ", 
    "\n"
  )
  
  message(
    "Exploration and Exploitation Trade-off:\n",
    "  ", "Initial Values: ", Q1, "\n",
    "  ", "Exploration Strategy: ", EE_tradeof, "\n",
    "  ", "Upper-Confidence-Bound: ", UCB, "\n",
    "  ", "Soft-Max: ", softmax, "\n",
    
    "\n"
  )
  
  message(
    "Model Fit:\n",
    # Indent model fit metrics
    "  ", "Accuracy: ", object$acc, "%\n",
    "  ", "LogL: ", object$ll, "\n",
    "  ", "AIC: ", object$aic, "\n",
    "  ", "BIC: ", object$bic,"\n"
  )
  
  message(
    "Free Parameters:\n",
    
    # Indent parameters for better readability
    "  ", "\u03B1: ", paste0(round(object$params$alpha, 3), collapse = ", "), "\n",
    "  ", "\u03B2: ", paste0(round(object$params$beta, 3), collapse = ", "), "\n",
    "  ", "\u03B3: ", paste0(round(object$params$gamma, 3), collapse = ", "), "\n",
    "  ", "\u03B7: ", paste0(round(object$params$eta, 3), collapse = ", "), "\n",
    "  ", "\u03B5: ", paste0(round(object$params$epsilon, 3), collapse = ", "), "\n",
    "  ", "\u03BB: ", paste0(round(object$params$lambda, 3), collapse = ", "), "\n",
    "  ", "\u03C0: ", paste0(round(object$params$pi, 3), collapse = ", "), "\n",
    "  ", "\u03C4: ", paste0(round(object$params$tau, 3), collapse = ", "), "\n"
  )
  
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
    param_list[[paste("Value", i, sep = "")]] <- sapply(
      object$params, function(param) {
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