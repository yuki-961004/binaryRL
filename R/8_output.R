output <- function(
    mode, policy,
    name = NA,
    data, 
    n_params, n_trials, 
    initial_value, threshold, lapse,
    alpha, beta, gamma, eta, epsilon, lambda, pi, tau,
    priors
){
  # 去掉行名
  rownames(data) <- NULL 
  
  params <- list(
    Q1 = initial_value,
    threshold = threshold,
    lapse = lapse,
    
    alpha = c(alpha),
    beta = c(beta),
    gamma = c(gamma),
    eta = c(eta), 
    epsilon = c(epsilon),
    lambda = c(lambda),
    pi = c(pi),
    tau = c(tau)
  )
  
  # 计算正确率
  mean_ACC <- round(mean(data$ACC), digits = 4) * 100
  
  # Log-Likelihood
  sum_logLi <- sum(data$L_logl) + sum(data$R_logl)
  
  # 如果没有输入先验分布, 则说明使用的是MLE
  if (base::is.null(priors)) {
    estimate <- "MLE"
    sum_logPr <- NA
    sum_logPo <- NA
  }
  # 输入了先验分布, 就计算后验概率
  else {
    estimate <- "MAP"
    
    # 避免显式循环和扁平化。直接使用 mapply 同时遍历 priors 和 params。
    log_densities <- base::mapply(
      FUN = function(param_values, prior_fn) {
        # 对参数向量中的每一个值，应用对应的先验函数
        base::sapply(
          X = param_values,
          FUN = prior_fn
        )
      },
      param_values = params[base::names(priors)], # 确保只传入有 prior 的参数
      prior_fn = priors,
      SIMPLIFY = FALSE
    )
    
    # 求和所有对数先验概率密度
    sum_logPr <- base::sum(base::unlist(log_densities))
    
    # Log-Posterior Probability (假设 sum_logLi 已定义)
    sum_logPo <- sum_logLi + sum_logPr
  }
  
  if (mode != "fit") {
    estimate <- NA
  }

  AIC <- 2 * n_params - 2 * sum_logLi
  BIC <- n_params * log(n_trials) - 2 * sum_logLi
  
  res <- list(
    data = data,
    params = params,
    name = name,
    mode = mode,
    policy = policy,
    estimate = estimate,
    acc = mean_ACC,
    ll = sum_logLi,
    lpr = sum_logPr,
    lpo = sum_logPo,
    aic = AIC,
    bic = BIC
  )
  
  class(res) <- c("binaryRL")
  
  return(res)
}
