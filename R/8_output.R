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
  sum_logLi <- round(sum(data$L_logl) + sum(data$R_logl), digits = 2)
  
  # 如果没有输入先验分布, 则说明使用的是MLE
  if (is.null(priors)) {
    estimate <- "MLE"
    sum_logPr <- NA
    sum_logPo <- NA
  }
  # 输入了先验分布, 就计算后验概率
  else {
    estimate <- "MAP"
    # 找到priors定义了几类参数的先验概率
    priors_name <- unique(names(priors))
    
    # 存储自由参数的数值
    params_value <- c()
    
    # 定义了先验概率的才是自由参数
    for (param_name in priors_name) {
      # 把带入run_m的自由参数存在params_value中
      params_value <- c(params_value, params[[param_name]])
    }
    
    # 初始化Log Prior Probability
    logPr <- c()
    
    for (i in 1:length(priors)) {
      # 使用先验分布概率, 计算该参数对应的概率密度
      logPr[i] <- priors[[i]](params_value[i])
    }
    
    # 求和每个参数对应的log先验概率密度
    sum_logPr <- sum(logPr)
    
    # Log-Posterior Probability
    sum_logPo <- sum_logLi + sum_logPr
  }
  
  if (mode != "fit") {
    estimate <- NA
  }

  AIC <- round(2 * n_params - 2 * sum_logLi, digits = 2)
  BIC <- round(n_params * log(n_trials) - 2 * sum_logLi, digits = 2)
  
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
