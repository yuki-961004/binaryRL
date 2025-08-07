decision_making <- function(
    mode = "fit",
    policy = "off",
    data, 
    options,
    sub_choose, rob_choose,
    seed = 123, 
    
    initial_value = NA,
    threshold = 1,
    lapse = 0.02,
    
    alpha, beta, gamma, eta, epsilon, lambda, pi, tau, 
    
    expl_func = func_epsilon,
    bias_func = func_pi,
    prob_func = func_tau,
    util_func = func_gamma,
    rate_func = func_eta,
    
    L_choice = "L_choice", R_choice = "R_choice",
    L_reward = "L_reward", R_reward = "R_reward", 
    var1 = NA, var2 = NA
){
########################### [update row by row] ################################  
  
  # 逐行更新Value
  for (i in 2:nrow(data)) {
    
    # 记录此时L和R的名字
    L_name <- data[[L_choice]][i]
    R_name <- data[[R_choice]][i]
    
    # 查询此时左选项已经出现过几次了
    data$L_freq[i] <- 
      sum(data[[L_choice]][1:(i)] == L_name, na.rm = TRUE) + 
      sum(data[[R_choice]][1:(i)] == L_name, na.rm = TRUE)
    # 计算此时右选项已经出现过几次了
    data$R_freq[i] <- 
      sum(data[[L_choice]][1:(i)] == R_name, na.rm = TRUE) + 
      sum(data[[R_choice]][1:(i)] == R_name, na.rm = TRUE)
    
    # 计算此时左选项被选了几次
    data$L_pick[i] <- sum(data[[rob_choose]] == L_name, na.rm = TRUE)
    # 计算此时右选项被选了几次
    data$R_pick[i] <- sum(data[[rob_choose]] == R_name, na.rm = TRUE)
    
    # 在上一行找此时左右选项对应的心中的价值
    data$L_value[i] <- data[[L_name]][i - 1]
    data$R_value[i] <- data[[R_name]][i - 1]
    
################################## [action] ####################################  
    
    # 查询此次选择时, 已经选过哪些了
    chosen <- unique(data[[rob_choose]])
    
    # 设置随机种子
    set.seed(seed = seed + i)
    
################################## [epsilon] ###################################
    
    # epsilon: 确定是否需要随机选择(探索)
    data$Try[i] <- expl_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      
      threshold = threshold,
      epsilon = epsilon,
      lambda = lambda,
      alpha = alpha,
      beta = beta
    )
    
#################################### [pi] ######################################   
    
    # pi: 对选项价值的偏差值, 默认和被被选次数成反比例
    data$L_bias[i] <- bias_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      
      LR = "L",
      
      pi = pi,
      alpha = alpha,
      beta = beta
    )
    
    data$R_bias[i] <- bias_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      
      LR = "R",
      
      pi = pi,
      alpha = alpha,
      beta = beta
    )
    
################################### [tau] ######################################
    
    # tau: 左右选项备选的概率
    data$L_prob[i] <- prob_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i] + data$L_bias[i],
      R_value = data$R_value[i] + data$R_bias[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      
      try = data$Try[i],
      LR = "L",
      
      lapse = lapse,
      tau = tau,
      alpha = alpha,
      beta = beta
    )
    
    data$R_prob[i] <- prob_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i] + data$L_bias[i],
      R_value = data$R_value[i] + data$R_bias[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      
      try = data$Try[i],
      LR = "R",
      
      lapse = lapse,
      tau = tau,
      alpha = alpha,
      beta = beta
    )
    
############################### [ PASS VALUE ] #################################  
    
    # 查询列名等于选项名的列记录该选项之前的价值
    for (name in options) {
      data[[name]][i] <- data[[name]][i - 1]
    }
    
############################# [ action select ] ################################    
    
    # 如果是off-policy, 以人类的选择作为被更新的价值
    if (policy == "off") {
      data[[rob_choose]][i] <- data[[sub_choose]][i] 
    }
    # 如果是on-policy, 机器人自己做选择, 更新自己选择的价值
    else if (policy == "on") {
      data[[rob_choose]][i] <- sample(
        x = c(data[[L_choice]][i], data[[R_choice]][i]), 
        prob = c(data$L_prob[i], data$R_prob[i]),
        size = 1
      )
    }

################################ [occurrence] ##################################   
    
    # 计算这次是第几次选了这个选项
    data$Occurrence[[i]] <- sum(
      data[[rob_choose]] == data[[rob_choose]][[i]], 
      na.rm = TRUE
    )
    
################################## [ Reward ] ##################################    
    
    # 基于选择, 来给予奖励
    if (data[[rob_choose]][i] == data[[L_choice]][i]){
      # 选了左边, 给左的奖励
      data$Reward[i] <- data[[L_reward]][i]
    } else if (data[[rob_choose]][i] == data[[R_choice]][i]) {
      # 选了右边, 给右的奖励
      data$Reward[i] <- data[[R_reward]][i]
    }
    
################################## [ value ] ###################################     
    
    # 记录这次选了哪个
    choose <- data[[rob_choose]][i]
    # 看到奖励前, 对该选项预期的奖励, 去上一行找
    data$V_value[i] <- data[[choose]][i - 1]
    
################################# [ gamma ] #################################### 
    
    # gamma: 用幂函数将物理量reward转化成心理量utility
    gamma_utility <- util_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      
      value = data$V_value[i],
      utility = data$R_utility[i],
      reward = data$Reward[i],
      occurrence = data$Occurrence[i],
      
      gamma = gamma,
      alpha = alpha,
      beta = beta
    )
    data$gamma[i] <- as.numeric(gamma_utility[[1]])
    data$R_utility[i] <- as.numeric(gamma_utility[[2]])
    
################################## [ eta ] #####################################     
    
    # eta: 基于Rescorla-Wagner Model更新价值
    data$eta[i] <- rate_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      
      value = data$V_value[i],
      utility = data$R_utility[i],
      reward = data$Reward[i],
      occurrence = data$Time_Line[i],
      
      eta = eta,
      alpha = alpha,
      beta = beta
    )
    
############################ [1st Learning Rate] ############################### 
    
    # 如果没有设置初始值, 且是第一次选这个选项
    if (is.na(initial_value) & !(choose %in% chosen)) {
      # 则此次学习率为1
      data$eta[i] <- 1
      # 以第一次见到的价值作为初始值
      data$V_update[i] <- data$V_value[i] + 
        data$eta[i] * (data$R_utility[i] - data$V_value[i])
      data[[choose]][i] <- data$V_update[i]
    } 
    else {
      # 以设定的值为初始值, 且依据设定的eta更新价值
      data$V_update[i] <- data$V_value[i] + 
        data$eta[i] * (data$R_utility[i] - data$V_value[i])
      data[[choose]][i] <- data$V_update[i]  
    }
  }
  
  # 删除第一行赋予的初始值
  data <- data[-1, ]
  
  return(data)
}