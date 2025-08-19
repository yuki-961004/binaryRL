decision_making_r <- function(
    mode = "fit",
    policy = "off",
    data, 
    options,
    seed = 123, 
    
    sub_choose, rob_choose,
    L_choice = "L_choice", R_choice = "R_choice",
    L_reward = "L_reward", R_reward = "R_reward", 
    var1 = NA_character_, var2 = NA_character_,
    
    initial_value = NA_real_,
    threshold = 1,
    lapse = 0.02,
    
    alpha, beta, gamma, eta, epsilon, lambda, pi, tau, 
    
    expl_func = func_epsilon,
    bias_func = func_pi,
    prob_func = func_tau,
    util_func = func_gamma,
    rate_func = func_eta
){
  # 用于记录每个选项作为刺激呈现次数的计数器
  stim_freq <- stats::setNames(object = rep(0L, length(options)), nm = options)
  
  # 用于记录每个选项被选择次数的计数器
  pick_counts <- stats::setNames(object = rep(0L, length(options)), nm = options)
  
########################### [update row by row] ################################  
  
  # 逐行更新Value
  for (i in 2:nrow(data)) {
    
    # 记录此时L和R的名字
    L_name <- data[[L_choice]][i]
    R_name <- data[[R_choice]][i]
    shown_name <- unique(c(L_name, R_name))
    
    # 在计数器中给这两个刺激的呈现次数 +1, 仅针对不重复的情况
    stim_freq[shown_name] <- stim_freq[shown_name] + 1
    
    # 查询此时左选项已经出现过几次了
    data$L_freq[i] <- stim_freq[L_name]
    # 计算此时右选项已经出现过几次了
    data$R_freq[i] <- stim_freq[R_name]
    
    # 计算此时左选项被选了几次
    data$L_pick[i] <- pick_counts[L_name]
    # 计算此时右选项被选了几次
    data$R_pick[i] <- pick_counts[R_name]
    
    # 在上一行找此时左右选项对应的心中的价值
    data$L_value[i] <- data[[L_name]][i - 1]
    data$R_value[i] <- data[[R_name]][i - 1]
    
################################# [ epsilon ] ##################################
    
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
    
################################### [ pi ] #####################################   
    
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
    
################################## [ tau ] #####################################
    
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
    data[i, options] <- data[i - 1, options]
    
############################# [ on/off policy ] ################################    
    
    # 设置随机种子
    set.seed(seed = seed + i)

    # off-policy [Q-learning]: 更新被人类选择的选项的价值
    if (policy == "off") {
      data[[rob_choose]][i] <- data[[sub_choose]][i] 
    }
    # on-policy [SARSA]: 更新被机器人选择的选项的价值
    else if (policy == "on") {
      data[[rob_choose]][i] <- sample(
        x = c(data[[L_choice]][i], data[[R_choice]][i]), 
        prob = c(data$L_prob[i], data$R_prob[i]),
        size = 1
      )
    }

############################# [ chosen count ] #################################  

    # 记录这次选了哪个
    choose <- data[[rob_choose]][i]

    # 计算这次是第几次选了这个选项
    data$Occurrence[[i]] <- pick_counts[data[[rob_choose]][i]]

    # 对被选的选项, 在计数器上+1
    pick_counts[data[[rob_choose]][i]] <- pick_counts[data[[rob_choose]][i]] + 1
     
################################## [ Reward ] ##################################    
    
    # 基于选择, 来给予奖励
    if (data[[rob_choose]][i] == data[[L_choice]][i]){
      # 选了左边, 给左的奖励
      data$Reward[i] <- data[[L_reward]][i]
    } 
    else if (data[[rob_choose]][i] == data[[R_choice]][i]) {
      # 选了右边, 给右的奖励
      data$Reward[i] <- data[[R_reward]][i]
    }
    
################################# [ gamma ] ####################################     
    
    # 看到奖励前, 对该选项预期的奖励, 去上一行找
    data$V_value[i] <- data[[choose]][i - 1]
    
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
      occurrence = data$Occurrence[i],
      
      eta = eta,
      alpha = alpha,
      beta = beta
    )
    
########################## [ Rescorla-Wagner Model ] ########################### 
    
    # 如果没有设置初始值, 且是第一次选这个选项
    if (is.na(initial_value) & data$Occurrence[[i]] == 0) {
      # 第一次的学习率强制为1
      data$eta[i] <- 1
      # Rescorla-Wagner Model
      data$V_update[i] <- data$V_value[i] + 
        data$eta[i] * (data$R_utility[i] - data$V_value[i])
      # 传递更新后的价值回原始表格
      data[[choose]][i] <- data$V_update[i]
    } 
    else {
      # Rescorla-Wagner Model
      data$V_update[i] <- data$V_value[i] + 
        data$eta[i] * (data$R_utility[i] - data$V_value[i])
      # 传递更新后的价值回原始表格
      data[[choose]][i] <- data$V_update[i]  
    }
  }
  
  return(data)
}