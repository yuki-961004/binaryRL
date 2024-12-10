#' rl_generate_d
#'
#' @param data Data for each subject
#' @param sub The column name for the subject ID
#' @param L_choice The column name for the left option
#' @param R_choice The column name for the right option
#' @param L_reward The column name for the reward of right option
#' @param R_reward The column name for the reward of left option
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param sub_choose subject choose
#' @param var1 extra variable 1
#' @param var2 extra variable 2
#' @param id the subject being analysised
#' @param initial_value The initial value you assign to a stimulus, defaulting to 0
#' @param softmax use softmax or not, defaulting to TRUE
#' @param seed seed
#' @param threshold How many trials ago were subjects randomly selected?
#' @param lambda the eta or gamma could be divided into different intervals.
#' @param gamma In the Utility model, it is assumed that all rewards will be discounted
#' @param eta In the RSTD model, the learning rate is different for positive and negative conditions.
#' @param epsilon How much the subjects like to try
#' @param tau The τ parameter in the soft-max function, with a default value of 1
#' @param digits_1 digits
#' @param digits_2 digits
#' @param n_params number of parameters
#' @param n_trials number of trails
#' @param util_func The function for the discount rate β, which you can customize
#' @param rate_func The function for the learning rate η, which you can customize
#' @param expl_func Exploration function, which determines how likely the subject is to try randomly
#' @param prob_func The soft-max function, which you can customize.
#'
#' @return generated data
#' @export
#' 
rl_generate_d <- function(
    data,
    sub = "Subject",
    L_choice = "LC",
    R_choice = "LC",
    L_reward = "LC",
    R_reward = "LC",
    time_line = c("Block", "Trial"),
    sub_choose,
    var1 = NA,
    var2 = NA,
    id,
    initial_value = NA,
    softmax = TRUE,
    seed = 123,
    threshold = 15,
    lambda = NA,
    gamma = 1,
    eta = c(0.6, 0.8),
    epsilon = NA,
    tau = 0.5,
    util_func = func_gamma,
    rate_func = func_eta,
    expl_func = func_epsilon,
    prob_func = func_tau,
    digits_1 = 2,
    digits_2 = 5,
    n_params = 3,
    n_trials = 288
){
  # 选择被试
  data <- data[data[[sub]] == id, ]
  
  # 获取 L_choice 和 R_choice 的唯一值
  unique_L <- unique(data[[L_choice]])
  unique_R <- unique(data[[R_choice]])
  
  # 检查L_choice 和R_choice是否包含了一样的选项
  if (!all(unique_L %in% unique_R) || !all(unique_R %in% unique_L)) {
    stop("Error: L_choice and R_choice have different unique values!")
  } else {
    # 如果一致，将唯一值输入到 alternative_choice 列
    alternative_choice <- unique_L
  }
  
  # 把所有选项, 变成新的列. 方便每个选项更新价值
  for (name in alternative_choice) {
    data[[name]] <- NA
  }
  
  ################################## [Arrange] ###################################
  # 基于time_line这个向量, 录入排序向量
  order_vector <- lapply(time_line, function(col) data[[col]])
  
  # 基于排序向量对输入数据集进行排序
  temp_data <- data[do.call(order, order_vector), ]
  
  ############################### [Add Null row] #################################
  # 生成一个与输入数据集相同的单行数据集. 用于存放初始值
  empty_row <- as.data.frame(matrix(ncol = ncol(data), nrow = 1))
  colnames(empty_row) <- colnames(data)
  
  # 在第一行插入一个空行
  temp_data <- rbind(empty_row, temp_data)
  # 新建一列Time_Line
  temp_data$Time_Line <- NA
  
  ################################ [ new col ] ###################################
  # 添加空列 update_v 相关
  temp_data$Reward <- NA
  temp_data$gamma <- NA
  temp_data$R_utility <- NA
  
  temp_data$V_value <- NA
  temp_data$eta <- NA
  temp_data$V_update <- NA
  
  # 添加空列 action_c 相关
  temp_data$L_value <- NA
  temp_data$R_value <- NA
  
  temp_data$Try <- NA
  
  temp_data$L_prob <- NA
  temp_data$R_prob <- NA
  temp_data$Rob_Choose <- NA
  
  # 设置初始值, 如果没有设置初始值, 则赋予一个0
  # 这个初始值并不是很关键, 只要有数字就行
  if (is.na(initial_value)) {
    # update_v 相关
    temp_data$V_value[1] <- 0
    temp_data$R_utility[1] <- 0
    temp_data$V_update[1] <- 0
    # action_c 相关
    temp_data$L_value[1] <- 0
    temp_data$R_value[1] <- 0
    
    # 给每个选项赋予初始值
    for (name in alternative_choice) {
      temp_data[[name]][1] <- 0
    }
  } else {
    # 赋予设定的初始值
    # update_v 相关
    temp_data$V_value[1] <- initial_value
    temp_data$R_utility[1] <- initial_value
    temp_data$V_update[1] <- initial_value
    # action_c 相关
    temp_data$L_value[1] <- initial_value
    temp_data$R_value[1] <- initial_value
    
    # 给每个选项赋予初始值
    for (name in alternative_choice) {
      temp_data[[name]][1] <- initial_value
    }
  }
  ########################### [update row by row] ################################  
  # 逐行更新Value
  for (i in 2:nrow(temp_data)) {
    
    # 记录此时L和R的名字
    L_name <- temp_data[[L_choice]][i]
    R_name <- temp_data[[R_choice]][i]
    # 在上一行找此时左右选项对应的心中的价值
    temp_data$L_value[i] <- temp_data[[L_name]][i - 1]
    temp_data$R_value[i] <- temp_data[[R_name]][i - 1]
    
    ################################ [ 1+ CHOOSE ] #################################
    # 查询此次选择时, 已经选过哪些了
    chosen <- unique(temp_data$Rob_Choose)
    
    # 设置随机种子
    set.seed(seed = seed + i)
    
    temp_data$Try[i] <- expl_func(
      i = i,
      var1 = temp_data[[var1]][i],
      var2 = temp_data[[var2]][i],
      threshold = threshold,
      epsilon = epsilon
    )
    
    # 如果选项都不是第一次出现, 则正常计算概率
    if ((temp_data[[L_choice]][i] %in% chosen) & (temp_data[[R_choice]][i] %in% chosen)) {
      # 基于prob函数计算选择左边和右边的概率
      # 如果选项都不是第一次出现, 则正常计算概率
      
      # 基于prob函数计算选择左边和右边的概率
      temp_data$L_prob[i] <- prob_func(
        L_value = temp_data$L_value[i],
        R_value = temp_data$R_value[i],
        try = temp_data$Try[i],
        var1 = temp_data[[var1]][i],
        var2 = temp_data[[var2]][i],
        LR = "L", 
        tau = tau
      )
      
      temp_data$R_prob[i] <- prob_func(
        L_value = temp_data$L_value[i],
        R_value = temp_data$R_value[i],
        try = temp_data$Try[i],
        var1 = temp_data[[var1]][i],
        var2 = temp_data[[var2]][i],
        LR = "R", 
        tau = tau
      )
      ############################### [ 1st CHOOSE ] #################################
    } else if (!(temp_data[[L_choice]][i] %in% chosen) & (temp_data[[R_choice]][i] %in% chosen)) {
      # 如果左边选项是第一次出现, 则一定选左边  
      temp_data$L_prob[i] <- 1
      temp_data$R_prob[i] <- 0
    } else if ((temp_data[[L_choice]][i] %in% chosen) & !(temp_data[[R_choice]][i] %in% chosen)) {
      # 如果右边选项是第一次出现, 则一定选右边
      temp_data$L_prob[i] <- 0
      temp_data$R_prob[i] <- 1
    } else if (!(temp_data[[L_choice]][i] %in% chosen) & !(temp_data[[R_choice]][i] %in% chosen)) {
      # 如果都是第一次出现, 则随便选
      temp_data$L_prob[i] <- 0.5
      temp_data$R_prob[i] <- 0.5
    }
    ############################### [ PASS VALUE ] #################################  
    # 去上一行找每个选项此时的value
    for (name in alternative_choice) {
      temp_data[[name]][i] <- temp_data[[name]][i - 1]
    }
    
    ################################ [ Soft-Max ] ##################################    
    # 检查是否设定了softmax
    if (!(softmax %in% c(TRUE, FALSE))) {
      stop("softmax TRUE or FALSE?")
      # 如果是softmax = TRUE就基于概率随机选
    } else if (softmax == TRUE) {
      if (!is.numeric(temp_data$L_value[i]) | !is.numeric(temp_data$R_value[i])) {
        stop("An error occurs when softmax == FALSE")
      }
      # 设置随机种子
      set.seed(seed = seed)
      # 基于刚刚的概率, 随机选一个. 而不是谁大选谁
      temp_data$Rob_Choose[i] <- sample(
        c(temp_data[[L_choice]][i], temp_data[[R_choice]][i]), 
        prob = c(temp_data$L_prob[i], temp_data$R_prob[i]),
        size = 1
      ) 
      # 如果softmax = FALSE, 则按照谁大选谁
    } else if (softmax == FALSE) {
      if (!is.numeric(temp_data$L_value[i]) | !is.numeric(temp_data$R_value[i])) {
        stop("An error occurs when softmax == FALSE")
      } else if (temp_data$L_value[i] > temp_data$R_value[i]) {
        # 如果左边大选左边
        temp_data$Rob_Choose[i] <- temp_data[[L_choice]][i]
      } else if (temp_data$L_value[i] < temp_data$R_value[i]) {
        # 如果左边小于右边
        temp_data$Rob_Choose[i] <- temp_data[[R_choice]][i]
      } else if (temp_data$L_value[i] == temp_data$R_value[i]) {
        # 一样大随机选一个(是Single情况), 或者极端情况[Value_L == Value_R]
        temp_data$Rob_Choose[i] <- sample(
          c(temp_data[[L_choice]][i], temp_data[[R_choice]][i]), 
          size = 1
        )
      } 
    }
    ################################ [occurrence] ##################################   
    temp_data$Time_Line[[i]] <- sum(
      temp_data$Rob_Choose == temp_data$Rob_Choose[[i]], 
      na.rm = TRUE
    )
    ################################## [ Reward ] ##################################    
    # 基于选择, 来给予奖励
    if (temp_data$Rob_Choose[i] == temp_data[[L_choice]][i]){
      # 选了左边, 给左的奖励
      temp_data$Reward[i] <- temp_data[[L_reward]][i]
    } else if (temp_data$Rob_Choose[i] == temp_data[[R_choice]][i]) {
      # 选了右边, 给右的奖励
      temp_data$Reward[i] <- temp_data[[R_reward]][i]
    }
    
    ################################ [ update_v ] ##################################     
    # 记录这次选了哪个
    choose <- temp_data$Rob_Choose[i]
    # 看到奖励前, 对该选项预期的奖励, 去上一行找
    temp_data$V_value[i] <- temp_data[[choose]][i - 1]
    
    # 看到reward之后的折扣率, 用util_func选择此时对应的gamma, 计算出R_utility
    gamma_utility <- util_func(
      value = temp_data$V_value[i],
      utility = temp_data$R_utility[i],
      reward = temp_data$Reward[i],
      occurrence = temp_data$Time_Line[i],
      var1 = temp_data[[var1]][i],
      var2 = temp_data[[var2]][i],
      gamma = gamma,
      lambda = lambda
    )
    temp_data$gamma[i] <- as.numeric(gamma_utility[1])
    temp_data$R_utility[i] <- as.numeric(gamma_utility[2])
    
    # 看到reward之后的学习率, 用rate_func选择此时对应的eta
    temp_data$eta[i] <- rate_func(
      value = temp_data$V_value[i],
      utility = temp_data$R_utility[i],
      reward = temp_data$Reward[i],
      occurrence = temp_data$Time_Line[i],
      var1 = temp_data[[var1]][i],
      var2 = temp_data[[var2]][i],
      eta = eta,
      lambda = lambda
    )
    
    # 如果是第一次选这个选项, 则此次学习率为1
    if (is.na(initial_value) & !(choose %in% chosen)) {
      temp_data$eta[i] <- 1
      temp_data$V_update[i] <- temp_data$V_value[i] + 
        temp_data$eta[i] * (temp_data$R_utility[i] - temp_data$V_value[i])
      temp_data[[choose]][i] <- temp_data$V_update[i]
      # 如果这次的选项是选过的, 正常按照eta更新价值
    } else {
      temp_data$V_update[i] <- temp_data$V_value[i] + 
        temp_data$eta[i] * (temp_data$R_utility[i] - temp_data$V_value[i])
      temp_data[[choose]][i] <- temp_data$V_update[i]  
    } 
  }
  ############################## [delete first row] ############################## 
  # 删除第一行赋予的初始值
  res_data <- temp_data[-1, ]
  # round
  res_data$L_prob <- round(res_data$L_prob, digits_2)
  res_data$R_prob <- round(res_data$R_prob, digits_2)
  
  res_data$L_value <- round(res_data$L_value, digits_1)
  res_data$R_value <- round(res_data$R_value, digits_1)
  
  res_data$V_value <- round(res_data$V_value, digits_1)
  res_data$V_update <- round(res_data$V_update, digits_1)
  
  for (name in alternative_choice) {
    res_data[[name]] <- round(res_data[[name]], digits_1)
  }
  
  # 如果输入了sub_choose, 就计算rob_choose和sub_choose的匹配度
  if (is.character(sub_choose)) {
    # 重新命名成Sub_Choose
    colnames(res_data)[colnames(res_data) == sub_choose] <- "Sub_Choose"
  }
  
  res_data$ACC <- NA
  
  for (i in 1:nrow(res_data)){
    if (res_data$Sub_Choose[i] == res_data$Rob_Choose[i]) {
      res_data$ACC[i] <- 1
    } else if (res_data$Sub_Choose[i] != res_data$Rob_Choose[i]) {
      res_data$ACC[i] <- 0
    } else {
      res_data$ACC[i] <- "ERROR"
    }
  }
  
  res_data$L_dir <- NA
  res_data$R_dir <- NA
  
  for (i in 1:nrow(res_data)){
    if (
      res_data$Sub_Choose[i] == res_data$LC[i] & 
      res_data$Sub_Choose[i] != res_data$RC[i]
    ) {
      res_data$L_dir[i] <- 1
      res_data$R_dir[i] <- 0
    } else if (
      res_data$Sub_Choose[i] != res_data$LC[i] & 
      res_data$Sub_Choose[i] == res_data$RC[i]
    ) {
      res_data$L_dir[i] <- 0
      res_data$R_dir[i] <- 1
    } else if (
      res_data$Sub_Choose[i] == res_data$LC[i] & 
      res_data$Sub_Choose[i] == res_data$RC[i]
    ) {
      res_data$L_dir[i] <- 0
      res_data$R_dir[i] <- 0
    } else {
      res_data$L_dir[i] <- "ERROR"
      res_data$R_dir[i] <- "ERROR"
    }
  }
  
  res_data$L_logl <- round(
    res_data$L_dir * log(res_data$L_prob + 1e-10), 
    digits_2
  )
  res_data$R_logl <- round(
    res_data$R_dir * log(res_data$R_prob + 1e-10), 
    digits_2
  )
  
  ################################# [output] #####################################
  
  params <- list(
    lambda = c(lambda),
    gamma = c(gamma),
    eta = c(eta), 
    epsilon = c(epsilon),
    tau = c(tau)
  )
  
  mean_ACC <- round(mean(res_data$ACC), 4) * 100
  sum_LL <- round(sum(res_data$L_logl) + sum(res_data$R_logl), digits = 2)
  AIC <- round(2 * n_params - 2 * sum_LL, digits = 2)
  BIC <- round(n_params * log(n_trials) - 2 * sum_LL, digits = 2)
  
  res <- list(
    data = res_data,
    params = params,
    acc = mean_ACC,
    ll = sum_LL,
    aic = AIC,
    bic = BIC
  )
  
  class(res) <- c("binaryRL")
  
  return(res)
}