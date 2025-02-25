#' rl_generate_d
#' @description
#' This function requires the optimal parameter values obtained through the 
#' `algorithm` package. Once the best parameter values are solved for, they 
#' are incorporated into the reinforcement learning model, allowing the model 
#' to simulate human-like decision-making. The function leverages these optimized 
#' parameters to generate choices that mimic the decision-making process of subjects, 
#' enabling the study of behavior under varying conditions. By integrating the best-fit 
#' parameters from the `algorithm` package, this function offers a powerful tool for 
#' simulating human choices in reinforcement learning contexts.
#' 
#' @param data A data frame containing the raw data. 
#' This data should include the following mandatory columns: 
#' - "sub", "time_line", "L_choice", "R_choice", "choose", "L_reward", "R_reward". 
#' The following arguments allow you to customize the column names used for processing
#' 
#' @param id A numeric value specifying the subject ID for which the model is being analyzed. 
#' The value should correspond to an entry in the "sub" column, which must contain the subject IDs. 
#' Provide the subject ID as a number.
#' e.g., `id = 18`
#' 
#' @param initial_value A numeric value representing the subject's initial expected value for each stimulus's reward. 
#' If this value is not set, the subject will use the reward received after the first trial as the initial value for that stimulus. 
#' In other words, the learning rate for the first trial is 100%. 
#' Provide the initial value as a number 
#' default: `initial_value = NA`
#' e.g., `initial_value = 0`
#' 
#' @param threshold A numeric value specifying the number of initial trials during which the subject makes random choices 
#' rather than choosing based on the values of the options. This occurs because the subject has not yet learned the values of the options. 
#' For example, threshold = 20 means the subject will make completely random choices for the first 20 trials.
#' Provide the value as a number. 
#' default: `threshold = 1`
#' 
#' @param n_params The number of free parameters in the model. 
#' @param n_trials The total number of trials in the experiment.
#'
#' 
#' 
#' @param lambda An additional parameter that may be used in these functions. 
#' Provide the value as a vector 
#' e.g., `lambda = c(0.4, 0.7, 20, 60)`
#' 
#' @param gamma A parameter used in the `util_func` (Utility Function), often 
#' referred to as the discount rate. For example, in the utility equation 
#' `utility = gamma * reward`, if gamma < 1, it indicates that people discount 
#' the objective reward. 
#' Provide the value as a vector 
#' e.g., `gamma = c(0.7)`
#' 
#' @param eta A parameter used in the `rate_func` (Learning Rate Function), 
#' representing the rate at which the subject updates the difference between the reward and the expected value in the subject's mind. 
#' In the TD model, there is a single learning rate throughout the experiment. 
#' In the RSTD model, two different learning rates are used when the reward is higher or lower than the expected value.
#' Provide the value as a vector 
#' e.g., `eta = c(0.3, 0.7)`
#' 
#' @param epsilon A parameter used in the `expl_func` (Exploration Function), 
#' determining whether the subject makes decisions based on the relative values of the left and right options, 
#' or chooses completely randomly. 
#' For example, when epsilon = 0.1, it means the subject has a 10% chance of making a completely random choice 
#' and a 90% chance of choosing based on the values of the options.
#' Provide the value as a vector 
#' e.g., `epsilon = c(0.1)`
#' 
#' @param tau A parameter used in the `prob_func` (Soft-Max Function), representing the sensitivity of the subject to the value difference when making decisions. 
#' It determines the probability of selecting the left option versus the right option based on their values. A larger value of tau indicates greater sensitivity 
#' to the value difference between the options. In other words, even a small difference in value will make the subject more likely to choose the higher-value option. 
#' Provide the value as a vector 
#' e.g., `tau = c(0.5)`
#' 
#' @param util_func The function for the utility gamma, which you can customize
#' @param rate_func The function for the learning rate eta, which you can customize
#' @param expl_func The function for the epsilon greedy, which you can customize
#' @param prob_func The function for the temperature tau, which you can customize.
#' 
#' @param sub A string specifying the name of the column that contains the subject ID.  
#' Provide the name of the column as a character string  
#' e.g., `sub = "Subject_ID"`
#' 
#' @param time_line A vector specifying the name of the column that the sequence of the experiment. 
#' This argument defines how the experiment is structured, such as whether it is organized by "Block" with breaks in between, and multiple trials within each block. 
#' Provide the sequence as a character vector, 
#' e.g., `time_line = c("Block", "Trial")`
#' 
#' @param L_choice A string specifying the name of the column that represents the left choice. 
#' Provide the name of the column as a character string 
#' e.g., `L_choice = "Left_Choice"`
#' 
#' @param R_choice A string specifying the name of the column that represents the right choice. 
#' Provide the name of the column as a character string 
#' e.g., `R_choice = "Right_Choice"`
#' 
#' @param sub_choose A string specifying the name of the column that represents the choice made by the subject. 
#' Provide the name of the column as a character string 
#' e.g., `sub_choose = "Choose"`
#' @param L_reward A string specifying the name of the left column. 
#' Provide the name of the column as a character string 
#' e.g., `L_reward = "Left_reward"`
#' @param R_reward A string specifying the name of the right column. 
#' Provide the name of the column as a character string 
#' e.g., `R_reward = "Right_reward"`
#' 
#' @param var1 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var1 = "Extra_Var1"`
#' 
#' @param var2 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var2 = "Extra_Var2"`
#'
#' 
#' 
#' @param softmax A logical value indicating whether to use the softmax function. 
#' When softmax = TRUE, the value of each option influences the probability of selecting that option. 
#' Higher values increase the probability of selecting that option. 
#' When softmax = FALSE, the subject will always choose the option with the higher value, with no possibility of selecting the lower-value option.
#' Provide the value as a logical (TRUE or FALSE).
#' default: `softmax = TRUE`
#' 
#' @param seed A numeric value to set the random seed. 
#' This ensures that the results are reproducible and remain the same each time the function is run.
#' Provide the value as a number. 
#' default: `seed = 123`
#' 
#' @param digits_1 The number of decimal places to retain for values related to the value function. 
#' The default is 2.
#' 
#' @param digits_2 The number of decimal places to retain for values related to the action function. 
#' The default is 5.
#' 
#' @return generated data
#' @export
#' @examples
#' data <- TAFC
#' 
#' simulated <- binaryRL::rl_generate_d(
#'   data = data,
#'   id = 18,
#'   eta = c(0.321, 0.765),
#'   n_params = 2, 
#'   n_trials = 288
#' )
#' 
#' summary(simulated)

rl_generate_d <- function(
    data,
    id,
    initial_value = NA,
    threshold = 1,
    n_params,
    n_trials,

    gamma = 1,
    eta,
    epsilon = NA,
    tau = 1,
    lambda = NA,
    util_func = func_gamma,
    rate_func = func_eta,
    expl_func = func_epsilon,
    prob_func = func_tau,

    sub = "Subject",
    time_line = c("Block", "Trial"),
    L_choice = "L_choice",
    R_choice = "R_choice",
    L_reward = "L_reward",
    R_reward = "R_reward",
    sub_choose = "Choose",
    var1 = NA,
    var2 = NA,
    
    softmax = TRUE,
    seed = 123,
    
    digits_1 = 2,
    digits_2 = 5
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
      epsilon = epsilon,
      lambda = lambda
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
        tau = tau,
        lambda = lambda
      )
      
      temp_data$R_prob[i] <- prob_func(
        L_value = temp_data$L_value[i],
        R_value = temp_data$R_value[i],
        try = temp_data$Try[i],
        var1 = temp_data[[var1]][i],
        var2 = temp_data[[var2]][i],
        LR = "R", 
        tau = tau,
      lambda = lambda
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
      res_data$Sub_Choose[i] == res_data[[L_choice]][i] & 
      res_data$Sub_Choose[i] != res_data[[R_choice]][i]
    ) {
      res_data$L_dir[i] <- 1
      res_data$R_dir[i] <- 0
    } else if (
      res_data$Sub_Choose[i] != res_data[[L_choice]][i] & 
      res_data$Sub_Choose[i] == res_data[[R_choice]][i]
    ) {
      res_data$L_dir[i] <- 0
      res_data$R_dir[i] <- 1
    } else if (
      res_data$Sub_Choose[i] == res_data[[L_choice]][i] & 
      res_data$Sub_Choose[i] == res_data[[R_choice]][i]
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