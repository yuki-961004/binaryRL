#' rl_update_v
#'
#' @param data A data frame containing only one stimulate. 
#' This data should include the following mandatory columns: 
#' "time_line", "L_choice", "R_choice", "choose", "reward". 
#' The following arguments allow you to customize the column names used for processing
#' 
#' @param initial_value A numeric value representing the subject's initial expected value for each stimulus's reward. 
#' If this value is not set, the subject will use the reward received after the first trial as the initial value for that stimulus. 
#' In other words, the learning rate for the first trial is 100%. 
#' Provide the initial value as a number 
#' default: `initial_value = NA`
#' e.g., `initial_value = 0`
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
#' @param lambda An additional parameter that may be used in these functions. 
#' Provide the value as a vector 
#' e.g., `lambda = c(0.4, 0.7, 20, 60)`
#' 
#' @param util_func The function for the utility gamma, which you can customize
#' @param rate_func The function for the learning rate eta, which you can customize
#'
#' @param time_line A vector specifying the name of the column that the sequence of the experiment. 
#' This argument defines how the experiment is structured, such as whether it is organized by "Block" with breaks in between, and multiple trials within each block. 
#' Provide the sequence as a character vector, 
#' e.g., `time_line = c("Block", "Trial")`
#' 
#' @param reward A string specifying the name of the column that represents the reward associated with the subject's choice. 
#' Provide the name of the column as a character string 
#' e.g., `reward = "Reward"`
#' 
#' @param var1 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var1 = "Extra_Var1"`
#' 
#' @param var2 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var2 = "Extra_Var2"`
#'
#' @param digits digits
#'
#' @return update value for 1 choice
#' @export

rl_update_v <- function(
  # 只包含一种刺激的data
  data,
  # 被试心中价值初始值
  initial_value,

  # parameters
  gamma = 1,
  eta,
  lambda = NA,
  # 价值函数选用示例函数
  util_func = func_gamma,
  rate_func = func_eta,

  # 价值更新的时间线, 基于的列
  time_line,
  # 奖励所在的列
  reward,
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,
  
  # 小数位数
  digits = 2
  ################################# [function start] #############################
){
  ################################## [Arrange] ###################################
  # 基于time_line这个向量, 录入排序向量
  order_vector <- lapply(time_line, function(col) data[[col]])
  
  # 基于排序向量对输入数据集进行排序
  temp_data <- data[do.call(order, order_vector), ]
  
  ############################## [Add Null row] #################################
  # 生成一个与输入数据集相同的单行数据集. 用于存放初始值
  empty_row <- as.data.frame(matrix(ncol = ncol(data), nrow = 1))
  colnames(empty_row) <- colnames(data)
  
  # 在第一行插入一个空行
  temp_data <- rbind(empty_row, temp_data)
  temp_data$Time_Line <- seq(from = 0, to = nrow(data))
  
  ############################# [ update row by row] #############################
  # 添加空列
  temp_data$gamma <- NA
  temp_data$R_utility <- NA
  temp_data$V_value <- NA
  temp_data$eta <- NA
  temp_data$V_update <- NA
  
  # 逐行更新Value
  for (i in 1:nrow(temp_data)) {
    # 如果是第一列
    # 而且没有设置初始值, 则见到的第一个值会被100%学到
    # 之后才回根据学习率对这个值进行矫正. 
    if (i == 1 & is.na(initial_value)) {
      # 奖励是第一行(i = 2)的奖励
      temp_data[[reward]][i] <- temp_data[[reward]][i+1]
      # Value是100%学习到了奖励, 所以直接赋值
      temp_data$V_value[i] <- temp_data[[reward]][i+1]
      # 如果输入了var1, 就赋值
      if (is.character(var1)) {
        temp_data[[var1]][i] <- temp_data[[var1]][i+1]
      }
      # 如果输入了var2, 就赋值
      if (is.character(var2)) {
        temp_data[[var2]][i] <- temp_data[[var2]][i+1]
      }
      
      # 使用gamma_func选择此时对应的gamma, 然后计算出temp
      gamma_utility <- util_func(
        value = temp_data$V_value[i],
        utility = temp_data$R_utility[i],
        reward = temp_data[[reward]][i],
        var1 = temp_data[[var1]][i],
        var2 = temp_data[[var2]][i],
        occurrence = temp_data$Time_Line[i],
        gamma = gamma,
        lambda = lambda
      )
      
      temp_data$gamma[i] <- as.numeric(gamma_utility[1])
      temp_data$R_utility[i] <- as.numeric(gamma_utility[2])
      # 设定学习率此时是100%, 而不使用rate_func
      temp_data$eta[i] <- 1
      temp_data$V_update[i] <- as.numeric(gamma_utility[2])
      
      # 如果是第一次, 但是给了初始值, 那么就赋予上初始值, 给予正常的奖励 
    } else if (i == 1 & !(is.na(initial_value))) {
      temp_data[[reward]][i] <- temp_data[[reward]][i+1]
      # 如果输入了var1, 就赋值
      if (is.character(var1)) {
        temp_data[[var1]][i] <- temp_data[[var1]][i+1]
      }
      # 如果输入了var2, 就赋值
      if (is.character(var2)) {
        temp_data[[var2]][i] <- temp_data[[var2]][i+1]
      }
      # 赋予初始值
      temp_data$V_value[i] <- initial_value
      temp_data$R_utility[i] <- initial_value
      temp_data$V_update[i] <- initial_value
      # 除此之外, 是i >= 2的情况. 则Value是上一次的Update
    } else {
      temp_data$V_value[i] <- temp_data$V_update[i - 1]
    }
    
    # 使用gamma_func选择此时对应的gamma, 然后计算出temp
    gamma_utility <- util_func(
      value = temp_data$V_value[i],
      utility = temp_data$R_utility[i],
      reward = temp_data[[reward]][i],
      var1 = temp_data[[var1]][i],
      var2 = temp_data[[var2]][i],
      occurrence = temp_data$Time_Line[i],
      gamma = gamma,
      lambda = lambda
    )
    temp_data$gamma[i] <- as.numeric(gamma_utility[1])
    temp_data$R_utility[i] <- as.numeric(gamma_utility[2])
    
    # 使用rate_func选择此时对应的eta
    temp_data$eta[i] <- rate_func(
      value = temp_data$V_value[i],
      utility = temp_data$R_utility[i],
      reward = temp_data$Reward[i],
      var1 = temp_data[[var1]][i],
      var2 = temp_data[[var2]][i],
      occurrence = temp_data$Time_Line[i],
      eta = eta,
      lambda = lambda
    )
    
    # 从第二次才是真的开始学习了
    if (i >= 2) {
      # 计算此次update的值
      temp_data$V_update[i] <- temp_data$V_value[i] + 
        temp_data$eta[i] * (temp_data$R_utility[i] - temp_data$V_value[i])
    }
  }
  
  ############################## [delete first row] ############################## 
  # 删除第一行赋予的初始值
  res_data <- temp_data[-1, ]
  # 对于V取两位小数
  res_data$V_value <- round(res_data$V_value, digits)
  res_data$V_update <- round(res_data$V_update, digits)
  
  return(res_data)
}