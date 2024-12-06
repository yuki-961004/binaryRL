#' rl_update_v
#'
#' @param data Data containing only one type of stimulus
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param initial_value The initial value you assign to a stimulus, defaulting to NA
#' @param var1 extra variable 1
#' @param var2 extra variable 2
#' @param lambda the eta or gamma could be divided into different intervals.
#' @param gamma In the utility model, it is assumed that all rewards will be discounted
#' @param eta In the RSTD model, the learning rate is different for positive and negative conditions.
#' @param util_func The function for the discount rate β, which you can customize
#' @param rate_func The function for the learning rate η, which you can customize
#' @param digits digits
#'
#' @return update value for 1 choice
#' @export

rl_update_v <- function(
    # 输入data frame
  data,
  # 价值更新的时间线, 基于的列
  time_line = c("Block", "Trial"),
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,
  # 被试心中价值初始值
  initial_value = NA,
  # parameters
  gamma = 1,
  lambda = NA,
  eta = c(0.3, 0.7),
  # 价值函数选用示例函数
  util_func = func_gamma,
  rate_func = func_eta,
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
      temp_data$Reward[i] <- temp_data$Reward[i+1]
      # Value是100%学习到了奖励, 所以直接赋值
      temp_data$V_value[i] <- temp_data$Reward[i+1]
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
        reward = temp_data$Reward[i],
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
      temp_data$Reward[i] <- temp_data$Reward[i+1]
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
      reward = temp_data$Reward[i],
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