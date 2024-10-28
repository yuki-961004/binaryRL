#' rl_update_v
#'
#' @param data Data containing only one type of stimulus
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param initial_value The initial value you assign to a stimulus, defaulting to NA
#' @param beta In the utility model, it is assumed that all rewards will be discounted
#' @param epsilon In the WXT model, the discount rate is divided into different intervals.
#' @param eta In the RSTD model, the learning rate is different for positive and negative conditions.
#' @param beta_func The function for the discount rate β, which you can customize
#' @param eta_func The function for the learning rate η, which you can customize
#'
#' @return update value for 1 choice
#' @export

rl_update_v <- function(
    # 输入data frame
  data,
  # 价值更新的时间线, 基于的列
  time_line = c("Block", "Trial"),
  # 被试心中价值初始值
  initial_value = 0,
  # parameters
  beta = 1,
  epsilon = NA,
  eta = c(0.3, 0.7),
  # 价值函数选用示例函数
  beta_func,
  eta_func
################################# [function start] #############################
){
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
  temp_data$Time_Line <- seq(from = 0, to = nrow(data))
  
############################# [ update row by row] #############################
  # 添加空列
  temp_data$V_value <- NA
  temp_data$beta <- NA
  temp_data$V_temp <- NA
  temp_data$eta <- NA
  temp_data$V_update <- NA
  
  # 逐行更新Value
  for (i in 1:nrow(temp_data)) {
    # 如果是第一列
    # 而且没有设置初始值, 则见到的第一个值会被100%学到
    # 之后才回根据学习率对这个值进行矫正. 
    if (i == 1 & is.na(initial_value)) {
      temp_data$Reward[i] <- temp_data$Reward[i+1]
      temp_data$V_value[i] <- temp_data$Reward[i+1]
      
      # 使用beta_func选择此时对应的beta, 然后计算出temp
      beta_temp <- beta_func(
        value = temp_data$V_value[i],
        temp = temp_data$V_temp[i],
        reward = temp_data$Reward[i],
        occurrence = temp_data$Time_Line[i],
        beta = beta,
        epsilon = epsilon
      )
      temp_data$beta[i] <- as.numeric(beta_temp[1])
      
      temp_data$V_temp[i] <- as.numeric(beta_temp[2])
      temp_data$eta[i] <- 1
      temp_data$V_update[i] <- as.numeric(beta_temp[2])
      # 如果是第一次, 但是给了初始值, 那么就赋予上初始值, 给予正常的奖励 
    } else if (i == 1 & !(is.na(initial_value))) {
      temp_data$Reward[i] <- temp_data$Reward[i+1]
      temp_data$V_value[i] <- initial_value
      temp_data$V_temp[i] <- initial_value
      temp_data$V_update[i] <- initial_value
      # 除此之外, 就是正常的情况了. 基于上一行进行更新
    } else {
      temp_data$V_value[i] <- temp_data$V_update[i - 1]
    }
    
    # 使用beta_func选择此时对应的beta, 然后计算出temp
    beta_temp <- beta_func(
      value = temp_data$V_value[i],
      temp = temp_data$V_temp[i],
      reward = temp_data$Reward[i],
      occurrence = temp_data$Time_Line[i],
      beta = beta,
      epsilon = epsilon
    )
    temp_data$beta[i] <- as.numeric(beta_temp[1])
    temp_data$V_temp[i] <- as.numeric(beta_temp[2])
    
    # 使用eta_func选择此时对应的eta
    temp_data$eta[i] <- eta_func(
      value = temp_data$V_value[i],
      temp = temp_data$V_temp[i],
      reward = temp_data$Reward[i],
      occurrence = temp_data$Time_Line[i],
      eta = eta,
      epsilon = epsilon
    )
    
    # 从第二次才是真的开始学习了
    if (i >= 2) {
      # 计算此次update的值
      temp_data$V_update[i] <- temp_data$V_value[i] + 
        temp_data$eta[i] * (temp_data$V_temp[i] - temp_data$V_value[i])
    }
  }
  
############################## [delete first row] ############################## 
  # 删除第一行赋予的初始值
  res_data <- temp_data[-1, ]
  # 对于V取两位小数
  res_data$V_value <- round(res_data$V_value, 2)
  res_data$V_update <- round(res_data$V_update, 2)
  # 返回结果
  return(res_data)
}