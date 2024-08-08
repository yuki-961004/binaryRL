#' rl_update_v
#'
#' @param data Data containing only one type of stimulus
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param initial_value The initial value you assign to a stimulus, defaulting to 0
#' @param params The parameter values corresponding to eta_func.
#' @param eta_func The function for the learning rate η, which you can customize
#'
#' @return update value for 1 choice
#' @export

rl_update_v <- function(
    # 输入data frame
  data,
  # 价值更新的时间线, 基于的列
  time_line = c("Block", "Trial"),
  # 初始值
  initial_value = 0,
  # parameters
  params = c(0.3, 0.7),
  # 价值函数选用示例函数
  eta_func = ex_func_eta
){
  ################################# Arrange ######################################
  # 基于time_line这个向量, 录入排序向量
  order_vector <- lapply(time_line, function(col) data[[col]])
  
  # 基于排序向量对输入数据集进行排序
  temp_data <- data[do.call(order, order_vector), ]
  
  ############################### Add Null row ###################################
  # 生成一个与输入数据集相同的单行数据集. 用于存放初始值
  empty_row <- as.data.frame(matrix(ncol = ncol(data), nrow = 1))
  colnames(empty_row) <- colnames(data)
  
  # 在第一行插入一个空行
  temp_data <- rbind(empty_row, temp_data)
  temp_data$Time_Line <- seq(from = 0, to = nrow(data))
  
  ############################### Add Null row ###################################
  
  temp_data$V_value <- NA
  temp_data$eta <- NA
  temp_data$V_update <- NA
  
  
  # 设置初始值
  temp_data$V_value[1] <- initial_value + 1e-10
  temp_data$V_update[1] <- initial_value + 1e-10
  
  
  for (i in 2:nrow(temp_data)) {
    temp_data$V_value[i] <- temp_data$V_update[i - 1]
    temp_data$eta[i] <- eta_func(
      value = temp_data$V_value[i],
      reward = temp_data$Reward[i],
      occurrence = temp_data$Time_Line[i],
      params = params
    )
    temp_data$V_update[i] <- temp_data$V_value[i] + temp_data$eta[i] * (temp_data$Reward[i] - temp_data$V_value[i])
  }
  
  res_data <- temp_data[-1, ]
  res_data$V_value <- round(res_data$V_value, 2)
  res_data$V_update <- round(res_data$V_update, 2)
  
  return(res_data)
}