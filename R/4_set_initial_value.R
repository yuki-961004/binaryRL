set_initial_value <- function(data, options, initial_value = NA){
  # 设置初始值, 如果没有设置初始值, 则赋予一个0
  # 这个初始值并不是很关键, 只要有数字就行
  if (is.na(initial_value)) {

    # action_c 相关
    data$L_freq[1] <- 0
    data$R_freq[1] <- 0
    
    data$L_pick[1] <- 0
    data$R_pick[1] <- 0
    
    data$L_value[1] <- 0
    data$R_value[1] <- 0
    
    # update_v 相关
    data$V_value[1] <- 0
    data$R_utility[1] <- 0
    data$V_update[1] <- 0
    
    # 给每个选项赋予初始值
    for (name in options) {
      data[[name]][1] <- 0
    }
  } else {
    # 赋予设定的初始值
    # update_v 相关
    data$V_value[1] <- initial_value
    data$R_utility[1] <- initial_value
    data$V_update[1] <- initial_value
    # action_c 相关
    data$L_value[1] <- initial_value
    data$R_value[1] <- initial_value
    
    # 给每个选项赋予初始值
    for (name in options) {
      data[[name]][1] <- initial_value
    }
  }
  
  return(data)
}
