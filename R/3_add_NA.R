add_NA <- function(data){
  
  # 生成一个与输入数据集相同的单行数据集. 用于存放初始值
  empty_row <- as.data.frame(matrix(ncol = ncol(data), nrow = 1))
  colnames(empty_row) <- colnames(data)
  # 在第一行插入一个空行
  data <- rbind(empty_row, data)
  
  data$Rob_Choose <- NA
  
  data$ACC <- NA
  
  data$L_value <- NA
  data$R_value <- NA

  data$L_bias <- NA
  data$R_bias <- NA
  
  data$L_prob <- NA
  data$R_prob <- NA
  
  data$L_dir <- NA
  data$R_dir <- NA
  
  data$L_logl <- NA
  data$R_logl <- NA
  
  # 添加空列 action_c 相关
  data$L_freq <- NA
  data$R_freq <- NA
  
  data$L_pick <- NA
  data$R_pick <- NA
  
  # 添加空列 update_v 相关
  data$Occurrence <- NA
  
  data$Reward <- NA
  data$gamma <- NA
  data$R_utility <- NA
  
  data$V_value <- NA
  data$eta <- NA
  data$V_update <- NA
  
  data$Try <- NA
  
  return(data)
}
