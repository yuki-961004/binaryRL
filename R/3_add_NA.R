#' Create NULL columns and the line 0
#'
#' @param data [data.frame] 
#' A data frame resulting from the 'step2' process of the `arrange_data` function. 
#'
#' @returns data frame:
#'  \itemize{
#'    \item{
#'      \code{data}: step2 + row[0] for initial value + null cols 
#'      [Reward, gamma, R_utility, ...].}
#'  }
#'   
#' @noRd
#' 
add_NA <- function(data){
  
  # 生成一个与输入数据集相同的单行数据集. 用于存放初始值
  empty_row <- as.data.frame(matrix(ncol = ncol(data), nrow = 1))
  colnames(empty_row) <- colnames(data)
  # 在第一行插入一个空行
  data <- rbind(empty_row, data)
  
  # 添加空列 action_c 相关
  data$L_freq <- NA
  data$R_freq <- NA
  
  data$L_pick <- NA
  data$R_pick <- NA
  
  data$L_value <- NA
  data$R_value <- NA

  data$L_bias <- NA
  data$R_bias <- NA
  
  data$L_prob <- NA
  data$R_prob <- NA
  
  data$Try <- NA
  
  data$Rob_Choose <- NA
  
  # 添加空列 update_v 相关
  data$Occurrence <- NA
  
  data$Reward <- NA
  data$gamma <- NA
  data$R_utility <- NA
  
  data$V_value <- NA
  data$eta <- NA
  data$V_update <- NA
  
  return(data)
}
