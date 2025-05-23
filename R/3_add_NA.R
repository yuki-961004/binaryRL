#' Create NULL columns and the line 0
#'
#' @param data [data.frame] A data frame resulting from the 'step2' process of the `arrange_data` function. 
#'
#' @returns data frame:
#'   \itemize{
#'     \item{\code{data}: step2 + row[0] for initial value + null cols [Reward, gamma, R_utility, ...].}
#'   }
#'   
#' @noRd
#' 
add_NA <- function(data){
  
  # 生成一个与输入数据集相同的单行数据集. 用于存放初始值
  empty_row <- as.data.frame(matrix(ncol = ncol(data), nrow = 1))
  colnames(empty_row) <- colnames(data)
  # 在第一行插入一个空行
  data <- rbind(empty_row, data)
  
  # 新建一列Time_Line
  data$Time_Line <- NA
  
  # 添加空列 update_v 相关
  data$Reward <- NA
  data$gamma <- NA
  data$R_utility <- NA
  
  data$V_value <- NA
  data$eta <- NA
  data$V_update <- NA
  
  # 添加空列 action_c 相关
  data$L_value <- NA
  data$R_value <- NA
  
  data$Try <- NA
  
  data$L_prob <- NA
  data$R_prob <- NA
  data$Rob_Choose <- NA
  
  return(data)
}
