#' add_NA
#'
#' @param data A dataframe resulting from the 'step2' process of the `arrange_data` function. 
#'
#' @returns step3
#' @export
#'
#' @examples
#' data <- TAFC[TAFC$Subject == 1, ]
#' 
#' step1 <- unique_choice(
#'   data = data,
#'   L_choice = "L_choice", 
#'   R_choice = "R_choice"
#'  )
#'  
#' step2 <- arrange_data(
#'   data = step1[[1]],
#'   time_line = c("Block", "Trial")
#' )
#' 
#' step3 <- add_NA(
#'   data = step2
#' )
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
