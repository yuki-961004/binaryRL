#' unique_choice
#'
#' @param data A data frame containing the raw data. 
#' This data should include the following mandatory columns: 
#' - "sub", "time_line", "L_choice", "R_choice", "choose", "reward". 
#' The following arguments allow you to customize the column names used for processing
#' @param L_choice A string specifying the name of the column that represents the left choice. 
#' Provide the name of the column as a character string 
#' e.g., `L_choice = "Left_Choice"`
#' 
#' @param R_choice A string specifying the name of the column that represents the right choice. 
#' Provide the name of the column as a character string 
#' e.g., `R_choice = "Right_Choice"`
#'
#' @returns step1
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
unique_choice <- function(data, L_choice = "L_choice", R_choice = "R_choice"){
  # 用于获取左右选项中的备选项.
  # 以选项的名称创建新列, 用于储存每一次决策后的价值更新
  
  # 获取 L_choice 和 R_choice 的唯一值
  unique_L <- unique(data[[L_choice]])
  unique_R <- unique(data[[R_choice]])
  
  # 检查L_choice 和R_choice是否包含了一样的选项
  if (!all(unique_L %in% unique_R) || !all(unique_R %in% unique_L)) {
    stop("Error: L_choice and R_choice have different options!")
  } 
  else {
    # 将所有备选项传输到函数的上一层
    options <- unique_L
  }
  
  # 把所有备选项以列名创建, 方便存放价值更新
  for (name in options) {
    data[[name]] <- NA
  }
  
  res <- list(data, options)
  names(res)[1] <- "data"
  names(res)[2] <- "options"
  
  return(res)
}
