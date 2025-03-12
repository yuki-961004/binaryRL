#' arrange_data
#'
#' @param data A data frame resulting from the 'step1' process of the `unique_choice` function.
#' 
#' @param time_line A vector specifying the name of the column that the sequence of the experiment. 
#' This argument defines how the experiment is structured, such as whether it is organized by "Block" with breaks in between, and multiple trials within each block. 
#' Provide the sequence as a character vector, 
#' e.g., `time_line = c("Block", "Trial")`
#'
#' @returns data frame:
#'   \itemize{
#'     \item{\code{data}: step1 arranged by 'time_line'.}
#'   }
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
arrange_data <- function(data, time_line = c("Block", "Trial")){
  # 基于time_line这个向量, 录入排序向量
  order_vector <- lapply(time_line, function(col) data[[col]])
  
  # 基于排序向量对输入数据集进行排序
  data <- data[do.call(order, order_vector), ]
  
  return(data)
}
