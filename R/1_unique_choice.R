#' Figure out how many options exist
#'
#' @param data [data.frame] 
#' This data should include the following mandatory columns: 
#'  \itemize{
#'    \item "sub"
#'    \item "time_line" (e.g., "Block", "Trial")
#'    \item "L_choice"
#'    \item "R_choice"
#'    \item "L_reward"
#'    \item "R_reward"
#'    \item "sub_choose"
#'  }
#' 
#' @param L_choice [character] 
#' Column name of left choice. 
#'  e.g., `L_choice = "Left_Choice"`
#' 
#' @param R_choice [character] 
#' Column name of right choice. 
#'  e.g., `R_choice = "Right_Choice"`
#'
#' @returns list:
#'  \itemize{
#'    \item{\code{data}: raw data frame + null cols [options].}
#'    \item{\code{options}: a vector containing all options.}
#'  }
#'
#' @noRd
#' 
unique_choice <- function(data, L_choice = "L_choice", R_choice = "R_choice"){
  # 用于获取左右选项中的备选项.
  # 以选项的名称创建新列, 用于储存每一次决策后的价值更新
  
  # 获取 L_choice 和 R_choice 的唯一值
  unique_L <- unique(data[[L_choice]])
  unique_R <- unique(data[[R_choice]])
  
  # 检查L_choice 和R_choice是否包含了一样的选项
  if (!all(unique_L %in% unique_R) || !all(unique_R %in% unique_L)) {
    warning("Warning: L_choice and R_choice have different options!")
  }
  
  # 记录所有选项
  options <- sort(unique(c(unique_L, unique_R)))
  
  # 把所有备选项以列名创建, 方便存放价值更新
  for (name in options) {
    data[[name]] <- NA
  }
  
  res <- list(data, options)
  names(res)[1] <- "data"
  names(res)[2] <- "options"
  
  return(res)
}
