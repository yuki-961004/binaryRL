#' Set initial values for all options
#'
#' @param data [data.frame] A data frame resulting from the 'step3' process of the `add_NA` function. 
#' 
#' @param options [vector] all alternative options from 'step1' `unique_choice`
#' 
#' @param initial_value [numeric] subject's initial expected value for each 
#'  stimulus's reward. If this value is not set (`initial_value = NA`), 
#'  the subject will use the reward received after the first trial as the 
#'  initial value for that stimulus. In other words, the learning rate for the 
#'  first trial is 100%. default: `initial_value = NA` 
#'  e.g., `initial_value = 0`
#'
#' @returns data frame:
#'   \itemize{
#'     \item{\code{data}: step3 + row[0] with initial value.}
#'   }
#' @export
#'
set_initial_value <- function(data, options, initial_value = NA){
  # 设置初始值, 如果没有设置初始值, 则赋予一个0
  # 这个初始值并不是很关键, 只要有数字就行
  if (is.na(initial_value)) {
    # update_v 相关
    data$V_value[1] <- 0
    data$R_utility[1] <- 0
    data$V_update[1] <- 0
    # action_c 相关
    data$L_value[1] <- 0
    data$R_value[1] <- 0
    
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
