#' func_gamma
#'
#' @note When customizing these functions, please ensure that you do not modify the arguments. 
#' Instead, only modify the `if-else` statements or the internal logic to adapt the function to your needs.
#'
#' @param value The expected value of the stimulus in the subject's mind at this point in time.
#' @param utility The subjective value that the subject assigns to the objective reward.
#' @param reward The objective reward received by the subject after selecting a stimulus.
#' @param occurrence The number of times the same stimulus has appeared.
#' 
#' @param var1 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var1 = "Extra_Var1"`
#' 
#' @param var2 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var2 = "Extra_Var2"`
#' 
#' @param gamma A parameter used in the `util_func` (Utility Function), often 
#' referred to as the discount rate. For example, in the utility equation 
#' `utility = gamma * reward`, if gamma < 1, it indicates that people discount 
#' the objective reward. 
#' Provide the value as a vector 
#' e.g., `gamma = c(0.7)`
#' 
#' @param lambda An additional parameter that may be used in these functions. 
#' Provide the value as a vector 
#' e.g., `lambda = c(0.4, 0.7, 20, 60)`
#' 
#' @return Discount rate and utility
#' @export
#'
func_gamma <- function(
  # 此时心中对该刺激的的value
  value, 
  # 心中的主观价值
  utility,
  # 选择后看到的reward
  reward, 
  # 第几次看到这个刺激
  occurrence, 
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,
  # 使用的参数
  gamma = 1,
  # 额外参数
  lambda
  ################################# [function start] #############################
){
  ################################# [ Utility ] ##################################
  # 如果gamma只有一种, 则直接用这个gamma计算temp
  if (length(gamma) == 1) {
    gamma <- as.numeric(gamma)
    utility <- gamma * reward
  }
  ################################# [ Utility ] ##################################
  else {
    utility <- "ERROR" # 检查错误
  }
  return(list(gamma, utility))
}
