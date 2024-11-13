#' func_beta
#'
#' @param value The value you assign to this stimulus at this moment
#' @param temp Intermediate transition value. May not be used
#' @param reward The reward given to you by the experimental procedure after choosing this stimulus
#' @param ev expected value
#' @param frame decision frame
#' @param occurrence The number of times this stimulus is encountered
#' @param beta In the utility model, it is assumed that all rewards will be discounted
#' @param epsilon In the Prospect theory, the discount is divided into different intervals.
#'
#' @return Discount rate and temp value
#' @export
#'
func_beta <- function(
    # 此时心中对该刺激的的value
  value, 
  # 中间过渡的value. 可能用不上
  temp,
  # 选择后看到的reward
  reward, 
  # 目前这个框架下的奖励期望
  ev,
  # 框架low, high
  frame,
  # 第几次看到这个刺激
  occurrence, 
  # 使用的参数
  beta = 1,
  epsilon = NA
  ################################# [function start] #############################
){
  ################################# [ Utility ] ##################################
  # 如果beta只有一种, 则直接用这个beta计算temp
  if (length(beta) == 1) {
    beta <- as.numeric(beta)
    temp <- beta * reward
  }
  ################################# [ Utility ] ##################################
  else {
    temp <- "ERROR" # 检查错误
  }
  return(list(beta, temp))
}
