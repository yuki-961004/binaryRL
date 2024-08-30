#' ex_func_beta
#'
#' @param value The value you assign to this stimulus at this moment
#' @param temp Intermediate transition value. May not be used
#' @param reward The reward given to you by the experimental procedure after choosing this stimulus
#' @param occurrence The number of times this stimulus is encountered
#' @param beta In the utility model, it is assumed that all rewards will be discounted
#' @param epsilon In the WXT model, the discount is divided into different intervals.
#'
#' @return Discount rate and temp value
#' @export
#'
ex_func_beta <- function(
    # 此时心中对该刺激的的value
  value, 
  # 中间过渡的value. 可能用不上
  temp,
  # 选择后看到的reward
  reward, 
  # 第几次看到这个刺激
  occurrence, 
  # 使用的参数
  beta = 1,
  epsilon = NA
  ################################# [function start] #############################
){
  # 如果epsilon是NA, 则说明只使用了beta, 也可以beta也没用是默认值1
  if (any(is.na(epsilon))) {
    beta <- beta
    temp <- beta * reward
  }
  ################################# [ Utility ] ##################################

  ################################# [ Utility ] ##################################
  else {
    temp <- "ERROR" # 检查错误
  }
  return(list(beta, temp))
}
