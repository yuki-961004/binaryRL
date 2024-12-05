#' func_gamma
#'
#' @param value The value you assign to this stimulus at this moment
#' @param utility Subjects' subjective value of the reward
#' @param reward The reward given to you by the experimental procedure after choosing this stimulus
#' @param occurrence The number of times this stimulus is encountered
#' @param var1 extra variable 1
#' @param var2 extra variable 2
#' @param gamma In the utility model, it is assumed that all rewards will be discounted
#' @param lambda the eta or gamma could be divided into different intervals.
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
  lambda = NA
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