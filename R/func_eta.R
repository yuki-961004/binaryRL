#' func_eta
#'
#' @param value The value you assign to this stimulus at this moment
#' @param utility Subjects' subjective value of the reward
#' @param reward The reward given to you by the experimental procedure after choosing this stimulus
#' @param occurrence The number of times this stimulus is encountered
#' @param var1 extra variable 1
#' @param var2 extra variable 2
#' @param eta In the RSTD model, the learning rate is different for positive and negative conditions.
#' @param epsilon In the WXT model, the discount is divided into different intervals.
#'
#' @return learning rate eta
#' @export
#'
func_eta <- function(
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
  eta,
  epsilon
  ################################# [function start] #############################
){
  #################################### [ TD ] ####################################
  if (length(eta) == 1) {
    eta <- as.numeric(eta)
  }
  ################################### [ RSTD ] ###################################
  else if (length(eta) > 1 & utility < value) {
    eta <- eta[1]
  } 
  else if (length(eta) > 1 & utility >= value) {
    eta <- eta[2]
  }
  ################################## [ ERROR ] ###################################
  else {
    eta <- "ERROR" # 检查错误
  }
  return(eta)
}