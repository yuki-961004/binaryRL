#' func_eta
#'
#' @param value The value you assign to this stimulus at this moment
#' @param temp Intermediate transition value. May not be used
#' @param reward The reward given to you by the experimental procedure after choosing this stimulus
#' @param occurrence The number of times this stimulus is encountered
#' @param eta In the RSTD model, the learning rate is different for positive and negative conditions.
#' @param epsilon In the WXT model, the discount is divided into different intervals.
#'
#' @return learning rate eta
#' @export
#'
func_eta <- function(
    # 此时心中对该刺激的的value
  value, 
  # 中间过渡的value. 可能用不上
  temp,
  # 选择后看到的reward
  reward, 
  # 第几次看到这个刺激
  occurrence, 
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
  else if (length(eta) > 1 & temp < value) {
    eta <- eta[1]
  } 
  else if (length(eta) > 1 & temp >= value) {
    eta <- eta[2]
  }
  ################################### [ RSTD ] ###################################
  else {
    eta <- "ERROR" # 检查错误
  }
  return(eta)
}
