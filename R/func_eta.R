#' func_eta
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
#' @param eta A parameter used in the `rate_func` (Learning Rate Function), 
#' representing the rate at which the subject updates the difference between the reward and the expected value in the subject's mind. 
#' In the TD model, there is a single learning rate throughout the experiment. 
#' In the RSTD model, two different learning rates are used when the reward is higher or lower than the expected value.
#' Provide the value as a vector 
#' e.g., `eta = c(0.3, 0.7)`
#' 
#' @param lambda An additional parameter that may be used in these functions. 
#' Provide the value as a vector 
#' e.g., `lambda = c(0.4, 0.7, 20, 60)`
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
  # 额外参数
  lambda
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