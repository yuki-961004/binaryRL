#' func_prob
#' 
#' @param LR Are you calculating the probability for the left option or the right option?
#' @param try If the choice was random, the value is 1; if the choice was based on value, the value is 0.
#' @param L_value The value of the left option
#' @param R_value The value of the right option
#' 
#' @param var1 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var1 = "Extra_Var1"`
#' 
#' @param var2 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var2 = "Extra_Var2"`
#' 
#' @param tau A parameter used in the `prob_func` (Soft-Max Function), representing the sensitivity of the subject to the value difference when making decisions. 
#' It determines the probability of selecting the left option versus the right option based on their values. A larger value of tau indicates greater sensitivity 
#' to the value difference between the options. In other words, even a small difference in value will make the subject more likely to choose the higher-value option. 
#' Provide the value as a vector 
#' e.g., `tau = c(0.5)`
#' 
#' @param lambda An additional parameter that may be used in these functions. 
#' Provide the value as a vector 
#' e.g., `lambda = c(0.4, 0.7, 20, 60)`
#' 
#' @return The probability of choosing this option
#' @export
#'
func_tau <- function(
  # 此时计算的是选左概率还是选右概率
  LR,
  # 是否随机尝试
  try,
  # 左边选项的价值
  L_value,
  # 右边选项的价值
  R_value,
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,
  # softmax固有参数, 表示对左右差异的敏感性
  tau = 1,
  # 额外参数
  lambda
){
  if (!(LR %in% c("L", "R"))) {
    stop("LR = 'L' or 'R'")
  }
  ################################# [ softmax ] ##################################
  else if (try == 0 & LR == "L") {
    prob <- 1 / (1 + exp(-(L_value - R_value) * tau))
  }
  else if (try == 0 & LR == "R") {
    prob <- 1 / (1 + exp(-(R_value - L_value) * tau))
  }
  ################################# [ softmax ] ##################################
  else if (try == 1) {
    prob <- 0.5
  } 
  else {
    prob <- "ERROR"
  }
  
  return(prob)
}
