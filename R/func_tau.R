#' func_prob
#'
#' @param try random choose one or based on the values
#' @param L_value The value of the left option
#' @param R_value The value of the right option
#' @param var1 extra variable 1
#' @param var2 extra variable 2
#' @param tau The τ parameter in the soft-max function, with a default value of 1
#' @param LR Are you calculating the probability for the left option or the right option?
#' 
#' @return The probability of choosing this option
#' @export
#'
func_tau <- function(
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
  # 此时计算的是选左概率还是选右概率
  LR
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