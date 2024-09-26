#' func_prob
#'
#' @param L_value The value of the left option
#' @param R_value The value of the right option
#' @param tau The τ parameter in the soft-max function, with a default value of 1
#' @param params Other parameters that you think might influence the softmax function
#' @param LR Are you calculating the probability for the left option or the right option?
#'
#' @return example soft-max function
#' @export
#'
func_prob <- function(
  # 左边选项的价值
  L_value,
  # 右边选项的价值
  R_value,
  # softmax固有参数, 默认为1
  tau = 1,
  # 其他你想使用的参数, 默认没有
  params = NA,
  # 此时计算的是选左概率还是选右概率
  LR
){
  if (!(LR %in% c("L", "R"))) {
    stop("LR = 'L' or 'R'")
  }
  ################################# [ softmax ] ##################################
  else if (LR == "L") {
    prob <- 1 / (1 + exp(-(L_value - R_value) * tau))
  }
  else if (LR == "R") {
    prob <- 1 / (1 + exp(-(R_value - L_value) * tau))
  }
  ################################# [ softmax ] ##################################
  else {
    prob <- "ERROR"
  }
  return(prob)
}
