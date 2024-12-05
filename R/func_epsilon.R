#' func_epsilon
#'
#' @param i row number
#' @param var1 extra variable 1
#' @param var2 extra variable 2
#' @param threshold Before how many trials will the subject try randomly
#' @param epsilon How much the subjects like to try
#'
#' @return explore or not
#' @export 
#'
func_epsilon <- function(
  # 这是第几个试次
  i,
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,
  # 多少次后就不是一定尝试了
  threshold = 5,
  # 参数
  epsilon = NA
){
  # 如果没有输入epsilon, 那么只在临界值前随机选
  if (i <= threshold) {
    try <- 1
    # 除此之外说明输入了epsilon
  } else if (i > threshold & !(is.na(epsilon))){
    # 如果选项出现的次数超过了临界值, 那么就按照概率进行尝试
    try <- sample(
      c(1, 0),
      prob = c(epsilon, 1 - epsilon),
      size = 1
    )
  } else if (i > threshold & is.na(epsilon)) {
    try <- 0
  } else {
    try <- "ERROR"
  }
  
  return(try)
}