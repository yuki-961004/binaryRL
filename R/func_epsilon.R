#' Function: Epsilon Greedy
#'
#' @note When customizing these functions, please ensure that you do not modify 
#' the arguments. Instead, only modify the `if-else` statements or the internal 
#' logic to adapt the function to your needs.
#' 
#' @param var1 [character] column name of extra variable 1. If your model uses 
#'  more than just reward and expected value, and you need other information, 
#'  such as whether the choice frame is Gain or Loss, then you can input the 
#'  'Frame' column as var1 into the model.
#'  e.g., `var1 = "Extra_Var1"`
#' 
#' @param var2 [character] column name of extra variable 2. If one additional 
#'  variable, var1, does not meet your needs, you can add another additional 
#'  variable, var2, into your model.
#'  e.g., `var2 = "Extra_Var2"`
#'  
#' @param i The current row number. The `threshold` for random selection, which 
#'  is used to explore the value of different options, will be determined based 
#'  on this row number. This is because I believe that in the early stages of 
#'  an experiment, participants will choose options completely at random to 
#'  explore the reward value associated with each option.
#' 
#' @param threshold [integer] the number of initial trials during which the 
#'  subject makes random choices rather than choosing based on the values of 
#'  the options. This occurs because the subject has not yet learned the values 
#'  of the options. For example, `threshold = 20` means the subject will make 
#'  completely random choices for the first 20 trials. 
#'  default: `threshold = 1`
#' 
#' @param epsilon [vector] Parameters used in the Exploration Function
#' `expl_func` determining whether the subject makes decisions based on the 
#'  relative values of the left and right options, or chooses completely 
#'  randomly. For example, when epsilon = 0.1, it means the subject has a 10% 
#'  chance of making a completely random choice and a 90% chance of choosing 
#'  based on the values of the options.
#'  e.g., `epsilon = c(0.1)`
#' 
#' @param lambda [vector] Extra parameters that may be used in functions. 
#'  e.g., `lambda = c(0.4, 0.7, 20, 60)`
#'  
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
  threshold = 1,
  # 参数
  epsilon = NA,
  # 额外参数
  lambda
){
  # epsilon-first: 在一定试次前随机尝试
  if (i <= threshold) {
    try <- 1
  } else if (i > threshold & is.na(epsilon)) {
    try <- 0
  # epsilon-greedy: 在整个实验中随机抽风
  } else if (i > threshold & !(is.na(epsilon))){
    try <- sample(
      c(1, 0),
      prob = c(epsilon, 1 - epsilon),
      size = 1
    )
  # epsilon-decreasing: 随机抽风的概率随着试次的上升而下降
  } else if (!(is.na(lambda)) & is.na(epsilon)) {
    try <- sample(
      c(1, 0),
      prob = c(
        1 / (1 + lambda * i), 
        lambda * i / (1 + lambda * i)
      ),
      size = 1
    )
  }
  else {
    try <- "ERROR"
  }
  
  return(try)
}
