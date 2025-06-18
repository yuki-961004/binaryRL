#' Function: Exploration Strategy
#'
#' @description
#' The exploration strategy parameters are `threshold`, `epsilon`, and `pai`.
#' 
#'   \itemize{
#'     \item \strong{Epsilon-first strategy:} Used when only `threshold` is set. 
#'       Subjects choose randomly for trials less than `threshold` and by value 
#'       for trials greater than `threshold`.
#'     \item \strong{Epsilon-greedy strategy:} Used if `threshold` is default 
#'       (1) and `epsilon` is set. Subjects explore with probability `epsilon` 
#'       throughout the experiment.
#'     \item \strong{Epsilon-decreasing strategy:} Used if `threshold` is 
#'       default (1), and `pai` is set. In this strategy, the probability of 
#'       random choice (exploration) decreases as trials increase. The 
#'       parameter `pai` controls the rate at which this probability declines 
#'       with each trial.
#'   }
#' 
#' @note 
#' When customizing these functions, please ensure that you do not modify the 
#'  arguments. Instead, only modify the `if-else` statements or the internal 
#'  logic to adapt the function to your needs.
#' 
#' @param var1 [character] 
#' Column name of extra variable 1. If your model uses 
#'  more than just reward and expected value, and you need other information, 
#'  such as whether the choice frame is Gain or Loss, then you can input the 
#'  'Frame' column as var1 into the model.
#'  e.g., `var1 = "Extra_Var1"`
#' 
#' @param var2 [character] 
#' Column name of extra variable 2. If one additional 
#'  variable, var1, does not meet your needs, you can add another additional 
#'  variable, var2, into your model.
#'  e.g., `var2 = "Extra_Var2"`
#'  
#' @param i 
#' The current row number. The `threshold` for random selection, which 
#'  is used to explore the value of different options, will be determined based 
#'  on this row number. This is because I believe that in the early stages of 
#'  an experiment, participants will choose options completely at random to 
#'  explore the reward value associated with each option.
#' 
#' @param threshold [integer]
#' Controls the initial exploration phase in the \strong{epsilon-first} strategy.
#'  This is the number of early trials where the subject makes purely random
#'  choices, as they haven't yet learned the options' values. For example,
#'  `threshold = 20` means random choices for the first 20 trials.
#'  For \strong{epsilon-greedy} or \strong{epsilon-decreasing} strategies,
#'  `threshold` should be kept at its default value.
#'  Default: `threshold = 1`
#'
#' @param epsilon [vector]
#' A parameter used in the \strong{epsilon-greedy} exploration strategy. It defines
#'  the probability of making a completely random choice, as opposed to choosing
#'  based on the relative values of the left and right options. For example,
#'  if `epsilon = 0.1`, the subject has a 10% chance of random choice and a
#'  90% chance of value-based choice. This parameter is only relevant when
#'  `threshold` is at its default value (1) and `pai` is not set.
#'  e.g., `epsilon = 0.1`
#' 
#' @param lambda [vector] 
#' A numeric value that controls the decay rate of exploration probability
#'  in the \strong{epsilon-decreasing} strategy. A higher `lambda` value
#'  means the probability of random choice will decrease more rapidly
#'  as the number of trials increases.
#' 
#' @param alpha [vector]
#' Extra parameters that may be used in functions. 
#'
#' @param beta [vector]
#' Extra parameters that may be used in functions. 
#'  
#' @return
#' A numeric value, either 0 or 1. 0 indicates no exploration (choice based 
#'  on value), and 1 indicates exploration (random choice) for that trial.
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
  lambda = NA,
  # 额外参数
  alpha,
  beta
){
  # epsilon-first: 在一定试次前随机尝试
  if (i <= threshold) {
    try <- 1
  } else if (i > threshold & is.na(epsilon) & is.na(lambda)) {
    try <- 0
  # epsilon-greedy: 在整个实验中随机抽风
  } else if (i > threshold & !(is.na(epsilon)) & is.na(lambda)){
    try <- sample(
      c(1, 0),
      prob = c(epsilon, 1 - epsilon),
      size = 1
    )
  # epsilon-decreasing: 随机抽风的概率随着试次的上升而下降
  } else if (i > threshold & is.na(epsilon) & !(is.na(lambda))) {
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
