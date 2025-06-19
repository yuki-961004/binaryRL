#' Function: Exploration Strategy
#'
#' @description
#' The exploration strategy parameters are 
#'  \code{threshold}, \code{epsilon}, and \code{lambda}.
#' 
#'   \itemize{
#'     \item \strong{Epsilon-first strategy:} Used when only \code{threshold} is set. 
#'       Subjects choose randomly for trials less than \code{threshold} and by value 
#'       for trials greater than `\code{threshold}.
#'     \item \strong{Epsilon-greedy strategy:} Used if \code{threshold} is default 
#'       (1) and \code{epsilon} is set. Subjects explore with probability 
#'       \code{epsilon} throughout the experiment.
#'     \item \strong{Epsilon-decreasing strategy:} Used if \code{threshold} is 
#'       default (1), and \code{lambda} is set. In this strategy, the probability of 
#'       random choice (exploration) decreases as trials increase. The 
#'       parameter \code{lambda} controls the rate at which this probability 
#'       declines with each trial.
#'   }
#' 
#' @note 
#' When customizing these functions, please ensure that you do not modify the 
#'  arguments. Instead, only modify the `if-else` statements or the internal 
#'  logic to adapt the function to your needs.
#' 
#' @param var1 [character] 
#' Column name of extra variable 1. If your model uses more than just reward 
#'  and expected value, and you need other information, such as whether the 
#'  choice frame is Gain or Loss, then you can input the 'Frame' column as 
#'  var1 into the model.
#'  
#'  \code{default: var1 = "Extra_Var1"}
#' 
#' @param var2 [character] 
#' Column name of extra variable 2. If one additional variable, var1, does not 
#'  meet your needs, you can add another additional variable, var2, into your 
#'  model.
#'  
#'  \code{default: var2 = "Extra_Var2"}
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
#'  \code{threshold = 20} means random choices for the first 20 trials.
#'  For \strong{epsilon-greedy} or \strong{epsilon-decreasing} strategies,
#'  `threshold` should be kept at its default value.
#'  
#'  \deqn{P(x) = \begin{cases}
#'    \text{trial} \le \text{threshold}, & x=1 \text{ (random choosing)} \\
#'    \text{trial} > \text{threshold}, & x=0 \text{ (value-based choosing)}
#'  \end{cases}}
#'  
#'  \code{default: threshold = 1}
#'  
#'  \code{epsilon-first: threshold = 20, epsilon = NA, lambda = NA}
#'
#' @param epsilon [numeric]
#' A parameter used in the \strong{epsilon-greedy} exploration strategy. It defines
#'  the probability of making a completely random choice, as opposed to choosing
#'  based on the relative values of the left and right options. For example,
#'  if `epsilon = 0.1`, the subject has a 10% chance of random choice and a
#'  90% chance of value-based choice. This parameter is only relevant when
#'  `threshold` is at its default value (1) and `lambda` is not set.
#'  
#'  \deqn{P(x) = \begin{cases}
#'    \epsilon, & x=1 \text{ (random choosing)} \\
#'    1-\epsilon, & x=0 \text{ (value-based choosing)}
#'  \end{cases}}
#' 
#'  \code{epsilon-greedy: threshold = 1, epsilon = 0.1, lambda = NA}
#' 
#' @param lambda [vector] 
#' A numeric value that controls the decay rate of exploration probability
#'  in the \strong{epsilon-decreasing} strategy. A higher `lambda` value
#'  means the probability of random choice will decrease more rapidly
#'  as the number of trials increases.
#'  
#'  \deqn{P(x) = \begin{cases}
#'    \frac{1}{1+\lambda \cdot trial}, & x=1 \text{ (random choosing)} \\
#'    \frac{\lambda \cdot trial}{1+\lambda \cdot trial}, & x=0 \text{ (value-based choosing)}
#'  \end{cases}}
#'  
#'  \code{epsilon-decreasing threshold = 1, epsilon = NA, lambda = 0.5}
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
#' @examples
#' \dontrun{
#' func_epsilon <- function(
#'   # Current trial number
#'   i,
#'   # Extra variables
#'   var1 = NA,
#'   var2 = NA,
#'   # Free Parameters
#'   threshold = 1,
#'   epsilon = NA,
#'   lambda = NA,
#'   # Extra parameters
#'   alpha,
#'   beta
#' ){
#'   # Epsilon-First: random choosing before a certain trial number
#'   if (i <= threshold) {
#'     try <- 1
#'   } else if (i > threshold & is.na(epsilon) & is.na(lambda)) {
#'     try <- 0
#'   # Epsilon-Greedy: random choosing throughout the experiment with probability epsilon
#'   } else if (i > threshold & !(is.na(epsilon)) & is.na(lambda)){
#'     try <- sample(
#'       c(1, 0),
#'       prob = c(epsilon, 1 - epsilon),
#'       size = 1
#'     )
#'   # Epsilon-Decreasing: probability of random choosing decreases as trials increase
#'   } else if (i > threshold & is.na(epsilon) & !(is.na(lambda))) {
#'     try <- sample(
#'       c(1, 0),
#'       prob = c(
#'         1 / (1 + lambda * i),
#'         lambda * i / (1 + lambda * i)
#'       ),
#'       size = 1
#'     )
#'   }
#'   else {
#'     try <- "ERROR"
#'   }
#'
#'   return(try)
#' }
#' }
#' 
func_epsilon <- function(
  # 这是第几个试次
  i,
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,
  # 自由参数
  threshold = 1,
  epsilon = NA,
  lambda = NA,
  # 额外参数
  alpha,
  beta
){
############################ [ Epsilon-First ] #################################
  if (i <= threshold) {
    try <- 1
  } else if (i > threshold & is.na(epsilon) & is.na(lambda)) {
    try <- 0
########################### [ Epsilon-Greedy ] #################################
  } else if (i > threshold & !(is.na(epsilon)) & is.na(lambda)){
    try <- sample(
      c(1, 0),
      prob = c(epsilon, 1 - epsilon),
      size = 1
    )
######################### [ Epsilon-Decreasing ] ###############################
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
################################## [ ERROR ] ###################################
  else {
    try <- "ERROR"
  }
  
  return(try)
}
