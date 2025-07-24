#' Function: Loss Function
#'
#' @description
#' This loss function reflects the similarity between human choices
#'  and RL model predictions. If a human selects the left option and the
#'  RL model predicts a high probability for the left option, then 
#'  \eqn{logP_{L}} approaches 0, causing the first term to approach 0.
#'
#' Since the human chose the left option, \eqn{B_{R}} becomes 0, making the 
#'  second term naturally zero. Therefore, the more consistent the RL model's 
#'  prediction is with human choice, the closer this LL value is to 0. 
#'  Conversely, it approaches negative infinity.
#'  \deqn{
#'    LL = \sum B_{L} \times \log P_{L} + \sum B_{R} \times \log P_{R}
#'  } 
#'  
#' @note 
#' When customizing these functions, please ensure that you do not modify 
#'  the arguments. Instead, only modify the \code{if-else} statements or 
#'  the internal logic to adapt the function to your needs.
#' 
#' @param i 
#' The current row number.
#' 
#' @param L_freq 
#' The frequency of left option appearance
#' 
#' @param R_freq 
#' The frequency of right option appearance
#' 
#' @param L_pick 
#' The number of times left option was picked
#' 
#' @param R_pick 
#' The number of times left option was picked
#' 
#' @param L_value 
#' The value of the left option
#' 
#' @param R_value 
#' The value of the right option
#' 
#' @param L_dir 
#' Whether the participant chose the left option.
#' 
#' @param R_dir 
#' Whether the participant chose the right option.
#' 
#' @param L_prob 
#' The probability that the model assigns to choosing the left option.
#' 
#' @param R_prob 
#' The probability that the model assigns to choosing the left option.
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
#' @param value 
#' The expected value of the stimulus in the subject's mind at this point in time.
#'  
#' @param utility 
#' The subjective value that the subject assigns to the objective reward.
#'  
#' @param reward 
#' The objective reward received by the subject after selecting a stimulus.
#' 
#' @param LR 
#' Are you calculating the probability for the left option or the right option?
#'  
#' @param try 
#' If the choice was random, the value is 1; 
#' If the choice was based on value, the value is 0. 
#' 
#' @param occurrence 
#' The number of times the same stimulus has been chosen.
#' 
#' @param alpha [vector]
#' Extra parameters that may be used in functions. 
#'
#' @param beta [vector]
#' Extra parameters that may be used in functions. 
#' 
#' @return log-likelihood
#' 
#' @examples
#' \dontrun{
#' func_logl <- function(
#'   # Trial number
#'   i,
#'   # Number of times this option has appeared
#'   L_freq,
#'   R_freq,
#'   # Number of times this option has been chosen
#'   L_pick,
#'   R_pick,
#'   # Current value of this option
#'   L_value,
#'   R_value,
#'   # 
#'   L_dir,
#'   R_dir,
#'   #
#'   L_prob,
#'   R_prob,
#'   # Extra variables
#'   var1 = NA,
#'   var2 = NA,
#'
#'   # Whether calculating probability for left or right choice
#'   LR,
#'   # Is it a random choosing trial?
#'   try,
#'
#'   # Extra parameters
#'   alpha,
#'   beta
#' ){
#'   logl <- switch(
#'     EXPR = LR,
#'     "L" = L_dir * log(L_prob + 1e-10),
#'     "R" = R_dir * log(R_prob + 1e-10)
#'   )
#' }
#' }
#' 
func_logl <- function(
  # 试次序号
  i,
  # 该选项出现了几次
  L_freq,
  R_freq,
  # 该选项被选过几次
  L_pick,
  R_pick,
  # 该选项目前的价值
  L_value,
  R_value,
  # 真人选的左还是右
  L_dir,
  R_dir,
  # 模型估计选左或右的概率
  L_prob,
  R_prob,
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,  
  
  # 此时计算的是选左概率还是选右概率
  LR,
  # 是否随机尝试
  try,
  
  # 被选择选项之前的客观价值
  value, 
  # 被选择选项之前的主观价值
  utility,
  # 被选择选项给予的奖励
  reward, 
  # 第几次选择该选项
  occurrence, 
  
  # 额外参数
  alpha,
  beta
){
  logl <- switch(
    EXPR = LR,
    "L" = L_dir * log(L_prob + 1e-10),
    "R" = R_dir * log(R_prob + 1e-10)
  )
  
  return(logl)  
}