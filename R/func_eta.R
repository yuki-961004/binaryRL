#' Function: Learning Rate
#'
#' @description
#' The structure of \code{eta} depends on the model type:
#'  \itemize{
#'    \item \strong{Temporal Difference (TD) model}:
#'      \code{eta} is a single numeric value representing the learning rate.
#'
#'    \item \strong{Risk-Sensitive Temporal Difference (RSTD) model}:
#'      \code{eta} is a numeric vector of length two, where 
#'      \code{eta[1]} represents the learning rate for "good" outcomes, which 
#'      means the reward is higher than the expected value.
#'      \code{eta[2]} represents the learning rate for "bad" outcomes, which 
#'      means the reward is lower than the expected value.
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
#' @param occurrence 
#' The number of times the same stimulus has been chosen.
#' 
#' @param eta [vector]
#' Parameters used in the Learning Rate Function, \code{rate_func}, representing
#'  the rate at which the subject updates the difference (prediction error)
#'  between the reward and the expected value in the subject's mind.
#'
#'  The structure of \code{eta} depends on the model type:
#'  \itemize{
#'    \item For the \strong{Temporal Difference (TD) model}, 
#'    where a single learning rate is used throughout the experiment 
#'    \deqn{V_{new} = V_{old} + \eta \cdot (R - V_{old})}
#'    
#'    \item For the \strong{Risk-Sensitive Temporal Difference (RDTD) model},
#'    where two different learning rates are used depending on whether the 
#'    reward is lower or higher than the expected value:
#'    \deqn{V_{new} = V_{old} + \eta_{+} \cdot (R - V_{old}), R > V_{old}}
#'    \deqn{V_{new} = V_{old} + \eta_{-} \cdot (R - V_{old}), R < V_{old}}
#'  }
#'  
#'  \code{TD: eta = 0.3}
#'  
#'  \code{RSTD: eta = c(0.3, 0.7)}
#' 
#' @param alpha [vector]
#' Extra parameters that may be used in functions. 
#'
#' @param beta [vector]
#' Extra parameters that may be used in functions. 
#' 
#' @return learning rate eta
#' @examples
#' \dontrun{
#' func_eta <- function(
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
#'   # Extra variables
#'   var1 = NA,
#'   var2 = NA,
#'   
#'   # Expected value for this stimulus
#'   value,
#'   # Subjective utility
#'   utility,
#'   # Reward observed after choice
#'   reward,
#'   # Occurrence count for this stimulus
#'   occurrence,
#'   
#'   # Free Parameter
#'   eta,
#'   # Extra parameters
#'   alpha,
#'   beta
#' ){
#' ################################# [ TD ] ####################################
#'   if (length(eta) == 1) {
#'     eta <- as.numeric(eta)
#'   }
#' ################################ [ RSTD ] ###################################
#'   else if (length(eta) == 2 & utility < value) {
#'     eta <- eta[1]
#'   }
#'   else if (length(eta) == 2 & utility >= value) {
#'     eta <- eta[2]
#'   }
#' ################################ [ ERROR ] ##################################
#'   else {
#'     eta <- "ERROR" # Error check
#'   }
#'   return(eta)
#' }
#' }
#' 
func_eta <- function(
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
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,  
  
  # 被选择选项之前的客观价值
  value, 
  # 被选择选项之前的主观价值
  utility,
  # 被选择选项给予的奖励
  reward, 
  # 第几次选择该选项
  occurrence, 

  # 自由参数
  eta,
  # 额外参数
  alpha,
  beta
){
#################################### [ TD ] ####################################
  if (length(eta) == 1) {
    eta <- as.numeric(eta)
  }
################################### [ RSTD ] ###################################
  else if (length(eta) == 2 & utility < value) {
    eta <- eta[1]
  } 
  else if (length(eta) == 2 & utility >= value) {
    eta <- eta[2]
  }
################################## [ ERROR ] ###################################
  else {
    eta <- "ERROR" # 检查错误
  }
  return(eta)
}
