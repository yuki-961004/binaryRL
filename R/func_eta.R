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
#' @note When customizing these functions, please ensure that you do not modify 
#' the arguments. Instead, only modify the `if-else` statements or the internal 
#' logic to adapt the function to your needs.
#'
#' @param value The expected value of the stimulus in the subject's mind at 
#'  this point in time.
#'  
#' @param utility The subjective value that the subject assigns to the 
#'  objective reward.
#'  
#' @param reward The objective reward received by the subject after selecting 
#'  a stimulus.
#'  
#' @param occurrence The number of times the same stimulus has appeared.
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
#' @param eta [numeric]
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
#'   # Expected value for this stimulus
#'   value,
#'   # Subjective utility
#'   utility,
#'   # Reward observed after choice
#'   reward,
#'   # Occurrence count for this stimulus
#'   occurrence,
#'   # Extra variables
#'   var1 = NA,
#'   var2 = NA,
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
#'   else if (length(eta) > 1 & utility < value) {
#'     eta <- eta[1]
#'   }
#'   else if (length(eta) > 1 & utility >= value) {
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
  # 此时心中对该刺激的的value
  value, 
  # 心中的主观价值
  utility,
  # 选择后看到的reward
  reward, 
  # 第几次看到这个刺激
  occurrence, 
  # 额外变量
  var1 = NA,
  var2 = NA,
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
