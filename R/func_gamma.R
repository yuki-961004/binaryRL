#' Function: Utility Function
#' 
#' @description
#' This function represents an exponent used in calculating utility
#' from reward. Its application varies depending on the specific model:
#'  \itemize{
#'    \item \strong{Stevens' Power Law}:
#'    Here, utility is calculated by raising the reward to the power
#'    of \code{gamma}. This describes how the subjective value (utility) of a
#'    reward changes non-linearly with its objective magnitude.
#'
#'    \item \strong{Kahneman's Prospect Theory}:
#'    This theory applies exponents differently for gains and losses,
#'    and introduces a loss aversion coefficient:
#'    \itemize{
#'      \item For positive rewards (gains), utility is the reward
#'      raised to the power of \code{gamma[1]}.
#'      \item For negative rewards (losses), utility is calculated
#'      by first multiplying the reward by \code{beta}, and then raising
#'      this product to the power of \code{gamma[2]}. Here, \code{beta} acts 
#'      as a loss aversion parameter, accounting for the greater psychological 
#'      impact of losses compared to equivalent gains.
#'    }
#'  }
#' 
#' @note When customizing these functions, please ensure that you do not modify 
#' the arguments. Instead, only modify the `if-else` statements or the internal 
#' logic to adapt the function to your needs.
#'
#' @param value The expected value of the stimulus in the subject's mind at 
#'  this point in time.
#' @param utility The subjective value that the subject assigns to the 
#'  objective reward.
#' @param reward The objective reward received by the subject after selecting 
#'  a stimulus.
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
#' @param gamma [vector]
#' This parameter represents the exponent in utility functions, specifically:
#'  \itemize{
#'    \item \strong{Stevens' Power Law}:
#'    Utility is modeled as:
#'    \deqn{U = {R}^{\gamma}}
#'
#'    \item \strong{Kahneman's Prospect Theory}:
#'    This exponent is applied differently based on the sign of the reward:
#'    \deqn{U = \begin{cases}
#'      R^{\gamma_{1}}, & R > 0 \\
#'      \beta \cdot R^{\gamma_{2}}, & R < 0
#'    \end{cases}}
#'  }
#' 
#' @param alpha [vector]
#' Extra parameters that may be used in functions. 
#'
#' @param beta [vector]
#' Extra parameters that may be used in functions. 
#' 
#' @return Discount rate and utility
#'
#' @examples
#' \dontrun{
#' func_gamma <- function(
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
#'   gamma = 1,
#'   # Extra parameters
#'   alpha,
#'   beta
#' ){
#' ############################## [ Utility ] ##################################
#'   if (length(gamma) == 1) {
#'     gamma <- as.numeric(gamma)
#'     utility <- sign(reward) * (abs(reward) ^ gamma)
#'   }
#' ############################### [ Error ] ###################################
#'   else {
#'     utility <- "ERROR" 
#'   }
#'   return(list(gamma, utility))
#' }
#' }
#' 
func_gamma <- function(
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
  gamma = 1,
  # 额外参数
  alpha,
  beta
){
################################# [ Utility ] ##################################
  if (length(gamma) == 1) {
    gamma <- as.numeric(gamma)
    utility <- sign(reward) * (abs(reward) ^ gamma)
  }
################################## [ Error ] ###################################
  else {
    utility <- "ERROR" # 检查错误
  }
  return(list(gamma, utility))
}
