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
#' @param gamma [vector]
#' This parameter represents the exponent in utility functions, \code{util_func}, 
#'  specifically:
#'  \itemize{
#'    \item \strong{Stevens' Power Law}:
#'    Utility is modeled as:
#'    \deqn{U(R) = {R}^{\gamma}}
#'
#'    \item \strong{Kahneman's Prospect Theory}:
#'    This exponent is applied differently based on the sign of the reward:
#'    \deqn{U(R) = \begin{cases}
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
