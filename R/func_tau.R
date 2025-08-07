#' Function: Soft-Max Function
#' @description
#' The softmax function describes a probabilistic choice rule. It implies
#'  that options with higher subjective values are chosen with a greater
#'  probability, rather than deterministic. This probability of choosing
#'  the higher-valued option increases with the parameter \code{tau}.
#'  A higher \code{tau} indicates greater sensitivity to value differences, making
#'  choices more deterministic.
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
#' The value of the left option with bias (if pi != 0)
#' 
#' @param R_value 
#' The value of the right option with bias (if pi != 0)
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
#' @param LR 
#' Are you calculating the probability for the left option or the right option?
#'  
#' @param try 
#' If the choice was random, the value is 1; 
#' If the choice was based on value, the value is 0.
#' 
#' @param lapse [numeric]
#' A numeric value between 0 and 1, representing the lapse rate.
#' 
#' You can interpret this parameter as the probability of the agent "slipping"
#'  or making a random choice, irrespective of the learned action values. This
#'  accounts for moments of inattention or motor errors. In this sense, it
#'  represents the minimum probability with which any given option will be
#'  selected. It is a free parameter that acknowledges that individuals do not
#'  always make decisions with full concentration throughout an experiment.
#'  
#' From a modeling perspective, the lapse rate is crucial for preventing the
#'  log-likelihood calculation from returning \code{-Inf}. This issue arises 
#'  when the model assigns a probability of zero to an action that the 
#'  participant actually chose (\code{log(0)} is undefined). By ensuring every 
#'  option has a non-zero minimum probability, the \code{lapse} parameter makes 
#'  the fitting process more stable and robust against noise in the data.
#'  
#'  \deqn{
#'    P_{final} = (1 - lapse) \cdot P_{softmax} + \frac{lapse}{N_{choices}}
#'  }
#'  
#' \code{default: lapse = 0.02} 
#' 
#' This ensures each option has a minimum selection probability of 1 percent 
#'  in TAFC tasks. 
#' 
#' @param tau [vector] 
#' Parameters used in the Soft-Max Function. \code{prob_func} 
#'  representing the sensitivity of the subject to the value difference when 
#'  making decisions. It determines the probability of selecting the left option 
#'  versus the right option based on their values. A larger value of tau 
#'  indicates greater sensitivity to the value difference between the options. 
#'  In other words, even a small difference in value will make the subject more 
#'  likely to choose the higher-value option. 
#'  
#'  \deqn{P_L = \frac{1}{1+e^{-(V_L-V_R) \cdot \tau}}; P_R = \frac{1}{1+e^{-(V_R-V_L) \cdot \tau}}} 
#' 
#'  \code{e.g., tau = c(0.5)}
#' 
#' @param alpha [vector]
#' Extra parameters that may be used in functions. 
#'
#' @param beta [vector]
#' Extra parameters that may be used in functions. 
#' 
#' @return The probability of choosing this option
#' 
#' @examples
#' \dontrun{
#' func_tau <- function(
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
#'   # Whether calculating probability for left or right choice
#'   LR,
#'   # Is it a random choosing trial?
#'   try,
#'
#'   # Free parameters
#'   tau,
#'   # Extra parameters
#'   alpha,
#'   beta
#' ){
#' ############################### [ random ] ##################################
#'   if (try == 1) {
#'     prob <- 0.5
#'   }
#' ############################# [ greedy-max ] ################################
#'   else if (try == 0 & LR == "L" & is.na(tau)) {
#'     if (L_value == R_value) {
#'       prob <- 0.5
#'     }
#'     else if (L_value > R_value) {
#'       prob <- 1
#'     }
#'     else if (L_value < R_value) {
#'       prob <- 0
#'     }
#'   }
#'   else if (try == 0 & LR == "R" & is.na(tau)) {
#'     if (L_value == R_value) {
#'       prob <- 0.5
#'     }
#'     else if (R_value > L_value) {
#'       prob <- 1
#'     }
#'     else if (R_value < L_value) {
#'       prob <- 0
#'     }
#'   }
#' ############################### [ soft-max ] ################################
#'   else if (try == 0 & LR == "L" & !(is.na(tau))) {
#'     prob <- 1 / (1 + exp(-(L_value - R_value) * tau))
#'   }
#'   else if (try == 0 & LR == "R" & !(is.na(tau))) {
#'     prob <- 1 / (1 + exp(-(R_value - L_value) * tau))
#'   }
#' ################################ [ error ] ##################################
#'   else {
#'     prob <- "ERROR"
#'   }
#' ################################ [ lapse ] ##################################  
#' 
#'   prob <- (1 - lapse) * prob + (lapse / 2)
#'
#'   return(prob)
#' }
#' }
#' 
func_tau <- function(
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
  
  # 此时计算的是选左概率还是选右概率
  LR,
  # 是否随机尝试
  try,
  
  # 自由参数
  lapse,
  tau,
  # 额外参数
  alpha,
  beta
){
################################# [ random ] ###################################  
  if (try == 1) {
    prob <- 0.5
  } 
############################### [ greedy-max ] #################################
  else if (try == 0 & LR == "L" & is.na(tau)) {
    if (L_value == R_value) {
      prob <- 0.5
    } 
    else if (L_value > R_value) {
      prob <- 1
    }
    else if (L_value < R_value) {
      prob <- 0
    }
  }
  else if (try == 0 & LR == "R" & is.na(tau)) {
    if (L_value == R_value) {
      prob <- 0.5
    } 
    else if (R_value > L_value) {
      prob <- 1
    }
    else if (R_value < L_value) {
      prob <- 0
    }
  }
################################ [ soft-max ] ##################################
  else if (try == 0 & LR == "L" & !(is.na(tau))) {
    prob <- 1 / (1 + exp(-(L_value - R_value) * tau))
  }
  else if (try == 0 & LR == "R" & !(is.na(tau))) {
    prob <- 1 / (1 + exp(-(R_value - L_value) * tau))
  }
################################# [ error ] ####################################
  else {
    prob <- "ERROR"
  }
################################# [ lapse ] ####################################  
  
  prob <- (1 - lapse) * prob + (lapse / 2)
  
  return(prob)
}
