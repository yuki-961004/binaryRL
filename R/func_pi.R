#' Function: Upper-Confidence-Bound
#' @description
#' Unlike epsilon-greedy, which explores indiscriminately, UCB is a more
#'  intelligent exploration strategy. It biases the value of each action based 
#'  on how often it has been selected. For options chosen fewer times, or those 
#'  with high uncertainty, a larger "uncertainty bonus" is added to their 
#'  estimated value. This increases their selection probability, effectively 
#'  encouraging the exploration of potentially optimal, yet unexplored actions.
#'  A higher \code{pi} indicates a greater bias toward giving less-chosen 
#'  options.
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
#' @param LR 
#' Are you calculating the probability for the left option or the right option?
#' 
#' @param pi [vector]
#' Parameter used in the Upper-Confidence-Bound (UCB) action selection
#'  formula. \code{bias_func} controls the degree of exploration by scaling the 
#'  uncertainty bonus given to less-explored options. A larger value of 
#'  \code{pi} (denoted as \code{c} in Sutton and Barto(1998) ) increases the 
#'  influence of this bonus, leading to more exploration of actions with 
#'  uncertain estimated values. Conversely, a smaller \code{pi} results in 
#'  less exploration.
#'
#' \deqn{
#'   A_t = \arg \max_{a} \left[ V_t(a) + \pi \sqrt{\frac{\ln(t)}{N_t(a)}} \right]
#' }
#' 
#' \code{default: pi = NA}
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
#'   
#'   # Free parameter
#'   pi = 0.1,
#'   # Extra parameters
#'   alpha,
#'   beta
#' ){
#' ############################ [ at least 1 ] #################################
#'   if (is.na(x = pi)) {
#'     if (L_pick == 0 & R_pick == 0) {
#'       bias <- 100
#'     }
#'     else if (LR == "L" & L_pick == 0 & R_pick > 0) {
#'       bias <- 100
#'     }
#'     else if (LR == "R" & R_pick == 0 & L_pick > 0) {
#'       bias <- 100
#'     }
#'     else {
#'       bias <- 0
#'     }
#'   }
#' ############################ [ bias value ] #################################
#'   else if (!(is.na(x = pi)) & LR == "L") {
#'     bias <- pi * sqrt(log(L_pick + exp(1)) / (L_pick + 1e-10))
#'   }
#'   else if (!(is.na(x = pi)) & LR == "R") {
#'     bias <- pi * sqrt(log(R_pick + exp(1)) / (R_pick + 1e-10))
#'   }
#' ############################## [ error ] ####################################
#'   else {
#'     bias <- "ERROR"
#'   }
#'  
#'   return(bias)
#' }
#' }
#' 
func_pi <- function(
  i,
  L_freq,
  R_freq,
  L_pick,
  R_pick,
  L_value,
  R_value,
  var1,
  var2,
  
  LR,
  
  pi,
  alpha,
  beta
){
############################## [ at least 1 ] ##################################
  if (is.na(x = pi)) {
    if (L_pick == 0 & R_pick == 0) {
      bias <- 100
    }
    else if (LR == "L" & L_pick == 0 & R_pick > 0) {
      bias <- 100
    }
    else if (LR == "R" & R_pick == 0 & L_pick > 0) {
      bias <- 100
    }
    else {
      bias <- 0
    }
  }
############################## [ bias value ] ##################################
  else if (!(is.na(x = pi)) & LR == "L") {
    bias <- pi * sqrt(log(L_pick + exp(1)) / (L_pick + 1e-10))
  }
  else if (!(is.na(x = pi)) & LR == "R") {
    bias <- pi * sqrt(log(R_pick + exp(1)) / (R_pick + 1e-10))
  }
################################# [ error ] ####################################
  else {
    bias <- "ERROR"
  }
  
  return(bias)
}
