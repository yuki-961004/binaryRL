#' ex_func_eta
#'
#' @param value The value you assign to this stimulus at this moment
#' @param reward The reward given to you by the experimental procedure after choosing this stimulus
#' @param occurrence The number of times this stimulus is encountered
#' @param params parameters
#'
#' @return learning rate eta
#' @export
#'
ex_func_eta <- function(value, reward, occurrence, params) {
  if (is.na(reward)) {
    stop()
  }
  ################################ [ RSTD ] ####################################
  else if (reward > value) {
    eta <- params[1]
  } 
  else if (reward <= value) {
    eta <- params[2]
  }
  else {
    eta <- "ERROR" # 检查错误
  }
  return(eta)
}