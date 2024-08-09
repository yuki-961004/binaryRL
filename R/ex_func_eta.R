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
ex_func_eta <- function(
  # 此时心中对该刺激的的value
  value, 
  # 选择后看到的reward
  reward, 
  # 第几次看到这个刺激
  occurrence, 
  # 使用的参数
  params
  ################################# [function start] #############################
){
  if (is.na(reward)) {
    stop()
  }
  ################################### [ RSTD ] ###################################
  else if (reward > value) {
    eta <- params[1]
  } 
  else if (reward <= value) {
    eta <- params[2]
  }
  ################################### [ RSTD ] ###################################
  else {
    eta <- "ERROR" # 检查错误
  }
  return(eta)
}