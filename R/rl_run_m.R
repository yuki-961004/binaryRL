#' rl_run_m
#'
#' @param data raw data
#' @param sub The column name for the subject ID
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param L_choice The column name for the left option
#' @param R_choice The column name for the right option
#' @param choose The column name indicating which option the subject chose
#' @param var1 extra variable 1
#' @param var2 extra variable 2
#' @param id the subject being analysised
#' @param initial_value The initial value you assign to a stimulus, defaulting to NA
#' @param epsilon In some models, the learning rate is divided into different intervals.
#' @param beta In the utility model, it is assumed that all rewards will be discounted
#' @param eta In the RSTD model, the learning rate is different for positive and negative conditions.
#' @param tau Indicates the sensitivity of the subjects to the two options
#' @param lambda extra param in soft-max function
#' @param beta_func The function for the discount rate β, which you can customize
#' @param eta_func The function for the learning rate η, which you can customize
#' @param prob_func The soft-max function, which you can customize.
#' @param seed seed
#' @param softmax use softmax or not, defaulting to TRUE
#' @param digits_1 digits in update v, default is 2
#' @param digits_2 digits in action c, default is 5
#'
#' @return A table showing how participants updated their values ​​and selected
#' @export
#'
rl_run_m <- function(
  data, 
  # 被试序号列
  sub = "Subject",
  # 价值更新的时间线, 基于的列
  time_line = c("Block", "Trial"),
  # 左右选项
  L_choice = "DL",
  R_choice = "DR",
  # 被试选择列
  choose = "Choose",
  # 额外需要使用的列1
  var1 = NA,
  # 额外需要使用的列2
  var2 = NA,
  # 需要分析的被试的序号
  id = 1,
  # 初始值
  initial_value = NA,
  # 参数们
  epsilon = NA,
  beta = 1,
  eta,
  tau,
  lambda = NA,
  # 示例函数
  beta_func = func_beta,
  eta_func = func_eta,
  prob_func = func_prob, 
  # 其他设定
  seed = 123,
  softmax = FALSE,
  digits_1 = 2, 
  digits_2 = 5
) {
  # 第一步, 生成Value
  step1 <- loop_update_v(
    data = data, 
    sub = sub,
    choose = choose,
    time_line = time_line,
    var1 = var1,
    var2 = var2,
    n = id,
    initial_value = initial_value,
    epsilon = epsilon,
    beta = beta,
    eta = eta,
    beta_func = beta_func,
    eta_func = eta_func,
    digits = digits_1
  ) 
  # 第二步, 基于左右的Value, 计算选择左右的概率
  step2 <- loop_action_c(
    data = step1,
    L_choice = L_choice,
    R_choice = R_choice,
    sub = sub,
    choose = choose,
    value = "V_value", # step1 会产生这一列
    var1 = var1,
    var2 = var2,
    n = 1, # 一次分析一个人
    initial_value = 0, # 比较无所谓的值. 但是必须有
    seed = seed,
    softmax = softmax,
    tau = tau,
    lambda = lambda,
    prob_func = prob_func,
    digits = digits_2
  )
  
  return(step2)
}