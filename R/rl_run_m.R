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
#' @param gamma In the utility model, it is assumed that all rewards will be discounted
#' @param eta In the RSTD model, the learning rate is different for positive and negative conditions.
#' @param tau Indicates the sensitivity of the subjects to the two options
#' @param lambda extra param in soft-max function
#' @param seed seed
#' @param softmax use softmax or not, defaulting to TRUE
#' @param digits_1 digits in update v, default is 2
#' @param digits_2 digits in action c, default is 5
#' @param n_params number of parameters
#' @param n_trials number of trails
#' @param utility_func The function for the discount rate β, which you can customize
#' @param rate_func The function for the learning rate η, which you can customize
#' @param prob_func The soft-max function, which you can customize.
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
    # 决策时情景对应的期望价值
    var1 = NA,
    # 决策时情景对应的框架名称
    var2 = NA,
    # 被试id
    id = 1,
    # 初始值
    initial_value = NA,
    # parameters
    epsilon = NA,
    gamma = 1,
    eta,
    tau,
    lambda = NA,
    seed = 123,
    softmax = FALSE,
    digits_1 = 2, 
    digits_2 = 5,
    # 示例函数
    n_params,
    n_trials,
    utility_func = func_gamma,
    rate_func = func_eta,
    prob_func = func_tau 
) {
  step0 <- data
  
  # 第一步, 基于奖励, 不同刺激独立更新价值
  step1 <- loop_update_v(
    data = step0, 
    sub = sub,
    choose = choose,
    time_line = time_line,
    var1 = var1,
    var2 = var2,
    n = id,
    initial_value = initial_value,
    epsilon = epsilon,
    gamma = gamma,
    eta = eta,
    utility_func = utility_func,
    rate_func = rate_func,
    digits = digits_1
  ) 
  
  # 第二步, 基于左右选项价值, 进行选择
  step2 <- loop_action_c(
    data = step1,
    L_choice = L_choice,
    R_choice = R_choice,
    sub = sub,
    choose = choose,
    value = "V_value",
    var1 = var1,
    var2 = var2,
    n = 1,
    initial_value = 0,
    seed = seed,
    softmax = softmax,
    tau = tau,
    lambda = lambda,
    prob_func = prob_func,
    digits = digits_2
  )
  
  params <- list(
    epsilon = c(epsilon),
    gamma = c(gamma),
    eta = c(eta),   
    tau = c(tau),
    lambda = c(lambda)
  )
  
  mean_ACC <- round(mean(step2$ACC), 4) * 100
  sum_LL <- round(sum(step2$L_logl) + sum(step2$R_logl), digits = 2)
  AIC <- round(2 * n_params - 2 * sum_LL, digits = 2)
  BIC <- round(n_params * log(n_trials) - 2 * sum_LL, digits = 2)
  
  res <- list(
    data = step2,
    params = params,
    acc = mean_ACC,
    ll = sum_LL,
    aic = AIC,
    bic = BIC
  )
  
  return(res)
}