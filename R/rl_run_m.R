#' rl_run_m
#' @description
#' This function is designed to be used in conjunction with the `algorithm` 
#' package. Users can define their own `value function` and `action function` 
#' to model the decision-making process. The function allows users to set 
#' certain parameters as free parameters, which will be optimized during the 
#' algorithm's execution. By integrating this function with the `algorithm` 
#' package, users can apply advanced optimization techniques to solve for the 
#' values of these free parameters. This approach facilitates flexible and 
#' customizable modeling of decision-making tasks, where the parameters' optimal 
#' values are learned through computational algorithms.
#' 
#' @param data A data frame containing the raw data. 
#' This data should include the following mandatory columns: 
#' - "sub", "time_line", "L_choice", "R_choice", "choose", "reward". 
#' The following arguments allow you to customize the column names used for processing
#' 
#' @param id A numeric value specifying the subject ID for which the model is being analyzed. 
#' The value should correspond to an entry in the "sub" column, which must contain the subject IDs. 
#' Provide the subject ID as a number.
#' e.g., `id = 18`
#' 
#' @param initial_value A numeric value representing the subject's initial expected value for each stimulus's reward. 
#' If this value is not set, the subject will use the reward received after the first trial as the initial value for that stimulus. 
#' In other words, the learning rate for the first trial is 100%. 
#' Provide the initial value as a number 
#' default: `initial_value = NA`
#' e.g., `initial_value = 0`
#' 
#' @param threshold A numeric value specifying the number of initial trials during which the subject makes random choices 
#' rather than choosing based on the values of the options. This occurs because the subject has not yet learned the values of the options. 
#' For example, threshold = 20 means the subject will make completely random choices for the first 20 trials.
#' Provide the value as a number. 
#' default: `threshold = 1`
#' 
#' @param n_params The number of free parameters in the model. 
#' @param n_trials The total number of trials in the experiment.
#'
#' 
#' 
#' @param lambda An additional parameter that may be used in these functions. 
#' Provide the value as a vector 
#' e.g., `lambda = c(0.4, 0.7, 20, 60)`
#' 
#' @param gamma A parameter used in the `util_func` (Utility Function), often 
#' referred to as the discount rate. For example, in the utility equation 
#' `utility = gamma * reward`, if gamma < 1, it indicates that people discount 
#' the objective reward. 
#' Provide the value as a vector 
#' e.g., `gamma = c(0.7)`
#' 
#' @param eta A parameter used in the `rate_func` (Learning Rate Function), 
#' representing the rate at which the subject updates the difference between the reward and the expected value in the subject's mind. 
#' In the TD model, there is a single learning rate throughout the experiment. 
#' In the RSTD model, two different learning rates are used when the reward is higher or lower than the expected value.
#' Provide the value as a vector 
#' e.g., `eta = c(0.3, 0.7)`
#' 
#' @param epsilon A parameter used in the `expl_func` (Exploration Function), 
#' determining whether the subject makes decisions based on the relative values of the left and right options, 
#' or chooses completely randomly. 
#' For example, when epsilon = 0.1, it means the subject has a 10% chance of making a completely random choice 
#' and a 90% chance of choosing based on the values of the options.
#' Provide the value as a vector 
#' e.g., `epsilon = c(0.1)`
#' 
#' @param tau A parameter used in the `prob_func` (Soft-Max Function), representing the sensitivity of the subject to the value difference when making decisions. 
#' It determines the probability of selecting the left option versus the right option based on their values. A larger value of tau indicates greater sensitivity 
#' to the value difference between the options. In other words, even a small difference in value will make the subject more likely to choose the higher-value option. 
#' Provide the value as a vector 
#' e.g., `tau = c(0.5)`
#' 
#' @param util_func The function for the utility gamma, which you can customize
#' @param rate_func The function for the learning rate eta, which you can customize
#' @param expl_func The function for the epsilon greedy, which you can customize
#' @param prob_func The function for the temperature tau, which you can customize.
#'
#' 
#' 
#' @param sub A string specifying the name of the column that contains the subject ID.  
#' Provide the name of the column as a character string  
#' e.g., `sub = "Subject_ID"`
#' 
#' @param time_line A vector specifying the name of the column that the sequence of the experiment. 
#' This argument defines how the experiment is structured, such as whether it is organized by "Block" with breaks in between, and multiple trials within each block. 
#' Provide the sequence as a character vector, 
#' e.g., `time_line = c("Block", "Trial")`
#' 
#' @param L_choice A string specifying the name of the column that represents the left choice. 
#' Provide the name of the column as a character string 
#' e.g., `L_choice = "Left_Choice"`
#' 
#' @param R_choice A string specifying the name of the column that represents the right choice. 
#' Provide the name of the column as a character string 
#' e.g., `R_choice = "Right_Choice"`
#' 
#' @param choose A string specifying the name of the column that represents the choice made by the subject. 
#' Provide the name of the column as a character string 
#' e.g., `choose = "Choose"`
#' 
#' @param reward A string specifying the name of the column that represents the reward associated with the subject's choice. 
#' Provide the name of the column as a character string 
#' e.g., `reward = "Reward"`
#' 
#' @param var1 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var1 = "Extra_Var1"`
#' 
#' @param var2 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var2 = "Extra_Var2"`
#'
#' 
#' 
#' @param softmax A logical value indicating whether to use the softmax function. 
#' When softmax = TRUE, the value of each option influences the probability of selecting that option. 
#' Higher values increase the probability of selecting that option. 
#' When softmax = FALSE, the subject will always choose the option with the higher value, with no possibility of selecting the lower-value option.
#' Provide the value as a logical (TRUE or FALSE).
#' default: `softmax = TRUE`
#' 
#' @param seed A numeric value to set the random seed. 
#' This ensures that the results are reproducible and remain the same each time the function is run.
#' Provide the value as a number. 
#' default: `seed = 123`
#' 
#' @param digits_1 The number of decimal places to retain for values related to the value function. 
#' The default is 2.
#' 
#' @param digits_2 The number of decimal places to retain for values related to the action function. 
#' The default is 5.
#' 
#' @return A table showing how participants updated their values ​​and selected
#' @export
#' @examples
#' data <- TAFC
#' 
#' binaryRL_res <- binaryRL::rl_run_m(
#'   data = data,                    
#'   id = 18,                       
#'   eta = c(0.321, 0.765),          
#'   n_params = 2,                   
#'   n_trials = 288                 
#' )
#' 
#' summary(binaryRL_res)
rl_run_m <- function(
  data, 
  id,
  initial_value = NA,
  threshold = 1,
  n_params,
  n_trials,
  
  gamma = 1,
  eta,
  epsilon = NA,
  tau = 1,
  lambda = NA,
  util_func = func_gamma,
  rate_func = func_eta,
  expl_func = func_epsilon,
  prob_func = func_tau, 
  
  sub = "Subject",
  time_line = c("Block", "Trial"),
  L_choice = "L_choice",
  R_choice = "R_choice",
  choose = "Choose",
  reward = "Reward",
  var1 = NA,
  var2 = NA,
  
  softmax = TRUE,
  seed = 123,

  digits_1 = 2, 
  digits_2 = 5
) {
  step0 <- data
  
  # 第一步, 基于奖励, 不同刺激独立更新价值
  step1 <- loop_update_v(
    data = step0, 
    n = id,
    initial_value = initial_value,

    lambda = lambda,
    gamma = gamma,
    eta = eta,
    
    util_func = util_func,
    rate_func = rate_func,

    sub = sub,
    choose = choose,
    reward = reward,
    time_line = time_line,
    var1 = var1,
    var2 = var2,

    digits = digits_1
  ) 
  
  # 第二步, 基于左右选项价值, 进行选择
  step2 <- loop_action_c(
    data = step1,
    n = 1,
    initial_value = 0,
    threshold = threshold,

    lambda = lambda,
    epsilon = epsilon,
    tau = tau,
    expl_func = expl_func,
    prob_func = prob_func,

    sub = sub,
    L_choice = L_choice,
    R_choice = R_choice,
    choose = choose,
    value = "V_value",
    var1 = var1,
    var2 = var2,
    
    seed = seed,
    softmax = softmax,
    
    digits = digits_2
  )
  
  params <- list(
    lambda = c(lambda),
    gamma = c(gamma),
    eta = c(eta), 
    epsilon = c(epsilon),
    tau = c(tau)
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
  
  class(res) <- c("binaryRL") 
  
  return(res)
}