#' output
#'
#' @param data data A dataframe resulting from the 'step7' process of the `digits` function. 
#' @param n_params The number of free parameters in the model. 
#' @param n_trials The total number of trials in the experiment.
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
#' @returns step8
#' @export
#'
#' @examples
#' data <- TAFC[TAFC$Subject == 1, ]
#' 
#' step1 <- unique_choice(
#'   data = data,
#'   L_choice = "L_choice", 
#'   R_choice = "R_choice"
#'  )
#'  
#' step2 <- arrange_data(
#'   data = step1[[1]],
#'   time_line = c("Block", "Trial")
#' )
#' 
#' step3 <- add_NA(
#'   data = step2
#' )
#' 
#' step4 <- set_initial_value(
#'   data = step3, 
#'   options = step1[["options"]], 
#'   initial_value = NA
#' )
#' step5 <- decision_making(
#'   data = step4,
#'   options = step1[["options"]],
#'   L_choice = "L_choice", R_choice = "R_choice",
#'   L_reward = "L_reward", R_reward = "R_reward",
#'   softmax = TRUE,
#'   threshold = 1,
#'   initial_value = NA,
#'   
#'   lambda = NA,
#'   gamma = 1,
#'   eta = c(0.3, 0.7),
#'   epsilon = NA,
#'   tau = 0.5
#' )
#' 
#' step6 <- model_fit(
#'   data = step5, 
#'   L_choice = "L_choice",
#'   R_choice = "R_choice",
#'   sub_choose = "Choose"
#' )
#' 
#' step7 <- digits(
#'   data = step6, 
#'   options = step1[["options"]],
#'   digits_1 = 2, 
#'   digits_2 = 5
#' )
#' 
#' step8 <- output(
#'   data = step7,
#'   n_params = 2,
#'   n_trials = 288,
#'   lambda = NA,
#'   gamma = 1,
#'   eta = c(0.3, 0.7),
#'   epsilon = NA,
#'   tau = 0.5
#'  )
output <- function(
    data, 
    n_params, n_trials, 
    gamma, eta, epsilon, tau, lambda
){
  params <- list(
    lambda = c(lambda),
    gamma = c(gamma),
    eta = c(eta), 
    epsilon = c(epsilon),
    tau = c(tau)
  )
  
  mean_ACC <- round(mean(data$ACC), 4) * 100
  sum_LL <- round(sum(data$L_logl) + sum(data$R_logl), digits = 2)
  AIC <- round(2 * n_params - 2 * sum_LL, digits = 2)
  BIC <- round(n_params * log(n_trials) - 2 * sum_LL, digits = 2)
  
  res <- list(
    data = data,
    params = params,
    acc = mean_ACC,
    ll = sum_LL,
    aic = AIC,
    bic = BIC
  )
  
  class(res) <- c("binaryRL")
  
  return(res)
}