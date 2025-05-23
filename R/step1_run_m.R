#' Step 1: Building reinforcement learning model
#'
#' @description
#'  This function requires the optimal parameter values obtained through the 
#'  `algorithm` package. Once the best parameter values are solved for, they 
#'  are incorporated into the reinforcement learning model, allowing the model 
#'  to simulate human-like decision-making. The function leverages these 
#'  optimized parameters to generate choices that mimic the decision-making 
#'  process of subjects, enabling the study of behavior under varying 
#'  conditions. By integrating the best-fit parameters from the `algorithm` 
#'  package, this function offers a powerful tool for simulating human choices 
#'  in reinforcement learning contexts.
#' 
#'  For more information, please refer to the GitHub repository:
#'  https://github.com/yuki-961004/binaryRL
#' 
#' @param data [data.frame] raw data. 
#'  This data should include the following mandatory columns: 
#'   \itemize{
#'     \item "sub"
#'     \item "time_line" (e.g., "Block", "Trial")
#'     \item "L_choice"
#'     \item "R_choice"
#'     \item "L_reward"
#'     \item "R_reward"
#'     \item "sub_choose"
#'   }
#'  
#' @param mode [character] This parameter has three possible values: 
#'  `simulate`, `fit`, and `review`. These correspond to their use 
#'  in `rcv_d`, `fit_p`, and `rev_e` respectively. In most cases, you won't need
#'  to modify this, as suitable default values are set for different contexts.
#'  
#' @param raw_cols [vector] Defaults to `NULL`. If left as `NULL`, it will
#'  directly capture all column names from the raw data.
#' 
#' @param id [integer] which subject is going to be analyzed.
#'  is being analyzed. The value should correspond to an entry in the "sub" 
#'  column, which must contain the subject IDs. 
#'  e.g., `id = 18`
#' 
#' @param initial_value [numeric] subject's initial expected value for each 
#'  stimulus's reward. If this value is not set (`initial_value = NA`), 
#'  the subject will use the reward received after the first trial as the 
#'  initial value for that stimulus. In other words, the learning rate for the 
#'  first trial is 100%. default: `initial_value = NA` 
#'  e.g., `initial_value = 0`
#' 
#' @param softmax [logical] whether to use the softmax function. 
#'  When `softmax = TRUE`, the value of each option influences the probability 
#'  of selecting that option. Higher values increase the probability of 
#'  selecting that option. When `softmax = FALSE`, the subject will always 
#'  choose the option with the higher value, with no possibility of selecting 
#'  the lower-value option. default: `softmax = TRUE`
#' 
#' @param threshold [integer] the number of initial trials during which the 
#'  subject makes random choices rather than choosing based on the values of 
#'  the options. This occurs because the subject has not yet learned the values 
#'  of the options. For example, `threshold = 20` means the subject will make 
#'  completely random choices for the first 20 trials. default: `threshold = 1`
#' 
#' @param seed [integer] random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  default: `seed = 123` 
#' 
#' @param n_params [integer] The number of free parameters in your model. 
#' 
#' @param n_trials [integer] The total number of trials in your experiment.
#'
#' @param lambda [vector] Extra parameters that may be used in functions. 
#'  e.g., `lambda = c(0.4, 0.7, 20, 60)`
#' 
#' @param gamma [vector] Parameters used in the Utility Function 
#'  `util_func`, often referred to as the discount rate. For example,
#'  `utility = reward^gamma`. If `gamma < 1`, it indicates that people
#'  tend to discount the objective reward. This equation is very similar
#'  to the Stevens' power function, reflecting humans' nonlinear perception
#'  of physical quantities. 
#'  e.g., `gamma = c(0.7)`.
#' 
#' @param eta [vector] Parameters used in the Learning Rate Function 
#' `rate_func` representing the rate at which the subject updates the 
#'  difference (prediction error) between the reward and the expected value 
#'  in the subject's mind. In the TD model, there is a single learning rate 
#'  throughout the experiment. In the RSTD model, two different learning rates 
#'  are used when the reward is higher or lower than the expected value.
#'  e.g., `eta = c(0.3, 0.7)`
#' 
#' @param epsilon [vector] Parameters used in the Exploration Function
#' `expl_func` determining whether the subject makes decisions based on the 
#'  relative values of the left and right options, or chooses completely 
#'  randomly. For example, when epsilon = 0.1, it means the subject has a 10% 
#'  chance of making a completely random choice and a 90% chance of choosing 
#'  based on the values of the options.
#'  e.g., `epsilon = c(0.1)`
#' 
#' @param tau [vector] Parameters used in the Soft-Max Function 
#' `prob_func` representing the sensitivity of the subject to the value 
#'  difference when making decisions. It determines the probability of selecting 
#'  the left option versus the right option based on their values. A larger 
#'  value of tau indicates greater sensitivity to the value difference between 
#'  the options. In other words, even a small difference in value will make the 
#'  subject more likely to choose the higher-value option. 
#'  e.g., `tau = c(0.5)`
#' 
#' @param util_func [function] Utility Function.
#' 
#' @param rate_func [function] Learning Rate Function.
#' 
#' @param expl_func [function] Exploration Function.
#' 
#' @param prob_func [function] Soft-Max Function.
#' 
#' @param sub [character] column name of subject ID
#'  e.g., `sub = "Subject"`
#' 
#' @param time_line [vector] A vector specifying the name of the column that 
#'  the sequence of the experiment. This argument defines how the experiment is 
#'  structured, such as whether it is organized by "Block" with breaks in 
#'  between, and multiple trials within each block. 
#'  e.g., `time_line = c("Block", "Trial")`
#' 
#' @param L_choice [character] column name of left choice. 
#'  e.g., `L_choice = "Left_Choice"`
#' 
#' @param R_choice [character] column name of right choice. 
#'  e.g., `R_choice = "Right_Choice"`
#' 
#' @param sub_choose [character] column name of choices made by the subject. 
#'  e.g., `sub_choose = "Choose"`
#' 
#' @param rob_choose [character] column name of choices made by the model. 
#'  e.g., `rob_choose = "Rob_Choose"`
#'  you should ignore this argument
#' 
#' @param L_reward [character] column name of the reward of left choice 
#'  e.g., `L_reward = "Left_reward"`
#' 
#' @param R_reward [character] column name of the reward of right choice 
#'  e.g., `R_reward = "Right_reward"`
#' 
#' @param var1 [character] column name of extra variable 1. If your model uses 
#'  more than just reward and expected value, and you need other information, 
#'  such as whether the choice frame is Gain or Loss, then you can input the 
#'  'Frame' column as var1 into the model.
#'  e.g., `var1 = "Extra_Var1"`
#' 
#' @param var2 [character] column name of extra variable 2. If one additional 
#'  variable, var1, does not meet your needs, you can add another additional 
#'  variable, var2, into your model.
#'  e.g., `var2 = "Extra_Var2"`
#' 
#' @param digits_1 [integer] The number of decimal places to retain for columns related 
#'  to the value function 
#'  The default is 2.
#' 
#' @param digits_2 [integer] The number of decimal places to retain for columns related 
#'  to the select function. 
#'  The default is 5.
#'
#' @returns A list of class \code{binaryRL} containing the 
#'  results of the model fitting.
#'  
#' @export
#'
#' @examples
#' data <- binaryRL::Mason_2024_Exp1
#' 
#' test <- binaryRL::run_m(
#'   data = data,
#'   id = 18,
#'   eta = c(0.321, 0.765),
#'   n_params = 2, 
#'   n_trials = 360
#' )
#' 
#' summary(test)
#' 
run_m <- function(
    data,
    id,
    mode = "fit",
    initial_value = NA,
    softmax = TRUE,
    threshold = 1,
    seed = 123,
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
    L_reward = "L_reward",
    R_reward = "R_reward",
    sub_choose = "Sub_Choose",
    rob_choose = "Rob_Choose",
    raw_cols = NULL,
    var1 = NA,
    var2 = NA,
    
    digits_1 = 2,
    digits_2 = 5
){
  if (is.null(raw_cols)) {
    raw_cols = colnames(data)
  }
  
  # 选择被试
  data <- data[data[[sub]] == id, ]
  
  step1 <- unique_choice(
    data = data,
    L_choice = L_choice, 
    R_choice = R_choice
  )
  
  step2 <- arrange_data(
    data = step1[["data"]],
    time_line = time_line
  )
  
  step3 <- add_NA(
    data = step2
  )
  
  step4 <- set_initial_value(
    data = step3, 
    options = step1[["options"]], 
    initial_value = initial_value
  )
  
  step5 <- decision_making(
    data = step4,
    options = step1[["options"]],
    L_choice = "L_choice", R_choice = "R_choice",
    L_reward = "L_reward", R_reward = "R_reward",
    softmax = softmax,
    threshold = threshold,
    initial_value = initial_value,
    
    lambda = lambda,
    gamma = gamma,
    eta = eta,
    epsilon = epsilon,
    tau = tau,

    util_func = util_func,
    rate_func = rate_func,
    expl_func = expl_func,
    prob_func = prob_func
  )
  
  step6 <- model_fit(
    data = step5, 
    L_choice = L_choice, 
    R_choice = R_choice, 
    sub_choose = sub_choose
  )
  
  step7 <- digits(
    data = step6, 
    options = step1[["options"]],
    digits_1 = digits_1, 
    digits_2 = digits_2
  )
  
  step8 <- output(
    data = step7,
    n_params = n_params,
    n_trials = n_trials,
    
    lambda = lambda,
    gamma = gamma,
    eta = eta,
    epsilon = epsilon,
    tau = tau
  )
  
  step9 <- mode(
    data = step8,
    mode = mode,
    sub_choose = sub_choose,
    rob_choose = rob_choose,
    raw_cols = raw_cols
  )
  
  final <- step9
  
  return(final)
}
