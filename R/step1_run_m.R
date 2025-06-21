#' @title 
#' Step 1: Building reinforcement learning model
#'
#' @description
#' This function is designed to construct and customize reinforcement
#' learning models.
#'
#' Items for model construction:
#' \itemize{
#'   \item \strong{Data Input and Specification:} You must provide the raw
#'     dataset for analysis. Crucially, you need to inform the \code{run_m}
#'     function about the corresponding column names within your dataset
#'     (e.g., subject IDs, choices, and outcomes).
#'
#'   \item \strong{Customizable RL Models:} This function allows you
#'     to define and adjust the number of free parameters to create
#'     various reinforcement learning models.
#'     \itemize{
#'       \item \emph{Basic Models:} By adjusting \code{eta} and \code{gamma}, 
#'         you can construct core types such as TD, RSTD, or Utility models.
#'       \item \emph{Exploration Strategies:} By modifying \code{threshold}, 
#'         \code{epsilon}, and \code{lambda}, you can implement strategies 
#'         including epsilon-first, greedy, or epsilon-decreasing exploration.
#'       \item \emph{Initial Value:} You can treat 
#'         \code{initial_value} as a free parameter. As discussed in 
#'         \emph{Palminteri and Stefano (2022)} 
#'         \doi{10.1016/j.tics.2022.04.005}, agents’ prior expectations about 
#'         options can affect the learning rates derived by the model.
#'     }
#'
#'   \item \strong{Objective Function Format for Optimization:} Once your
#'     model is defined in \code{run_m}, it must be structured as an objective
#'     function that accepts \code{params} as input and returns a loss value 
#'     (typically \code{logL}). This format ensures compatibility with the 
#'     \pkg{algorithm} package, which uses it to estimate optimal parameters. 
#'     For an example of a standard objective function format, see 
#'     \code{\link[binaryRL]{TD}}, 
#'     \code{\link[binaryRL]{RSTD}}, 
#'     \code{\link[binaryRL]{Utility}}.
#' }
#'
#' For more information, please refer to the GitHub repository:  
#' \url{https://github.com/yuki-961004/binaryRL}
#' 
#' @param mode [character]
#' This parameter controls the function's operational mode. It has three
#'  possible values, each typically associated with a specific function:
#'  \itemize{
#'    \item \code{"simulate"}: Should be used when working with \code{rcv_d}.
#'    \item \code{"fit"}: Should be used when working with \code{fit_p}.
#'    \item \code{"replay"}: Should be used when working with \code{rpl_e}.
#'  }
#'  In most cases, you won't need to modify this parameter directly, as suitable
#'  default values are set for different contexts.
#' 
#' @param data [data.frame] 
#' This data should include the following mandatory columns: 
#'  \itemize{
#'    \item "sub"
#'    \item "time_line" (e.g., "Block", "Trial")
#'    \item "L_choice"
#'    \item "R_choice"
#'    \item "L_reward"
#'    \item "R_reward"
#'    \item "sub_choose"
#'  }
#' 
#' @param id [integer] 
#' Which subject is going to be analyzed. The value should correspond to an 
#'  entry in the "sub" column, which must contain the subject IDs. 
#'  
#'  \code{e.g., id = 18}
#'  
#' @param n_params [integer] 
#' The number of free parameters in your model. 
#' 
#' @param n_trials [integer] 
#' The total number of trials in your experiment.
#' 
#' @param softmax [logical]
#'  Whether to use the softmax function.
#'    \itemize{
#'      \item \strong{\code{TRUE}}: The value of each option directly influences
#'       the probability of selecting that option. Higher values lead to a
#'       higher probability of selection.
#'      \item \strong{\code{FALSE}}: The subject will always choose the option
#'       with the higher value. There is no possibility of selecting the
#'       lower-value option.
#'  }
#'  
#'  \code{default: softmax = TRUE}
#' 
#' @param seed [integer] 
#' Random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  
#'  \code{default: seed = 123}
#' 
#' @param initial_value [numeric] 
#' Subject's initial expected value for each stimulus's reward. If this value 
#'  is not set \code{initial_value = NA}, the subject will use the reward received 
#'  after the first trial as the initial value for that stimulus. In other 
#'  words, the learning rate for the first trial is 100%. 
#'  
#'  \code{default: initial_value = NA}
#'  
#' @param threshold [integer]
#' Controls the initial exploration phase in the \strong{epsilon-first} strategy.
#'  This is the number of early trials where the subject makes purely random
#'  choices, as they haven't yet learned the options' values. For example,
#'  \code{threshold = 20} means random choices for the first 20 trials.
#'  For \strong{epsilon-greedy} or \strong{epsilon-decreasing} strategies,
#'  `threshold` should be kept at its default value.
#'  
#'  \deqn{P(x) = \begin{cases}
#'    \text{trial} \le \text{threshold}, & x=1 \text{ (random choosing)} \\
#'    \text{trial} > \text{threshold}, & x=0 \text{ (value-based choosing)}
#'  \end{cases}}
#'  
#'  \code{default: threshold = 1}
#'  
#'  \code{epsilon-first: threshold = 20, epsilon = NA, lambda = NA}
#' 
#' @param alpha [vector]
#' Extra parameters that may be used in functions. 
#'
#' @param beta [vector]
#' Extra parameters that may be used in functions. 
#' 
#' @param gamma [vector]
#' This parameter represents the exponent in utility functions, specifically:
#'  \itemize{
#'    \item \strong{Stevens' Power Law}:
#'    Utility is modeled as:
#'    \deqn{U = {R}^{\gamma}}
#'
#'    \item \strong{Kahneman's Prospect Theory}:
#'    This exponent is applied differently based on the sign of the reward:
#'    \deqn{U = \begin{cases}
#'      R^{\gamma_{1}}, & R > 0 \\
#'      \beta \cdot R^{\gamma_{2}}, & R < 0
#'    \end{cases}}
#'  }
#'  
#' @param eta [numeric]
#' Parameters used in the Learning Rate Function, \code{rate_func}, representing
#'  the rate at which the subject updates the difference (prediction error)
#'  between the reward and the expected value in the subject's mind.
#'
#'  The structure of \code{eta} depends on the model type:
#'  \itemize{
#'    \item For the \strong{Temporal Difference (TD) model}, 
#'    where a single learning rate is used throughout the experiment 
#'    \deqn{V_{new} = V_{old} + \eta \cdot (R - V_{old})}
#'    
#'    \item For the \strong{Risk-Sensitive Temporal Difference (RDTD) model},
#'    where two different learning rates are used depending on whether the 
#'    reward is lower or higher than the expected value:
#'    \deqn{V_{new} = V_{old} + \eta_{+} \cdot (R - V_{old}), R > V_{old}}
#'    \deqn{V_{new} = V_{old} + \eta_{-} \cdot (R - V_{old}), R < V_{old}}
#'  }
#'  
#'  \code{TD: eta = 0.3}
#'  
#'  \code{RSTD: eta = c(0.3, 0.7)}
#'
#' @param epsilon [numeric]
#' A parameter used in the \strong{epsilon-greedy} exploration strategy. It defines
#'  the probability of making a completely random choice, as opposed to choosing
#'  based on the relative values of the left and right options. For example,
#'  if `epsilon = 0.1`, the subject has a 10% chance of random choice and a
#'  90% chance of value-based choice. This parameter is only relevant when
#'  `threshold` is at its default value (1) and `lambda` is not set.
#'  
#'  \deqn{P(x) = \begin{cases}
#'    \epsilon, & x=1 \text{ (random choosing)} \\
#'    1-\epsilon, & x=0 \text{ (value-based choosing)}
#'  \end{cases}}
#' 
#'  \code{epsilon-greedy: threshold = 1, epsilon = 0.1, lambda = NA}
#' 
#' @param lambda [vector] 
#' A numeric value that controls the decay rate of exploration probability
#'  in the \strong{epsilon-decreasing} strategy. A higher `lambda` value
#'  means the probability of random choice will decrease more rapidly
#'  as the number of trials increases.
#'  
#'  \deqn{P(x) = \begin{cases}
#'    \frac{1}{1+\lambda \cdot trial}, & x=1 \text{ (random choosing)} \\
#'    \frac{\lambda \cdot trial}{1+\lambda \cdot trial}, & x=0 \text{ (value-based choosing)}
#'  \end{cases}}
#'  
#'  \code{epsilon-decreasing threshold = 1, epsilon = NA, lambda = 0.5}
#' 
#' @param tau [vector] 
#' Parameters used in the Soft-Max Function. `prob_func` representing the 
#'  sensitivity of the subject to the value difference when making decisions. 
#'  It determines the probability of selecting the left option versus the right 
#'  option based on their values. A larger value of tau indicates greater 
#'  sensitivity to the value difference between the options. In other words, 
#'  even a small difference in value will make the subject more likely to 
#'  choose the higher-value option. 
#'  
#'  \deqn{P_L = \frac{1}{1+e^{-(V_L-V_R) \cdot \tau}}; P_R = \frac{1}{1+e^{-(V_R-V_L) \cdot \tau}}} 
#' 
#'  \code{e.g., tau = c(0.5)}
#' 
#' @param util_func [function] Utility Function see \code{\link[binaryRL]{func_gamma}}.
#' 
#' @param rate_func [function] Learning Rate Function see \code{\link[binaryRL]{func_eta}}.
#' 
#' @param expl_func [function] Exploration Strategy Function see \code{\link[binaryRL]{func_epsilon}}.
#' 
#' @param prob_func [function] Soft-Max Function see \code{\link[binaryRL]{func_tau}}.
#' 
#' @param sub [character] column name of subject ID
#' 
#'  \code{e.g., sub = "Subject"}
#' 
#' @param time_line [vector] 
#' A vector specifying the name of the column that the sequence of the 
#'  experiment. This argument defines how the experiment is structured, 
#'  such as whether it is organized by "Block" with breaks in between, and 
#'  multiple trials within each block. 
#'  
#' \code{default: time_line = c("Block", "Trial")}
#' 
#' @param L_choice [character] 
#' Column name of left choice. 
#' 
#'  \code{default: L_choice = "Left_Choice"}
#' 
#' @param R_choice [character] 
#' Column name of right choice. 
#' 
#'  \code{default: R_choice = "Right_Choice"}
#'  
#' @param L_reward [character] 
#' Column name of the reward of left choice 
#' 
#'  \code{default: L_reward = "Left_reward"}
#' 
#' @param R_reward [character] 
#' Column name of the reward of right choice 
#' 
#'  \code{default: R_reward = "Right_reward"}
#'  
#' @param sub_choose [character] 
#' Column name of choices made by the subject. 
#' 
#'  \code{default: sub_choose = "Choose"}
#' 
#' @param rob_choose [character] 
#' Column name of choices made by the model, which you could ignore. 
#' 
#'  \code{default: rob_choose = "Rob_Choose"}
#'  
#' @param raw_cols [vector] 
#' Defaults to `NULL`. If left as `NULL`, it will directly capture all column 
#'  names from the raw data.
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
#' @param digits_1 [integer] 
#' The number of decimal places to retain for columns related to value function 
#'  
#'  \code{default: digits_1 = 2}
#' 
#' @param digits_2 [integer] 
#' The number of decimal places to retain for columns related to select function. 
#'  
#'  \code{default: digits_2 = 5}
#'
#' @returns 
#' A list of class \code{binaryRL} containing the results of the model fitting.
#'  
#' @examples
#' data <- binaryRL::Mason_2024_Exp1
#' 
#' binaryRL.res <- binaryRL::run_m(
#'   mode = "simulate",
#'   data = data,
#'   id = 18,
#'   eta = c(0.321, 0.765),
#'   n_params = 2, 
#'   n_trials = 360
#' )
#' 
#' summary(binaryRL.res)
#' 
run_m <- function(
  mode = c("simulate", "fit", "replay"),
  
  data,
  id,
  n_params,
  n_trials,
  
  softmax = TRUE,
  seed = 123,

  initial_value = NA,
  threshold = 1,
  
  alpha = NA,
  beta = NA,
  gamma = 1,
  eta,
  epsilon = NA,
  lambda = NA,
  tau = 1,
  
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
    L_choice = L_choice, R_choice = R_choice,
    L_reward = L_reward, R_reward = R_reward,
    softmax = softmax,
    
    threshold = threshold,
    initial_value = initial_value,
    
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    eta = eta,
    epsilon = epsilon,
    lambda = lambda,
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
    initial_value = initial_value,
    threshold = threshold,
    
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    eta = eta,
    epsilon = epsilon,
    lambda = lambda,
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
