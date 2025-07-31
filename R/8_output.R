#' Summary the Results
#' 
#' @param name [character] 
#' The name of your RL model
#' 
#' @param data [data.frame] 
#' A data frame resulting from the 'step7' process of the `digits` function. 
#' 
#' @param n_params [integer] 
#' The number of free parameters in your model. 
#' 
#' @param n_trials [integer] 
#' The total number of trials in your experiment.
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
#'  \code{threshold} should be kept at its default value.
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
#' This parameter represents the exponent in utility functions, 
#'  \code{util_func}, specifically:
#'  \itemize{
#'    \item \strong{Stevens' Power Law}:
#'    Utility is modeled as:
#'    \deqn{U(R) = {R}^{\gamma}}
#'
#'    \item \strong{Kahneman's Prospect Theory}:
#'    This exponent is applied differently based on the sign of the reward:
#'    \deqn{U(R) = \begin{cases}
#'      R^{\gamma_{1}}, & R > 0 \\
#'      \beta \cdot R^{\gamma_{2}}, & R < 0
#'    \end{cases}}
#'  }
#'  
#' @param eta [vector]
#' Parameters used in the Learning Rate Function, 
#'  \code{rate_func}, 
#'  representing the rate at which the subject updates the difference 
#'  (prediction error) between the reward and the expected value in the 
#'  subject's mind.
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
#' A parameter used in the \strong{epsilon-greedy} exploration strategy. It 
#'  defines the probability of making a completely random choice, as opposed 
#'  to choosing based on the relative values of the left and right options. 
#'  For example, if \code{epsilon = 0.1}, the subject has a 10% chance of random 
#'  choice and a 90% chance of value-based choice. This parameter is only 
#'  relevant when \code{threshold} is at its default value (1) and 
#'  \code{lambda} is not set.
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
#'  in the \strong{epsilon-decreasing} strategy. A higher \code{lambda} value
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
#' @param pi [vector]
#' Parameter used in the Upper-Confidence-Bound (UCB) action selection
#'  formula. \code{bias_func} controls the degree of 
#'  exploration by scaling the uncertainty bonus given to less-explored options. 
#'  A larger value of \code{pi} (denoted as \code{c} in Sutton and Barto(1998) ) 
#'  increases the influence of this bonus, leading to more exploration of 
#'  actions with uncertain estimated values. Conversely, a smaller \code{pi} 
#'  results in less exploration.
#'
#' \deqn{
#'   A_t = \arg \max_{a} \left[ V_t(a) + \pi \sqrt{\frac{\ln(t)}{N_t(a)}} \right]
#' }
#' 
#' \code{default: pi = NA}
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
#' @param priors [list] A \code{list} object for specifying the Bayesian prior
#'   distributions for each model parameter. Each element in the list
#'   should be named after a parameter and contain a function that returns the 
#'   log probability density.
#'   
#'   By default, a set of priors is used. For most parameters, this is a
#'   \strong{Uniform(0, 1)} distribution, which acts as an uninformative prior.
#'   This means that for these parameters, the Maximum A Posteriori (MAP)
#'   estimate will be identical to the Maximum Likelihood Estimate (MLE).
#'
#'   The exception is for the inverse temperature parameter associated with the
#'   softmax function (\code{tau}). This parameter uses an 
#'   \strong{Exponential(1)} distribution as its prior. This is a weakly
#'   informative prior that regularizes the model by favoring smaller,
#'   positive values, thus preventing extremely large parameter estimates.
#'   
#' @returns binaryRL[list]:
#'   \itemize{
#'     \item{\code{data}: output data frame with all information}
#'     \item{\code{params}: all parameters value}
#'     \item{\code{numeric}: ACC}
#'     \item{\code{numeric}: LogL}
#'     \item{\code{numeric}: AIC}
#'     \item{\code{numeric}: BIC}
#'   }
#'   
#' @noRd
#' 
output <- function(
    name = NA,
    data, 
    n_params, n_trials, 
    initial_value, threshold,
    alpha, beta, gamma, eta, epsilon, lambda, pi, tau,
    priors
){
  params <- list(
    Q1 = initial_value,
    threshold = threshold,
    
    alpha = c(alpha),
    beta = c(beta),
    gamma = c(gamma),
    eta = c(eta), 
    epsilon = c(epsilon),
    lambda = c(lambda),
    pi = c(pi),
    tau = c(tau)
  )
  
  # 因为第一行用来填写初始值了, 所以需要重新把第二行初始化成第一行
  rownames(data) <- NULL 
  mean_ACC <- round(mean(data$ACC), digits = 4) * 100
  
  # Log-Likelihood
  sum_logLi <- round(sum(data$L_logl) + sum(data$R_logl), digits = 2)
  
  # 找到priors定义了几个先验概率
  priors_name <- unique(names(priors))
  
  # 存储自由参数的数值
  params_value <- c()
  
  # 定义了先验概率的才是自由参数
  for (param_name in priors_name) {
    # 把带入run_m的自由参数存在params_value中
    params_value <- c(params_value, params[[param_name]])
  }
  
  # 初始化Log Prior Probability
  logPr <- c()
  
  for (i in 1:length(priors)) {
    # 使用先验分布概率, 求解该参数对应概率密度
    logPr[i] <- priors[[i]](params_value[i])
  }
  
  # 求和每个参数对应的log先验概率密度
  sum_logPr <- sum(logPr)
  
  # Log-Posterior Probability
  sum_logPo <- sum_logLi + sum_logPr
  
  AIC <- round(2 * n_params - 2 * sum_logLi, digits = 2)
  BIC <- round(n_params * log(n_trials) - 2 * sum_logLi, digits = 2)
  
  res <- list(
    data = data,
    params = params,
    name = name,
    acc = mean_ACC,
    ll = sum_logLi,
    lpr = sum_logPr,
    lpo = sum_logPo,
    aic = AIC,
    bic = BIC
  )
  
  class(res) <- c("binaryRL")
  
  return(res)
}
