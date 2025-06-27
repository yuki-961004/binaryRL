#' Markov Decision Process
#'
#' @param data [data.frame] A data frame resulting from the 'step4' process of the `set_initial_value` function. 
#' 
#' @param options [vector] all alternative options from 'step1' `unique_choice`
#' 
#' @param initial_value [numeric] 
#' Subject's initial expected value for each stimulus's reward. If this value 
#'  is not set (`initial_value = NA`), the subject will use the reward received 
#'  after the first trial as the initial value for that stimulus. In other 
#'  words, the learning rate for the first trial is 100%. 
#'  \code{default: `initial_value = NA` e.g., `initial_value = 0`}
#'  
#' @param threshold [integer]
#' Controls the initial exploration phase in the \strong{epsilon-first} strategy.
#'  This is the number of early trials where the subject makes purely random
#'  choices, as they haven't yet learned the options' values. For example,
#'  `threshold = 20` means random choices for the first 20 trials.
#'  For \strong{epsilon-greedy} or \strong{epsilon-decreasing} strategies,
#'  `threshold` should be kept at its default value.
#'  \code{Default: `threshold = 1`}
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
#'  \code{Default: `softmax = TRUE`}
#' 
#' @param seed [integer] 
#' Random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  \code{Default: `seed = 123`}
#' 
#' @param alpha [vector]
#' Extra parameters that may be used in functions. 
#'
#' @param beta [vector]
#' Extra parameters that may be used in functions. 
#' 
#' @param gamma [vector]
#' This parameter represents the exponent in \bold{Stevens' Power Law} within the
#'  Utility Function, where utility is modeled as 
#'  \eqn{\mathrm{u(x)} = \mathrm{x}^{\gamma_1}}.
#'
#' In \bold{Kahneman's Prospect Theory}, this exponent is applied differently:
#' \itemize{
#'   \item \eqn{\mathrm{utility} = \mathrm{reward}^{\gamma_{1}}}, \eqn{reward > 0}.
#'   \item \eqn{\mathrm{utility} = \beta \times \mathrm{reward}^{\gamma_{2}}}, \eqn{reward < 0}.
#' }
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
#'      \eqn{\eta, & |p| = 1 \text{ (TD)}}
#'    \item For the \strong{Risk-Sensitive Temporal Difference (RDTD) model},
#'    where two different learning rates are used depending on whether the 
#'    reward is lower or higher than the expected value:
#'      \eqn{\eta_-; \eta_+, & |p| = 2 \text{ (RDTD)}}.
#'  }
#'  e.g., \code{eta = 0.3} for TD, or \code{eta = c(0.3, 0.7)} for RDTD.
#'
#' @param epsilon [numeric]
#' A parameter used in the \strong{epsilon-greedy} exploration strategy. It defines
#'  the probability of making a completely random choice, as opposed to choosing
#'  based on the relative values of the left and right options. For example,
#'  if `epsilon = 0.1`, the subject has a 10% chance of random choice and a
#'  90% chance of value-based choice. This parameter is only relevant when
#'  `threshold` is at its default value (1) and `lambda` is not set.
#'  \code{e.g., `epsilon = 0.1`}
#' 
#' @param lambda [vector] 
#' A numeric value that controls the decay rate of exploration probability
#'  in the \strong{epsilon-decreasing} strategy. A higher `lambda` value
#'  means the probability of random choice will decrease more rapidly
#'  as the number of trials increases.
#' 
#' @param tau [vector] 
#' Parameters used in the Soft-Max Function. `prob_func` representing the 
#'  sensitivity of the subject to the value difference when making decisions. 
#'  It determines the probability of selecting the left option versus the right 
#'  option based on their values. A larger value of tau indicates greater 
#'  sensitivity to the value difference between the options. In other words, 
#'  even a small difference in value will make the subject more likely to 
#'  choose the higher-value option. 
#'  \code{e.g., `tau = c(0.5)`}
#' 
#' @param util_func [function] Utility Function.
#' 
#' @param rate_func [function] Learning Rate Function.
#' 
#' @param expl_func [function] Exploration Function.
#' 
#' @param prob_func [function] Soft-Max Function.
#' 
#' @param L_choice [character] 
#' Column name of left choice. 
#'  \code{e.g., `L_choice = "Left_Choice"`}
#' 
#' @param R_choice [character] 
#' Column name of right choice. 
#'  \code{e.g., `R_choice = "Right_Choice"`}
#'  
#' @param L_reward [character] 
#' Column name of the reward of left choice 
#'  \code{e.g., `L_reward = "Left_reward"`}
#' 
#' @param R_reward [character] 
#' Column name of the reward of right choice 
#'  \code{e.g., `R_reward = "Right_reward"`}
#'  
#' @param var1 [character] 
#' Column name of extra variable 1. If your model uses more than just reward 
#'  and expected value, and you need other information, such as whether the 
#'  choice frame is Gain or Loss, then you can input the 'Frame' column as 
#'  var1 into the model.
#'  \code{e.g., `var1 = "Extra_Var1"`}
#' 
#' @param var2 [character] 
#' Column name of extra variable 2. If one additional variable, var1, does not 
#'  meet your needs, you can add another additional variable, var2, into your 
#'  model.
#'  e.g., `var2 = "Extra_Var2"`
#'
#' @returns data frame:
#'   \itemize{
#'     \item{\code{data}: step4 + all decisions.}
#'   }
#'   
#' @noRd
#' 

decision_making <- function(
    data, 
    options,
    
    seed = 123, initial_value,
    softmax = TRUE, threshold = 1,
    
    alpha, beta, gamma, eta, epsilon, lambda, tau, 
    
    expl_func = func_epsilon,
    prob_func = func_tau,
    util_func = func_gamma,
    rate_func = func_eta,
    
    L_choice = "L_choice", R_choice = "R_choice",
    L_reward = "L_reward", R_reward = "R_reward", 
    var1 = NA, var2 = NA
){
########################### [update row by row] ################################  
  # 逐行更新Value
  for (i in 2:nrow(data)) {
    
    # 记录此时L和R的名字
    L_name <- data[[L_choice]][i]
    R_name <- data[[R_choice]][i]
    
    # 查询此时左选项已经出现过几次了
    data$L_freq[i] <- 
      sum(data[[L_choice]][1:(i)] == L_name, na.rm = TRUE) + 
      sum(data[[R_choice]][1:(i)] == L_name, na.rm = TRUE)
    # 计算此时右选项已经出现过几次了
    data$R_freq[i] <- 
      sum(data[[L_choice]][1:(i)] == R_name, na.rm = TRUE) + 
      sum(data[[R_choice]][1:(i)] == R_name, na.rm = TRUE)
    
    # 计算此时左选项被选了几次
    data$L_pick[i] <- sum(data$Rob_Choose == L_name, na.rm = TRUE)
    # 计算此时右选项被选了几次
    data$R_pick[i] <- sum(data$Rob_Choose == R_name, na.rm = TRUE)
    
    # 在上一行找此时左右选项对应的心中的价值
    data$L_value[i] <- data[[L_name]][i - 1]
    data$R_value[i] <- data[[R_name]][i - 1]
################################ [L & R prob] ##################################  
    
    # 查询此次选择时, 已经选过哪些了
    chosen <- unique(data$Rob_Choose)
    
    # 设置随机种子
    set.seed(seed = seed + i)
    
    # expl_func -> data$Try[i]: 是否进行探索
    data$Try[i] <- expl_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      
      threshold = threshold,
      epsilon = epsilon,
      lambda = lambda,
      alpha = alpha,
      beta = beta
    )
    
    # [ 1+ CHOOSE ]  
    
    # 如果选项都不是第一次出现, 则正常计算概率
    if ((data[[L_choice]][i] %in% chosen) & (data[[R_choice]][i] %in% chosen)) {
      # 基于prob函数计算选择左边和右边的概率
      # 如果选项都不是第一次出现, 则正常计算概率
      
      # prob_func -> data$L_prob[i] 计算选L概率
      data$L_prob[i] <- prob_func(
        i = i,
        L_freq = data$L_freq[i],
        R_freq = data$R_freq[i],
        L_pick = data$L_pick[i],
        R_pick = data$R_pick[i],
        L_value = data$L_value[i],
        R_value = data$R_value[i],
        var1 = data[[var1]][i],
        var2 = data[[var2]][i],
        
        try = data$Try[i],
        LR = "L",
        
        tau = tau,
        alpha = alpha,
        beta = beta
      )
      # prob_func -> data$R_prob[i] 计算选R概率
      data$R_prob[i] <- prob_func(
        i = i,
        L_freq = data$L_freq[i],
        R_freq = data$R_freq[i],
        L_pick = data$L_pick[i],
        R_pick = data$R_pick[i],
        L_value = data$L_value[i],
        R_value = data$R_value[i],
        var1 = data[[var1]][i],
        var2 = data[[var2]][i],
        
        try = data$Try[i],
        LR = "R",
        
        tau = tau,
        alpha = alpha,
        beta = beta
      )
      # [ 1st CHOOSE ] #
    } else if (!(data[[L_choice]][i] %in% chosen) & (data[[R_choice]][i] %in% chosen)) {
      # 如果左边选项是第一次出现, 则一定选左边  
      data$L_prob[i] <- 1
      data$R_prob[i] <- 0
    } else if ((data[[L_choice]][i] %in% chosen) & !(data[[R_choice]][i] %in% chosen)) {
      # 如果右边选项是第一次出现, 则一定选右边
      data$L_prob[i] <- 0
      data$R_prob[i] <- 1
    } else if (!(data[[L_choice]][i] %in% chosen) & !(data[[R_choice]][i] %in% chosen)) {
      # 如果都是第一次出现, 则随便选
      data$L_prob[i] <- 0.5
      data$R_prob[i] <- 0.5
    }
    
############################### [ PASS VALUE ] #################################  
    
    # 去上一行找每个选项此时的value
    for (name in options) {
      data[[name]][i] <- data[[name]][i - 1]
    }
    
################################ [ Soft-Max ] ##################################    
    
    # 检查是否设定了softmax
    if (!(softmax %in% c(TRUE, FALSE))) {
      stop("softmax TRUE or FALSE?")
      # 如果是softmax = TRUE就基于概率随机选
    } else if (softmax == TRUE) {
      if (!is.numeric(data$L_value[i]) | !is.numeric(data$R_value[i])) {
        stop("An error occurs when softmax == FALSE")
      }

      # 基于刚刚的概率, 随机选一个. 而不是谁大选谁
      data$Rob_Choose[i] <- sample(
        c(data[[L_choice]][i], data[[R_choice]][i]), 
        prob = c(data$L_prob[i], data$R_prob[i]),
        size = 1
      ) 
      # 如果softmax = FALSE, 则按照谁大选谁
    } else if (softmax == FALSE) {
      if (!is.numeric(data$L_value[i]) | !is.numeric(data$R_value[i])) {
        stop("An error occurs when softmax == FALSE")
      } else if (data$L_value[i] > data$R_value[i]) {
        # 如果左边大选左边
        data$Rob_Choose[i] <- data[[L_choice]][i]
      } else if (data$L_value[i] < data$R_value[i]) {
        # 如果左边小于右边
        data$Rob_Choose[i] <- data[[R_choice]][i]
      } else if (data$L_value[i] == data$R_value[i]) {
        # 一样大随机选一个(是Single情况), 或者极端情况[Value_L == Value_R]
        data$Rob_Choose[i] <- sample(
          c(data[[L_choice]][i], data[[R_choice]][i]), 
          size = 1
        )
      } 
    }
    
################################ [occurrence] ##################################   
    
    # 计算这次是第几次选了这个选项
    data$Occurrence[[i]] <- sum(
      data$Rob_Choose == data$Rob_Choose[[i]], 
      na.rm = TRUE
    )
    
################################## [ Reward ] ##################################    
    
    # 基于选择, 来给予奖励
    if (data$Rob_Choose[i] == data[[L_choice]][i]){
      # 选了左边, 给左的奖励
      data$Reward[i] <- data[[L_reward]][i]
    } else if (data$Rob_Choose[i] == data[[R_choice]][i]) {
      # 选了右边, 给右的奖励
      data$Reward[i] <- data[[R_reward]][i]
    }
    
################################ [ update_v ] ##################################     
    
    # 记录这次选了哪个
    choose <- data$Rob_Choose[i]
    # 看到奖励前, 对该选项预期的奖励, 去上一行找
    data$V_value[i] <- data[[choose]][i - 1]
    
    # 看到reward之后的折扣率, 用util_func选择此时对应的gamma, 计算出R_utility
    gamma_utility <- util_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      
      value = data$V_value[i],
      utility = data$R_utility[i],
      reward = data$Reward[i],
      occurrence = data$Occurrence[i],
      
      gamma = gamma,
      alpha = alpha,
      beta = beta
    )
    data$gamma[i] <- as.numeric(gamma_utility[[1]])
    data$R_utility[i] <- as.numeric(gamma_utility[[2]])
    
    # 看到reward之后的学习率, 用rate_func选择此时对应的eta
    data$eta[i] <- rate_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      
      value = data$V_value[i],
      utility = data$R_utility[i],
      reward = data$Reward[i],
      occurrence = data$Time_Line[i],
      
      eta = eta,
      alpha = alpha,
      beta = beta
    )
############################ [1st Learning Rate] ############################### 
    
    # 如果没有设置初始值, 且是第一次选这个选项, 则此次学习率为1
    if (is.na(initial_value) & !(choose %in% chosen)) {
      data$eta[i] <- 1
      data$V_update[i] <- data$V_value[i] + 
        data$eta[i] * (data$R_utility[i] - data$V_value[i])
      data[[choose]][i] <- data$V_update[i]
      # 其余情况, 正常按照eta更新价值
    } else {
      data$V_update[i] <- data$V_value[i] + 
        data$eta[i] * (data$R_utility[i] - data$V_value[i])
      data[[choose]][i] <- data$V_update[i]  
    }
  }
  
  # 删除第一行赋予的初始值
  data <- data[-1, ]
  
  return(data)
}