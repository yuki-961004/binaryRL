#' Markov Decision Process
#'
#' @param data [data.frame] A data frame resulting from the 'step4' process of the `set_initial_value` function. 
#' 
#' @param options [vector] all alternative options from 'step1' `unique_choice`
#' 
#' @param L_choice [character] column name of left choice. 
#'  e.g., `L_choice = "Left_Choice"`
#' 
#' @param R_choice [character] column name of right choice. 
#'  e.g., `R_choice = "Right_Choice"`
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
#' @param softmax [logical] whether to use the softmax function. 
#'  When softmax = TRUE, the value of each option influences the probability 
#'  of selecting that option. Higher values increase the probability of 
#'  selecting that option. When softmax = FALSE, the subject will always 
#'  choose the option with the higher value, with no possibility of selecting 
#'  the lower-value option. default: `softmax = TRUE`
#' 
#' @param seed [integer] random seed. This ensures that the results are 
#'  reproducible and remain the same each time the function is run. 
#'  default: `seed = 123` 
#' 
#' @param initial_value [numeric] subject's initial expected value for each 
#'  stimulus's reward. If this value is not set (`initial_value = NA`), 
#'  the subject will use the reward received after the first trial as the 
#'  initial value for that stimulus. In other words, the learning rate for the 
#'  first trial is 100%. default: `initial_value = NA` 
#'  e.g., `initial_value = 0`
#' 
#' @param threshold [integer] the number of initial trials during which the 
#'  subject makes random choices rather than choosing based on the values of 
#'  the options. This occurs because the subject has not yet learned the values 
#'  of the options. For example, threshold = 20 means the subject will make 
#'  completely random choices for the first 20 trials. default: `threshold = 1`
#' 
#' @param lambda [vector] Extra parameters that may be used in functions. 
#'  e.g., `lambda = c(0.4, 0.7, 20, 60)`
#' 
#' @param gamma [vector] Parameters used in the `util_func` (Utility Function), 
#'  often referred to as the discount rate. For example, 
#'  `utility = gamma * reward`, if gamma < 1, it indicates that people 
#'  tend to discount the objective reward. Provide the value as a vector 
#'  e.g., `gamma = c(0.7)`
#' 
#' @param eta [vector] Parameters used in the `rate_func` (Learning Rate Function), 
#'  representing the rate at which the subject updates the 
#'  difference (prediction error) between the reward and the expected value 
#'  in the subject's mind. In the TD model, there is a single learning rate 
#'  throughout the experiment. In the RSTD model, two different learning rates 
#'  are used when the reward is higher or lower than the expected value.
#'  e.g., `eta = c(0.3, 0.7)`
#' 
#' @param epsilon [vector] Parameters used in the `expl_func` (Exploration Function), 
#'  determining whether the subject makes decisions based on the relative values 
#'  of the left and right options, or chooses completely randomly. For example, 
#'  when epsilon = 0.1, it means the subject has a 10% chance of making a 
#'  completely random choice and a 90% chance of choosing based on the values 
#'  of the options.
#'  e.g., `epsilon = c(0.1)`
#' 
#' @param tau [vector] Parameters used in the `prob_func` (Soft-Max Function), 
#'  representing the sensitivity of the subject to the value difference when 
#'  making decisions. It determines the probability of selecting the left option 
#'  versus the right option based on their values. A larger value of tau 
#'  indicates greater sensitivity to the value difference between the options. 
#'  In other words, even a small difference in value will make the subject more 
#'  likely to choose the higher-value option. 
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
#' @returns data frame:
#'   \itemize{
#'     \item{\code{data}: step4 + all decisions.}
#'   }
#' @export
#'

decision_making <- function(
    data, 
    options,
    
    L_choice = "L_choice", R_choice = "R_choice",
    L_reward = "L_reward", R_reward = "R_reward", 
    var1 = NA, var2 = NA,
    
    seed = 123, initial_value,
    softmax = TRUE, threshold = 1,
    
    gamma, eta, epsilon, tau, lambda, 
    
    expl_func = func_epsilon,
    prob_func = func_tau,
    util_func = func_gamma,
    rate_func = func_eta
){
########################### [update row by row] ################################  
  # 逐行更新Value
  for (i in 2:nrow(data)) {
    
    # 记录此时L和R的名字
    L_name <- data[[L_choice]][i]
    R_name <- data[[R_choice]][i]
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
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      threshold = threshold,
      epsilon = epsilon,
      lambda = lambda
    )
    
    # [ 1+ CHOOSE ]  
    
    # 如果选项都不是第一次出现, 则正常计算概率
    if ((data[[L_choice]][i] %in% chosen) & (data[[R_choice]][i] %in% chosen)) {
      # 基于prob函数计算选择左边和右边的概率
      # 如果选项都不是第一次出现, 则正常计算概率
      
      # prob_func -> data$L_prob[i] 计算选L概率
      data$L_prob[i] <- prob_func(
        L_value = data$L_value[i],
        R_value = data$R_value[i],
        try = data$Try[i],
        var1 = data[[var1]][i],
        var2 = data[[var2]][i],
        LR = "L", 
        tau = tau,
        lambda = lambda
      )
      # prob_func -> data$R_prob[i] 计算选R概率
      data$R_prob[i] <- prob_func(
        L_value = data$L_value[i],
        R_value = data$R_value[i],
        try = data$Try[i],
        var1 = data[[var1]][i],
        var2 = data[[var2]][i],
        LR = "R", 
        tau = tau,
        lambda = lambda
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
    data$Time_Line[[i]] <- sum(
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
      value = data$V_value[i],
      utility = data$R_utility[i],
      reward = data$Reward[i],
      occurrence = data$Time_Line[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      gamma = gamma,
      lambda = lambda
    )
    data$gamma[i] <- as.numeric(gamma_utility[[1]])
    data$R_utility[i] <- as.numeric(gamma_utility[[2]])
    
    # 看到reward之后的学习率, 用rate_func选择此时对应的eta
    data$eta[i] <- rate_func(
      value = data$V_value[i],
      utility = data$R_utility[i],
      reward = data$Reward[i],
      occurrence = data$Time_Line[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      eta = eta,
      lambda = lambda
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