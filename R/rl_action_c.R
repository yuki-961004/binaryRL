#' rl_action_c
#'
#' @param data A dataset that has been processed through the `update_v` function, 
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
#' ## Parameters and Functions
#' 
#' @param lambda An additional parameter that may be used in these functions. 
#' Provide the value as a vector 
#' e.g., `lambda = c(0.4, 0.7, 20, 60)`
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
#' @param expl_func The function for the epsilon greedy, which you can customize
#' @param prob_func The function for the temperature tau, which you can customize.
#' 
#' ## Column Names
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
#' @param value A column derived from the `update_v` function, representing the updated values.
#'
#' @param var1 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var1 = "Extra_Var1"`
#' 
#' @param var2 A string specifying the name of an additional variable that can be used in the model. 
#' Provide the name of the column as a character string 
#' e.g., `var2 = "Extra_Var2"`
#' 
#' ## Other Arguments
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
#' @param digits digits
#'
#' @return robot choose R or L
#' @export
#'
rl_action_c <- function(
  # update_v数据集
  data,
  # 第0行的值, 只要有就可以
  initial_value = 0,
  # 多少次之前是随机选的
  threshold = 0,

  # 额外参数
  lambda,
  # 多大概率进行探索
  epsilon = NA,
  # softmax的固有参数, 默认为1
  tau = 1,
  # 示例探索函数
  expl_func = func_epsilon,
  # 示例softmax函数
  prob_func = func_tau,

  # 左右选项是什么, 对应的列名
  L_choice = "LC",
  R_choice = "RC",
  # 被试选择列的列名
  choose = "Choose",
  # 被试心中价值列的列名
  value = "V_value",
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,

  # softmax选择时的随机种子
  seed = 123,
  # 是否使用softmax, 还是说value谁大选谁
  softmax = TRUE,

  # 小数位数
  digits = 5
  ################################# [function start] #############################
){
  ################################# [function start] #############################
  # 为了保证choose和value在长转宽中这两列不消失. 所以复制一次
  data$names <- data[[choose]]
  data$values <- data[[value]]
  # 长转宽, 这样每个刺激类型, 就变成了一列
  df_wider <- data %>%
    tidyr::pivot_wider(
      names_from = "names",
      values_from = "values"
    )
  
  # 长转宽前后不同的列, 每个刺激类型, 就是新增的列
  wide_columns1 <- setdiff(x = names(data), y = "ID")
  wide_columns2 <- setdiff(x = names(df_wider), y = "ID")
  choose_col <- setdiff(wide_columns2, wide_columns1)
  
  ############################### [Initialization] ###############################
  # 左右选项对应的价值
  df_wider$L_value <- NA
  df_wider$R_value <- NA
  # 是否随机探索
  df_wider$Try <- NA
  # 基于左右选项价值算出来的选左或右的概率
  df_wider$L_prob <- NA
  df_wider$R_prob <- NA
  # 被试选了什么[刺激]
  df_wider$Sub_Choose <- df_wider$Choose
  # 机器人想选什么[刺激]
  df_wider$Rob_Choose <- NA
  # 被试选的是左还是右
  df_wider$L_dir <- NA
  df_wider$R_dir <- NA
  # 似然值: 机器人选[左/右]的概率 * 被试选了[左/右]
  df_wider$L_logl <- NA
  df_wider$R_logl <- NA
  # 被试选的和机器人选的是否一致
  df_wider$ACC <- NA
  
  ################################ [Loop Update] #################################
  # 对宽数据逐行赋予值
  for (i in 1:nrow(df_wider)) {
    # 首先对刺激种类列[choose_col]进行操作
    for (col in choose_col) {
      # 如果现在是刺激种类列[choose_col]的第一行, 则给他们赋予初始值
      if (i == 1) {
        df_wider[i, col] <- initial_value
      } 
      # 如果不是刺激种类列[choose_col]第一行, 且是NA, 那么说明此次没有选择它
      else if (is.na(df_wider[i, col])) {
        # 它的值等于上次更新它时的数值. 也就是上一行的值
        df_wider[i, col] <- df_wider[i - 1, col]
      }
    }
    
    # 第i行中, 左选项和右选项叫什么
    L_name <- df_wider[[L_choice]][i]
    R_name <- df_wider[[R_choice]][i]
    # 得到左右选项的名字后, 基于该名字找到对应的价值.(刺激名被长转宽成了列名)
    df_wider$L_value[i] <- df_wider[[L_name]][i]
    df_wider$R_value[i] <- df_wider[[R_name]][i]
    
    ################################ [ CORE CODE ] #################################
    # 设置随机种子
    set.seed(seed = seed + i)
    
    df_wider$Try[i] <- expl_func(
      i = i,
      var1 = df_wider[[var1]][i],
      var2 = df_wider[[var2]][i],
      threshold = threshold,
      epsilon = epsilon,
      lambda = lambda
    )
    
    # 基于prob函数计算选择左边和右边的概率
    df_wider$L_prob[i] <- prob_func(
      L_value = df_wider$L_value[i],
      R_value = df_wider$R_value[i],
      try = df_wider$Try[i],
      var1 = df_wider[[var1]][i],
      var2 = df_wider[[var2]][i],
      LR = "L", 
      tau = tau,
      lambda = lambda
    )
    df_wider$R_prob[i] <- prob_func(
      L_value = df_wider$L_value[i],
      R_value = df_wider$R_value[i],
      try = df_wider$Try[i],
      var1 = df_wider[[var1]][i],
      var2 = df_wider[[var2]][i],
      LR = "R", 
      tau = tau,
      lambda = lambda
    )
    ################################ [ CORE CODE ] #################################
    
    ################################ [ soft-max ] ##################################    
    # 如果是softmax就基于概率随机选
    if (!(softmax %in% c(TRUE, FALSE))) {
      stop("softmax TRUE or FALSE?")
    }
    else if (softmax == TRUE) {
      if (!is.numeric(df_wider$L_value[i]) | !is.numeric(df_wider$R_value[i])) {
        stop("An error occurs when softmax == FALSE")
      }
      # 设置随机种子
      set.seed(seed = seed)
      # 基于刚刚的概率, 随机选一个. 而不是谁大选谁
      df_wider$Rob_Choose[i] <- sample(
        c(df_wider[[L_choice]][i], df_wider[[R_choice]][i]), 
        prob = c(df_wider$L_prob[i], df_wider$R_prob[i]),
        size = 1
      ) 
    } 
    # 如果不基于softmax, 就是谁大选谁
    else if (softmax == FALSE) {
      if (!is.numeric(df_wider$L_value[i]) | !is.numeric(df_wider$R_value[i])) {
        stop("An error occurs when softmax == FALSE")
      }
      # 如果左边大选左边
      else if (df_wider$L_value[i] > df_wider$R_value[i]) {
        df_wider$Rob_Choose[i] <- df_wider[[L_choice]][i]
      } 
      # 如果左边小于右边
      else if (df_wider$L_value[i] < df_wider$R_value[i]) {
        df_wider$Rob_Choose[i] <- df_wider[[R_choice]][i]
      } 
      # 一样大随机选一个(说明是DS情况), 或者极端情况[L == R]
      else if (df_wider$L_value[i] == df_wider$R_value[i]) {
        df_wider$Rob_Choose[i] <- sample(
          c(df_wider[[L_choice]][i], df_wider[[R_choice]][i]), 
          size = 1
        )
      } 
    }
    ################################ [ direction ] #################################
    # 如果选项即等于左也等于右, 则说明此时只有一个选项
    if (df_wider$Sub_Choose[i] == L_name & df_wider$Sub_Choose[i] == R_name) {
      df_wider$L_dir[i] <- 0
      df_wider$R_dir[i] <- 0
    } 
    # 选了左
    else if (df_wider$Sub_Choose[i] == L_name & df_wider$Sub_Choose[i] != R_name){
      df_wider$L_dir[i] <- 1
      df_wider$R_dir[i] <- 0
    }
    # 选了右
    else if (df_wider$Sub_Choose[i] != L_name & df_wider$Sub_Choose[i] == R_name){
      df_wider$L_dir[i] <- 0
      df_wider$R_dir[i] <- 1
    }
    else {
      print("L/R_dir ERROR")
    }
    
    ############################## [ log_likelyhood ] ##############################    
    # 基于得到的被试选项与强化学习估计的选择概率, 计算似然值
    df_wider$L_logl[i] <- df_wider$L_dir[i] * log(df_wider$L_prob[i] + 1e-10)
    df_wider$R_logl[i] <- df_wider$R_dir[i] * log(df_wider$R_prob[i] + 1e-10)
    
    ################################ [ accuracy ] ##################################
    if (!(df_wider$Rob_Choose[i] %in% choose_col)) {
      stop("ERROR in ACC")
    }
    # 如果被试和强化学习选择一致, 则是1, 不一致则是0
    else if (df_wider$Sub_Choose[i] == df_wider$Rob_Choose[i]) {
      df_wider$ACC[i] <- 1
    }
    else if (df_wider$Sub_Choose[i] != df_wider$Rob_Choose[i]) {
      df_wider$ACC[i] <- 0
    }
  }
  
  ################################# [ ROUNND ] ###################################  
  df_wider$L_prob <- round(df_wider$L_prob, digits)
  df_wider$R_prob <- round(df_wider$R_prob, digits)
  df_wider$L_logl <- round(df_wider$L_logl, digits)
  df_wider$R_logl <- round(df_wider$R_logl, digits)
  
  return(df_wider)
}