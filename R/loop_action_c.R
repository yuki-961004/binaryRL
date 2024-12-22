#' loop_action_c
#'
#' @param data A dataset that has been processed through the `loop_update_v` function, 
#' 
#' @param n A numeric value specifying the subject ID for which the model is being analyzed. 
#' The value should correspond to an entry in the "sub" column, which must contain the subject IDs. 
#' Provide the subject ID as a number.
#' default, `n = 1`
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
#' 
#' @return robot choose R or L, loop for every subject
#' @export
#'
loop_action_c <- function(
  # update_v数据集
  data,
  # 由update_v函数得到的结果里, 需要跑几个被试的action_c, 默认为1
  n = 1,
  # 初始值
  initial_value = 0,
  # 多少次之前是随机选的
  threshold = 20,

  # 多大概率进行探索
  lambda,
  epsilon = NA,
  # softmax的固有参数, 默认为1
  tau = 1,
  # 示例探索函数
  expl_func = func_epsilon,
  # 示例softmax函数
  prob_func = func_tau,

  # 左右选项列名
  L_choice = "LC",
  R_choice = "RC",
  # 被试序号列, 列名
  sub = "Subject",
  # 被试选择刺激列, 列名
  choose = "Choose",
  # 被试当前心中价值, 列名. 默认为"V_value"
  value = "V_value",
  # 表示时间线的列, 便于排序. 可以不止两列
  time_line = c("Block", "Trial"),
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,

  # 是否使用softmax, 还是说value谁大选谁
  softmax = TRUE,
  # softmax选择时的随机种子
  seed = 123,

  # 小数位数
  digits = 5
  ################################# [function start] #############################
) {
  ################################# [split sub data] #############################  
  df_split <- base::split(x = data, f = data[[sub]])
  df_res <- list()
  
  for (i in n) {
    df_subject <- df_split[[i]]
    ################################ [ CORE CODE ] #################################
    df_res[[i]] <- rl_action_c(
      data = df_subject, 
      initial_value = initial_value,
      threshold = threshold,

      lambda = lambda,
      epsilon = epsilon,
      tau = tau,
      expl_func = expl_func,
      prob_func = prob_func,

      L_choice = L_choice,
      R_choice = R_choice,
      choose = choose,
      value = value,
      var1 = var1,
      var2 = var2,
      
      softmax = softmax,
      seed = seed,

      digits = digits
    )
  }
  ################################ [ CORE CODE ] #################################  
  # 把所有被试的结果合并, 成为总结果
  temp_res <- dplyr::bind_rows(df_res) 
  
  ################################### [result] ###################################
  # 此时排序基于sub和time_line
  order_var <- c(sub, time_line)
  # 基于time_line这个向量, 录入排序向量
  order_vector <- lapply(order_var, function(col) temp_res[[col]])
  # 基于排序向量对输入数据集进行排序
  res <- temp_res[do.call(order, order_vector), ]
  
  return(res)
}