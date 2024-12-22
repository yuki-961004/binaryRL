#' loop_update_v
#' 
#' @param data A data frame containing the raw data. 
#' This data should include the following mandatory columns: 
#' - "sub", "time_line", "L_choice", "R_choice", "choose", "reward". 
#' The following arguments allow you to customize the column names used for processing
#' 
#' @param n A numeric value specifying the subject ID for which the model is being analyzed. 
#' The value should correspond to an entry in the "sub" column, which must contain the subject IDs. 
#' Provide the subject ID as a number.
#' e.g., `n = 18`
#' 
#' @param initial_value A numeric value representing the subject's initial expected value for each stimulus's reward. 
#' If this value is not set, the subject will use the reward received after the first trial as the initial value for that stimulus. 
#' In other words, the learning rate for the first trial is 100%. 
#' Provide the initial value as a number 
#' default: `initial_value = NA`
#' e.g., `initial_value = 0`
#' 
#' ## Parameters and Functions
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
#' @param lambda An additional parameter that may be used in these functions. 
#' Provide the value as a vector 
#' e.g., `lambda = c(0.4, 0.7, 20, 60)`
#' 
#' @param util_func The function for the utility gamma, which you can customize
#' @param rate_func The function for the learning rate eta, which you can customize
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
#' @param digits digits
#'
#' @return update value for every subject with whole choice
#' @export 
#'
loop_update_v <- function(
  data,
  # 要处理多少个被试. 由于估计时候是对被试分别进行, 所以这里也是被试序号
  n,
  # 被试心中价值初始值
  initial_value = NA,

  # parameters
  lambda = NA,
  gamma = 1,
  eta,
  # 价值函数选用示例函数
  util_func = func_gamma,
  rate_func = func_eta,

  # 被试序号列, 列名
  sub = "Subject",
  # 价值更新的时间线, 基于的列
  time_line = c("Block", "Trial"),
  # 被试选择列
  choose = "Choose",
  # 奖励所在的列
  reward = "Reward",
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,

  # 小数位数
  digits = 2
################################# [function start] #############################
){
################################# [split sub data] #############################
  # 按照[被试序号列][sub]分裂原始数据
  df_split <- base::split(x = data, f = data[[sub]])
  # 新建空list, 每个被试的结果存入该list的一个元素中
  df_res <- list()
  
  # 循环的每次针对一个被试
  for (i in n) {
    # 被试i的数据
    df_subject <- df_split[[i]]
    # 按照选择的不同进行分裂
    df_choose <- split(x = df_subject, f = df_subject[[choose]])
    ############################# [res for each stimulate] #########################
    # 每种选择的结果存入该list的一个元素中
    df_update <- list()
    ################################ [ CORE CODE ] #################################
    # 对每种choose运行update_v
    df_update <- purrr::map(
      # list对象, 每个元素执行一次function
      .x = df_choose, 
      # function是update_v
      .f = rl_update_v,
      # 价值更新的时间线, 基于的列
      time_line = time_line,
      # 奖励所在的列
      reward = reward,
      # 额外需要用到的变量1
      var1 = var1,
      # 额外需要用到的变量2
      var2 = var2,
      # 初始值
      initial_value = initial_value,
      # parameters
      lambda = lambda,
      gamma = gamma,
      eta = eta,
      # 价值函数选用示例函数
      util_func = util_func,
      rate_func = rate_func,
      # 小数位数
      digits = digits
    )
    ################################ [ CORE CODE ] #################################
    # 把所有choose类型的结果合并, 成为一个被试的结果
    df_res[[i]] <- dplyr::bind_rows(df_update)
  }
  
  ################################### [result] ###################################
  # 把所有被试的结果合并, 成为总结果
  temp_res <- dplyr::bind_rows(df_res) 
  # 此时排序基于sub和time_line
  order_var <- c(sub, time_line)
  # 基于time_line这个向量, 录入排序向量
  order_vector <- lapply(order_var, function(col) temp_res[[col]])
  # 基于排序向量对输入数据集进行排序
  res <- temp_res[do.call(order, order_vector), ]
  
  return(res)
}