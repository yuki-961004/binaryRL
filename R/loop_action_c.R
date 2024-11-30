#' loop_action_c
#'
#' @param data raw data
#' @param L_choice The column name for the left option
#' @param R_choice The column name for the right option
#' @param sub The column name for the subject ID
#' @param choose The column name for the option chosen by the subject
#' @param value The column name for the stimulate's current value
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param var1 extra variable 1
#' @param var2 extra variable 2
#' @param initial_value The initial value you assign to a stimulus, defaulting to 0
#' @param n How many subjects' data do you need to run?
#' @param seed seed
#' @param softmax use softmax or not, defaulting to TRUE
#' @param tau The τ parameter in the soft-max function, with a default value of 1
#' @param lambda Other parameters that you think might influence the softmax function
#' @param prob_func The soft-max function, which you can customize.
#' @param digits digits
#' 
#' @return robot choose R or L, loop for every subject
#' @export
#'
loop_action_c <- function(
  # update_v数据集
  data,
  # 左右选项列名
  L_choice = "DL",
  R_choice = "DR",
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
  # 初始值
  initial_value = 0,
  # 由update_v函数得到的结果里, 需要跑几个被试的action_c, 默认为1
  n = 1,
  # softmax选择时的随机种子
  seed = 123,
  # 是否使用softmax, 还是说value谁大选谁
  softmax = TRUE,
  # softmax的固有参数, 默认为1
  tau = 1,
  # 如果你的softmax含有别的参数, 就放在这里
  lambda = NA,
  # 示例softmax函数
  prob_func = func_prob,
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
      L_choice = L_choice,
      R_choice = R_choice,
      choose = choose,
      value = value,
      initial_value = initial_value,
      var1 = var1,
      var2 = var2,
      seed = seed,
      softmax = softmax,
      tau = tau,
      lambda = lambda,
      prob_func = prob_func,
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