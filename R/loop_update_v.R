#' loop_update_v
#'
#' @param data raw data
#' @param sub The column name for the subject ID
#' @param choose The column name indicating which option the subject chose
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param n How many subjects' data do you need to run?
#' @param initial_value The initial value you assign to a stimulus, defaulting to 0
#' @param params The parameter values corresponding to eta_func.
#' @param eta_func The function for the learning rate η, which you can customize
#'
#' @return update value for every subject with whole choice
#' @export 
#'
loop_update_v <- function(
    data,
    # 被试序号列
    sub = "Subject",
    # 被试选择列
    choose = "Choose",
    # 价值更新的时间线, 基于的列
    time_line = c("Block", "Trial"),
    # 被试的id
    n,
    # 初始值
    initial_value = 0,
    # parameters
    params = c(0.3, 0.7),
    # 价值函数选用示例函数
    eta_func = ex_func_eta
) {
  # 按照被试分裂原始数据
  df_split <- base::split(x = data, f = data[[sub]])
  # 每个被试的结果存入该list的一个元素中
  df_res <- list()
  
  # 循环的每次针对一个被试
  for (i in n) {
    # 被试i的数据
    df_subject <- df_split[[i]]
    # 按照选择的不同进行分裂
    df_choose <- split(x = df_subject, f = df_subject[[choose]])
    # 每种选择的结果存入该list的一个元素中
    df_update <- list()
    # 对每种choose运行update_v
    df_update <- purrr::map(
      # list对象, 每个元素执行一次function
      .x = df_choose, 
      # function是update_v
      .f = rl_update_v,
      # 价值更新的时间线, 基于的列
      time_line = time_line,
      # 初始值
      initial_value = initial_value,
      # parameters
      params = params,
      # 价值函数选用示例函数
      eta_func = eta_func
    )
    # 把所有choose类型的结果合并, 成为一个被试的结果
    df_res[[i]] <- dplyr::bind_rows(df_update)
  }
  
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