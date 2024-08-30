#' loop_update_v
#'
#' @param data raw data
#' @param sub The column name for the subject ID
#' @param choose The column name indicating which option the subject chose
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param initial_value The initial value you assign to a stimulus, defaulting to 0
#' @param n How many subjects' data do you need to run?
#' @param beta In the utility model, it is assumed that all rewards will be discounted
#' @param epsilon In the WXT model, the discount rate is divided into different intervals.
#' @param eta In the RSTD model, the learning rate is different for positive and negative conditions.
#' @param beta_func The function for the discount rate β, which you can customize
#' @param eta_func The function for the learning rate η, which you can customize
#'
#' @return update value for every subject with whole choice
#' @export 
#'
loop_update_v <- function(
    data,
    # 被试序号列, 列名
    sub = "Subject",
    # 被试选择列
    choose = "Choose",
    # 价值更新的时间线, 基于的列
    time_line = c("Block", "Trial"),
    # 被试心中价值初始值
    initial_value = 0,
    # 要处理多少个被试. 由于估计时候是对被试分别进行, 所以这里也是被试序号
    n,
    # parameters
    beta = 1,
    epsilon = NA,
    eta,
    # 价值函数选用示例函数
    beta_func = ex_func_beta,
    eta_func = ex_func_eta
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
      # 初始值
      initial_value = initial_value,
      # parameters
      beta = beta,
      epsilon = epsilon,
      eta = eta,
      # 价值函数选用示例函数
      beta_func = beta_func,
      eta_func = eta_func
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