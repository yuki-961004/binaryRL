#' loop_action_c
#'
#' @param data raw data
#' @param L_choice The column name for the left option
#' @param R_choice The column name for the right option
#' @param sub The column name for the subject ID
#' @param choose The column name for the option chosen by the subject
#' @param value The column name for the stimulate's current value
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param n How many subjects' data do you need to run?
#' @param initial_value The initial value you assign to a stimulus, defaulting to 0
#' @param seed seed
#' @param softmax use softmax or not, defaulting to TRUE
#' @param tau The τ parameter in the soft-max function, with a default value of 1
#' @param beta Other parameters that you think might influence the softmax function
#' @param prob_func The soft-max function, which you can customize.
#' 
#' @return robot choose R or L, loop for every subject
#' @export
#'
loop_action_c <- function(
    # update_v数据集
  data,
  # 左右选项
  L_choice = "DL",
  R_choice = "DR",
  sub = "Subject",
  choose = "Choose",
  value = "V_value",
  time_line = c("Block", "Trial"),
  # 初始值
  n,
  initial_value = 0,
  seed = 123,
  softmax = TRUE,
  tau = 1,
  beta = NA,
  prob_func = ex_func_prob
) {
  
  df_split <- base::split(x = data, f = data[[sub]])
  df_res <- list()
  
  for (i in n) {
    df_subject <- df_split[[i]]
    df_res[[i]] <- rl_action_c(
      data = df_subject, 
      # 左右选项
      L_choice = L_choice,
      R_choice = R_choice,
      choose = choose,
      value = value,
      # 初始值
      initial_value = initial_value,
      seed = seed,
      softmax = softmax,
      tau = tau,
      beta = beta,
      prob_func = prob_func
    )
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