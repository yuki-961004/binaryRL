#' rl_action_c
#'
#' @param data Data for each subject
#' @param L_choice The column name for the left option
#' @param R_choice The column name for the right option
#' @param choose The column name for the option chosen by the subject
#' @param value The column name for the stimulate's current value
#' @param initial_value The initial value you assign to a stimulus, defaulting to 0
#' @param seed seed
#' @param softmax use softmax or not, defaulting to TRUE
#' @param tau The τ parameter in the soft-max function, with a default value of 1
#' @param beta Other parameters that you think might influence the softmax function
#' @param prob_func The soft-max function, which you can customize.
#'
#' @return robot choose R or L
#' @export
#'
rl_action_c <- function(
    # update_v数据集
  data,
  # 左右选项
  L_choice = "DL",
  R_choice = "DR",
  # column choose
  choose = "Choose",
  # column value
  value = "V_value",
  # 初始值
  initial_value = 0,
  seed = 123,
  softmax = TRUE,
  tau = 1,
  beta = NA,
  prob_func = ex_func_prob
){
  # 长转宽
  data$names <- data[[choose]]
  data$values <- data[[value]]
  df_wider <- data %>%
    tidyr::pivot_wider(
      names_from = "names",
      values_from = "values"
    )
  
  # 长转宽前后不同的列, 也就是Choose列
  wide_columns1 <- setdiff(x = names(data), y = "ID")
  wide_columns2 <- setdiff(x = names(df_wider), y = "ID")
  choose_col <- setdiff(wide_columns2, wide_columns1)
  
  # 初始化 L_value 和 R_value 列
  df_wider$L_value <- NA
  df_wider$R_value <- NA
  df_wider$L_prob <- NA
  df_wider$R_prob <- NA
  df_wider$Sub_Choose <- df_wider$Choose
  df_wider$Rob_Choose <- NA
  df_wider$L_dir <- NA
  df_wider$R_dir <- NA
  df_wider$L_logl <- NA
  df_wider$R_logl <- NA
  df_wider$ACC <- NA
  
  # 循环生成每一列
  for (i in 1:nrow(df_wider)) {
    # 基础是生成逐行更新的 L_value 和 R_value
    for (col in choose_col) {
      if (i == 1) {
        df_wider[i, col] <- initial_value
      } else if (is.na(df_wider[i, col])) {
        df_wider[i, col] <- df_wider[i - 1, col]
      }
    }
    
    # 基于L_choice, R_choice, 知道哪两列是选项列. 存入左右名字
    L_name <- df_wider[[L_choice]][i]
    R_name <- df_wider[[R_choice]][i]
    # 基于左右名字, 寻找左右选项此时价值
    df_wider$L_value[i] <- df_wider[[L_name]][i]
    df_wider$R_value[i] <- df_wider[[R_name]][i]
    ############################ [ CORE CODE ] #################################
    # 基于prob函数计算选择左边和右边的概率
    df_wider$L_prob[i] <- round(
      x = prob_func(
        L_value = df_wider$L_value[i],
        R_value = df_wider$R_value[i],
        LR = "L", 
        tau = tau,
        beta = beta
      ),
      digits = 2
    )
    df_wider$R_prob[i] <- round(
      x = prob_func(
        L_value = df_wider$L_value[i],
        R_value = df_wider$R_value[i],
        LR = "R", 
        tau = tau,
        beta = beta
      ),
      digits = 2
    )
    ############################################################################
    
    # 如果是softmax就基于概率随机选
    if (softmax == TRUE) {
      # 基于刚刚的概率, 随机选一个. 而不是谁大选谁
      set.seed(seed = seed)
      df_wider$Rob_Choose[i] <- sample(
        c(df_wider[[L_choice]][i], df_wider[[R_choice]][i]), size = 1, 
        prob = c(df_wider$L_prob[i], df_wider$R_prob[i])
      ) 
    } 
    # 如果不基于softmax, 就是谁大选谁
    else if (softmax == FALSE) {
      # 如果左边大选左边
      if (df_wider$L_value[i] > df_wider$R_value[i]) {
        df_wider$Rob_Choose[i] <- df_wider[[L_choice]][i]
      } 
      # 如果右边大选右边
      else if (df_wider$L_value[i] < df_wider$R_value[i]) {
        df_wider$Rob_Choose[i] <- df_wider[[R_choice]][i]
      } 
      # 一样大随机选一个(说明是DS情况)
      else if (df_wider$L_value[i] == df_wider$R_value[i]) {
        df_wider$Rob_Choose[i] <- sample(
          c(df_wider[[L_choice]][i], df_wider[[R_choice]][i]), size = 1
        )
      } 
      # 除此以外, 则说明有问题
      else {
        print("ERROR softmax")
      }
    }
    else {
      print("softmax TRUE or FALSE?")
    }
    
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
    # 基于得到的被试选项与强化学习估计的选择概率, 计算似然值
    df_wider$L_logl[i] <- round(df_wider$L_dir[i] * log(df_wider$L_prob[i] + 1e-10), 2)
    df_wider$R_logl[i] <- round(df_wider$R_dir[i] * log(df_wider$R_prob[i] + 1e-10), 2)
    # 如果被试和强化学习选择一致, 则是1, 不一致则是0
    if (df_wider$Sub_Choose[i] == df_wider$Rob_Choose[i]) {
      df_wider$ACC[i] <- 1
    }
    else {
      df_wider$ACC[i] <- 0
    }
  }
  
  return(df_wider)
}