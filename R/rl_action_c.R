#' rl_action_c
#'
#' @param data Data for each subject
#' @param L_choice The column name for the left option
#' @param R_choice The column name for the right option
#' @param choose The column name for the option chosen by the subject
#' @param value The column name for the stimulate's current value
#' @param initial_value The initial value you assign to a stimulus, defaulting to 0
#' @param var1 extra variable 1
#' @param var2 extra variable 2
#' @param seed seed
#' @param softmax use softmax or not, defaulting to TRUE
#' @param tau The τ parameter in the soft-max function, with a default value of 1
#' @param lambda Other parameters that you think might influence the softmax function
#' @param prob_func The soft-max function, which you can customize.
#' @param digits digits
#'
#' @return robot choose R or L
#' @export
#'
rl_action_c <- function(
    # update_v数据集
  data,
  # 左右选项是什么, 对应的列名
  L_choice = "DL",
  R_choice = "DR",
  # 被试选择列的列名
  choose = "Choose",
  # 被试心中价值列的列名
  value = "V_value",
  # 额外需要用到的变量1
  var1 = NA,
  # 额外需要用到的变量2
  var2 = NA,
  # 被试心中价值初始值
  initial_value = 0,
  # softmax选择时的随机种子
  seed = 123,
  # 是否使用softmax, 还是说value谁大选谁
  softmax = TRUE,
  # softmax的固有参数, 默认为1
  tau,
  # 如果你的softmax含有别的参数, 就放在这里
  lambda = NA,
  # 示例softmax函数
  prob_func = func_tau,
  # 小数位数
  digits = 5
  ################################# [function start] #############################
){
  
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
    # 基于prob函数计算选择左边和右边的概率
    df_wider$L_prob[i] <- prob_func(
      L_value = df_wider$L_value[i],
      R_value = df_wider$R_value[i],
      var1 = df_wider[[var1]][i],
      var2 = df_wider[[var2]][i],
      LR = "L", 
      tau = tau,
      lambda = lambda
    )
    df_wider$R_prob[i] <- prob_func(
      L_value = df_wider$L_value[i],
      R_value = df_wider$R_value[i],
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