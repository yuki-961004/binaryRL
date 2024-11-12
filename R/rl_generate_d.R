#' rl_generate_d
#'
#' @param data Data for each subject
#' @param L_choice The column name for the left option
#' @param R_choice The column name for the right option
#' @param L_reward The column name for the reward of right option
#' @param R_reward The column name for the reward of left option
#' @param time_line Variables used to represent the experimental timeline, such as block and trial
#' @param initial_value The initial value you assign to a stimulus, defaulting to 0
#' @param softmax use softmax or not, defaulting to TRUE
#' @param seed seed
#' @param beta In the Utility model, it is assumed that all rewards will be discounted
#' @param epsilon In the WXT model, the discount rate is divided into different intervals.
#' @param eta In the RSTD model, the learning rate is different for positive and negative conditions.
#' @param tau The τ parameter in the soft-max function, with a default value of 1
#' @param params Other parameters that you think might influence the softmax function
#' @param beta_func The function for the discount rate β, which you can customize
#' @param eta_func The function for the learning rate η, which you can customize
#' @param prob_func The soft-max function, which you can customize.
#'
#' @return generated data
#' @export
#' 
rl_generate_d <- function(
    data,
    L_choice,
    R_choice,
    L_reward,
    R_reward,
    time_line,
    initial_value = 0,
    softmax = TRUE,
    seed = 123,
    beta = 1,
    epsilon,
    eta,
    tau = 1,
    params = NA,
    beta_func,
    eta_func,
    prob_func
){
  # 获取 L_choice 和 R_choice 的唯一值
  unique_L <- unique(data[[L_choice]])
  unique_R <- unique(data[[R_choice]])
  
  # 检查是否一致
  if (!all(unique_L %in% unique_R) || !all(unique_R %in% unique_L)) {
    stop("Error: L_choice and R_choice have different unique values!")
  } else {
    # 如果一致，将唯一值输入到 alternative_choice 列
    alternative_choice <- unique_L
  }
  
  # 新建对应的列，命名为 LC 的唯一值，并赋值为 NA
  for (name in alternative_choice) {
    data[[name]] <- NA
  }
  
################################## [Arrange] ###################################
  # 基于time_line这个向量, 录入排序向量
  order_vector <- lapply(time_line, function(col) data[[col]])
  
  # 基于排序向量对输入数据集进行排序
  temp_data <- data[do.call(order, order_vector), ]
  
############################### [Add Null row] #################################
  # 生成一个与输入数据集相同的单行数据集. 用于存放初始值
  empty_row <- as.data.frame(matrix(ncol = ncol(data), nrow = 1))
  colnames(empty_row) <- colnames(data)
  
  # 在第一行插入一个空行
  temp_data <- rbind(empty_row, temp_data)
  temp_data$Time_Line <- seq(from = 0, to = nrow(data))
  
################################ [ new col ] ###################################
  
  temp_data$Reward <- NA
  temp_data$EV <- NA
  # 添加空列 update_v
  temp_data$V_value <- NA
  temp_data$beta <- NA
  temp_data$V_temp <- NA
  temp_data$eta <- NA
  temp_data$V_update <- NA
  
  # 添加空列 action_c
  temp_data$L_value <- NA
  temp_data$R_value <- NA
  temp_data$L_prob <- NA
  temp_data$R_prob <- NA
  temp_data$Rob_Choose <- NA
  
  # 设置初始值, 如果没有设置初始值, 则赋予一个0
  # 这个初始值并不是很关键, 只要有数字就行
  if (is.na(initial_value)) {
    # update_v 相关
    temp_data$V_value[1] <- 0
    temp_data$V_temp[1] <- 0
    temp_data$V_update[1] <- 0
    # action_c 相关
    temp_data$L_value[1] <- 0
    temp_data$R_value[1] <- 0
    
    # 给每个选项赋予初始值
    for (name in alternative_choice) {
      temp_data[[name]][1] <- 0
    }
  } else {
    # update_v 相关
    temp_data$V_value[1] <- initial_value
    temp_data$V_temp[1] <- initial_value
    temp_data$V_update[1] <- initial_value
    # action_c 相关
    temp_data$L_value[1] <- initial_value
    temp_data$R_value[1] <- initial_value
    
    # 给每个选项赋予初始值
    for (name in alternative_choice) {
      temp_data[[name]][1] <- initial_value
    }
  }
  
  # 逐行更新Value
  for (i in 2:nrow(temp_data)) {
    
    # 记录此时L和R的名字
    L_name <- temp_data[[L_choice]][i]
    R_name <- temp_data[[R_choice]][i]
    # 此时V_value的值传入L_value和R_value
    temp_data$L_value[i] <- temp_data[[L_name]][i - 1]
    temp_data$R_value[i] <- temp_data[[R_name]][i - 1]
    
################################ [ CORE CODE ] #################################
    
    # 查询此次选择时, 已经选过哪些了
    chosen <- unique(temp_data$Rob_Choose)
    
################################ [ 1+ CHOOSE ] #################################
    # 如果选项都不是第一次出现, 则正常计算概率
    if ((temp_data[[L_choice]][i] %in% chosen) & (temp_data[[R_choice]][i] %in% chosen)) {
      # 基于prob函数计算选择左边和右边的概率
      temp_data$L_prob[i] <- prob_func(
        L_value = temp_data$L_value[i],
        R_value = temp_data$R_value[i],
        LR = "L", 
        tau = tau,
        params = params
      )
      temp_data$R_prob[i] <- prob_func(
        L_value = temp_data$L_value[i],
        R_value = temp_data$R_value[i],
        LR = "R", 
        tau = tau,
        params = params
      )
############################### [ 1st CHOOSE ] #################################
    } else if (!(temp_data[[L_choice]][i] %in% chosen) & (temp_data[[R_choice]][i] %in% chosen)) {
      # 如果左边选项是第一次出现, 则一定选左边  
      temp_data$L_prob[i] <- 1
      temp_data$R_prob[i] <- 0
    } else if ((temp_data[[L_choice]][i] %in% chosen) & !(temp_data[[R_choice]][i] %in% chosen)) {
      # 如果右边选项是第一次出现, 则一定选右边
      temp_data$L_prob[i] <- 0
      temp_data$R_prob[i] <- 1
    } else if (!(temp_data[[L_choice]][i] %in% chosen) & !(temp_data[[R_choice]][i] %in% chosen)) {
      # 如果都是第一次出现, 则随便选
      temp_data$L_prob[i] <- 0.5
      temp_data$R_prob[i] <- 0.5
    }
    
############################### [ PASS VALUE ] #################################  
    # 去上一行找每个选项此时的value
    for (name in alternative_choice) {
      temp_data[[name]][i] <- temp_data[[name]][i - 1]
    }
    
################################ [ Soft-Max ] ##################################    
    # 如果是softmax = TRUE就基于概率随机选
    if (!(softmax %in% c(TRUE, FALSE))) {
      stop("softmax TRUE or FALSE?")
    } else if (softmax == TRUE) {
      if (!is.numeric(temp_data$L_value[i]) | !is.numeric(temp_data$R_value[i])) {
        stop("An error occurs when softmax == FALSE")
      }
      # 设置随机种子
      set.seed(seed = seed)
      # 基于刚刚的概率, 随机选一个. 而不是谁大选谁
      temp_data$Rob_Choose[i] <- sample(
        c(temp_data[[L_choice]][i], temp_data[[R_choice]][i]), 
        prob = c(temp_data$L_prob[i], temp_data$R_prob[i]),
        size = 1
      ) 
      # 如果softmax = FALSE, 则按照谁大选谁
    } else if (softmax == FALSE) {
      if (!is.numeric(temp_data$L_value[i]) | !is.numeric(temp_data$R_value[i])) {
        stop("An error occurs when softmax == FALSE")
      } else if (temp_data$L_value[i] > temp_data$R_value[i]) {
        # 如果左边大选左边
        temp_data$Rob_Choose[i] <- temp_data[[L_choice]][i]
      } else if (temp_data$L_value[i] < temp_data$R_value[i]) {
        # 如果左边小于右边
        temp_data$Rob_Choose[i] <- temp_data[[R_choice]][i]
      } else if (temp_data$L_value[i] == temp_data$R_value[i]) {
        # 一样大随机选一个(是Single情况), 或者极端情况[Value_L == Value_R]
        temp_data$Rob_Choose[i] <- sample(
          c(temp_data[[L_choice]][i], temp_data[[R_choice]][i]), 
          size = 1
        )
      } 
    }
################################## [ Reward ] ##################################    
    # 基于选择, 来给予奖励
    if (temp_data$Rob_Choose[i] == temp_data[[L_choice]][i]){
      temp_data$Reward[i] <- temp_data[[L_reward]][i]
    } else if (temp_data$Rob_Choose[i] == temp_data[[R_choice]][i]) {
      temp_data$Reward[i] <- temp_data[[R_reward]][i]
    }
    
################################ [ update_v ] ##################################     
    # 记录这次选了哪个
    choose <- temp_data$Rob_Choose[i]
    # 看到奖励前, 对该选项预期的奖励, 去上一行找
    temp_data$V_value[i] <- temp_data[[choose]][i - 1]
    
    # 看到reward之后的折扣率, 用beta_func选择此时对应的beta, 计算出V_temp
    beta_temp <- beta_func(
      value = temp_data$V_value[i],
      temp = temp_data$V_temp[i],
      reward = temp_data$Reward[i],
      ev = temp_data$EV[i],
      occurrence = temp_data$Time_Line[i],
      beta = beta,
      epsilon = epsilon
    )
    temp_data$beta[i] <- as.numeric(beta_temp[1])
    temp_data$V_temp[i] <- as.numeric(beta_temp[2])
    
    # 看到reward之后的学习率, 用eta_func选择此时对应的eta
    temp_data$eta[i] <- eta_func(
      value = temp_data$V_value[i],
      temp = temp_data$V_temp[i],
      reward = temp_data$Reward[i],
      occurrence = temp_data$Time_Line[i],
      eta = eta,
      epsilon = epsilon
    )
    
    # 如果是第一次选这个选项, 直接将temp赋予给V_update
    if (is.na(initial_value) & !(choose %in% chosen)) {
      temp_data$V_update[i] <- temp_data$V_temp[i]
      temp_data[[choose]][i] <- temp_data$V_update[i]
      # 如果这次的选项是选过的, 正常按照eta更新价值
    } else {
      temp_data$V_update[i] <- temp_data$V_value[i] + 
        temp_data$eta[i] * (temp_data$V_temp[i] - temp_data$V_value[i])
      temp_data[[choose]][i] <- temp_data$V_update[i]  
    } 
  }
############################## [delete first row] ############################## 
  # 删除第一行赋予的初始值
  res_data <- temp_data[-1, ]
  # round
  res_data$L_prob <- round(res_data$L_prob, 3)
  res_data$R_prob <- round(res_data$R_prob, 3)
  
  res_data$L_value <- round(res_data$L_value, 2)
  res_data$R_value <- round(res_data$R_value, 2)
  
  res_data$V_value <- round(res_data$V_value, 2)
  res_data$V_update <- round(res_data$V_update, 2)
  
  for (name in alternative_choice) {
    res_data[[name]] <- round(res_data[[name]], 2)
  }
  
  return(res_data)
}
