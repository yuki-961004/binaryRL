model_fit <- function(
  data, 
  loss_func,
  alpha,
  beta,
  var1,
  var2,
  L_choice = "L_choice", 
  R_choice = "R_choice", 
  sub_choose = "Sub_Choose"
){
  # 如果输入了sub_choose, 就计算rob_choose和sub_choose的匹配度
  if (is.character(sub_choose)) {
    # 重新命名成Sub_Choose
    colnames(data)[colnames(data) == sub_choose] <- "Sub_Choose"
  }
  
  # ACC & LL
  # 记录人类选了左还是右
  chose_L <- (data$Sub_Choose == data[[L_choice]])
  chose_R <- (data$Sub_Choose == data[[R_choice]])
  
  # 如果选了左, 就是左 = 1, 右 = 0, 反之亦然
  data$L_dir <- as.integer(chose_L & !chose_R)
  data$R_dir <- as.integer(chose_R & !chose_L)
  
  # 如果人类选择和机器人选择一样, 这ACC为1, 否则为0
  data$ACC <- as.integer(data$Sub_Choose == data$Rob_Choose)
  
  # 向量型计算, 重点在于i = 1:nrow(data), 避免了循环
  data$L_logl <- loss_func(
    i = 1:nrow(data), 
    L_freq = data$L_freq,
    R_freq = data$R_freq,
    L_pick = data$L_pick,
    R_pick = data$R_pick,
    L_value = data$L_value,
    R_value = data$R_value,
    L_dir = data$L_dir,
    R_dir = data$R_dir,
    L_prob = data$L_prob,
    R_prob = data$R_prob,
    var1 = data[[var1]],
    var2 = data[[var2]],
    try = data$Try,
    LR = "L", 
    value = data$V_value,
    utility = data$R_utility,
    reward = data$Reward,
    occurrence = data$Occurrence,
    alpha = alpha,
    beta = beta
  )
  
  data$R_logl <- loss_func(
    i = 1:nrow(data),
    L_freq = data$L_freq,
    R_freq = data$R_freq,
    L_pick = data$L_pick,
    R_pick = data$R_pick,
    L_value = data$L_value,
    R_value = data$R_value,
    L_dir = data$L_dir,
    R_dir = data$R_dir,
    L_prob = data$L_prob,
    R_prob = data$R_prob,
    var1 = data[[var1]],
    var2 = data[[var2]],
    try = data$Try,
    LR = "R", 
    value = data$V_value,
    utility = data$R_utility,
    reward = data$Reward,
    occurrence = data$Occurrence,
    alpha = alpha,
    beta = beta
  )

  return(data)
}
