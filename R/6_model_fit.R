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
  for (i in 1:nrow(data)){
    
    # 记录人类的选择方向
    if (
      # 人类选了左边
      data$Sub_Choose[i] == data[[L_choice]][i] & 
      data$Sub_Choose[i] != data[[R_choice]][i]
    ) {
      data$L_dir[i] <- 1
      data$R_dir[i] <- 0
    } 
    else if (
      # 人类选了右边
      data$Sub_Choose[i] != data[[L_choice]][i] & 
      data$Sub_Choose[i] == data[[R_choice]][i]
    ) {
      data$L_dir[i] <- 0
      data$R_dir[i] <- 1
    } 
    else if (
      # 左右选项相等, 则这次不记录
      data$Sub_Choose[i] == data[[L_choice]][i] & 
      data$Sub_Choose[i] == data[[R_choice]][i]
    ) {
      data$L_dir[i] <- 0
      data$R_dir[i] <- 0
    } 
    else {
      data$L_dir[i] <- "ERROR"
      data$R_dir[i] <- "ERROR"
    }
    
    # 计算ACC
    if (data$Sub_Choose[i] == data$Rob_Choose[i]) {
      data$ACC[i] <- 1
    } 
    else if (data$Sub_Choose[i] != data$Rob_Choose[i]) {
      data$ACC[i] <- 0
    } 
    else {
      data$ACC[i] <- "ERROR"
    }
    
############################## [loss function] ################################# 
    
    # 计算左右选项的log-likelihood
    data$L_logl[i] <- loss_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      L_dir = data$L_dir[i],
      R_dir = data$R_dir[i],
      L_prob = data$L_prob[i],
      R_prob = data$R_prob[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      
      try = data$Try[i],
      LR = "L",
      
      value = data$V_value[i],
      utility = data$R_utility[i],
      reward = data$Reward[i],
      occurrence = data$Occurrence[i],
      
      alpha = alpha,
      beta = beta
    )
    
    data$R_logl[i] <- loss_func(
      i = i,
      L_freq = data$L_freq[i],
      R_freq = data$R_freq[i],
      L_pick = data$L_pick[i],
      R_pick = data$R_pick[i],
      L_value = data$L_value[i],
      R_value = data$R_value[i],
      L_dir = data$L_dir[i],
      R_dir = data$R_dir[i],
      L_prob = data$L_prob[i],
      R_prob = data$R_prob[i],
      var1 = data[[var1]][i],
      var2 = data[[var2]][i],
      
      try = data$Try[i],
      LR = "R",
      
      value = data$V_value[i],
      utility = data$R_utility[i],
      reward = data$Reward[i],
      occurrence = data$Occurrence[i],
      
      alpha = alpha,
      beta = beta
    )
  }

  return(data)
}
