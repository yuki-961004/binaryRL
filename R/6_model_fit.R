#' Calculate the Model Fit
#'
#' @param data [data.frame] A data frame resulting from the 'step5' process of the `decision_making` function. 
#' 
#' @param loss_func [func] Loss Function (Log-likelihood)
#' 
#' @param alpha [vector]
#' Extra parameters that may be used in functions. 
#'
#' @param beta [vector]
#' Extra parameters that may be used in functions. 
#' 
#' @param var1 [character] 
#' Column name of extra variable 1. If your model uses more than just reward 
#'  and expected value, and you need other information, such as whether the 
#'  choice frame is Gain or Loss, then you can input the 'Frame' column as 
#'  var1 into the model.
#'  
#'  \code{default: var1 = "Extra_Var1"}
#' 
#' @param var2 [character] 
#' Column name of extra variable 2. If one additional variable, var1, does not 
#'  meet your needs, you can add another additional variable, var2, into your 
#'  model.
#'  
#'  \code{default: var2 = "Extra_Var2"}
#' 
#' @param L_choice [character] column name of left choice. 
#'  e.g., `L_choice = "Left_Choice"`
#' 
#' @param R_choice [character] column name of right choice. 
#'  e.g., `R_choice = "Right_Choice"`
#' 
#' @param sub_choose [character] column name of choices made by the subject. 
#'  e.g., `sub_choose = "Choose"`
#'
#' @returns data frame:
#'   \itemize{
#'     \item{\code{data}: step5 + ACC + logL.}
#'   }
#'   
#' @noRd
#' 
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
      data$Sub_Choose[i] == data[[L_choice]][i] & 
      data$Sub_Choose[i] != data[[R_choice]][i]
    ) {
      data$L_dir[i] <- 1
      data$R_dir[i] <- 0
    } else if (
      data$Sub_Choose[i] != data[[L_choice]][i] & 
      data$Sub_Choose[i] == data[[R_choice]][i]
    ) {
      data$L_dir[i] <- 0
      data$R_dir[i] <- 1
    } else if (
      data$Sub_Choose[i] == data[[L_choice]][i] & 
      data$Sub_Choose[i] == data[[R_choice]][i]
    ) {
      data$L_dir[i] <- 0
      data$R_dir[i] <- 0
    } else {
      data$L_dir[i] <- "ERROR"
      data$R_dir[i] <- "ERROR"
    }
    
    # 计算ACC
    if (data$Sub_Choose[i] == data$Rob_Choose[i]) {
      data$ACC[i] <- 1
    } else if (data$Sub_Choose[i] != data$Rob_Choose[i]) {
      data$ACC[i] <- 0
    } else {
      data$ACC[i] <- "ERROR"
    }
    
    # 计算LL
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
