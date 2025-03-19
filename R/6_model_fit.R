#' model_fit
#'
#' @param data A data frame resulting from the 'step5' process of the `decision_making` function. 
#' 
#' @param L_choice A string specifying the name of the column that represents the left choice. 
#' Provide the name of the column as a character string 
#' e.g., `L_choice = "Left_Choice"`
#' 
#' @param R_choice A string specifying the name of the column that represents the right choice. 
#' Provide the name of the column as a character string 
#' e.g., `R_choice = "Right_Choice"`
#' 
#' @param sub_choose A string specifying the name of the column that represents the choice made by the subject. 
#' Provide the name of the column as a character string 
#' e.g., `sub_choose = "Sub_Choose"`
#'
#' @returns data frame:
#'   \itemize{
#'     \item{\code{data}: step5 + ACC + logL.}
#'   }
#' @export
#' 
model_fit <- function(
  data, 
  L_choice = "L_choice", 
  R_choice = "R_choice", 
  sub_choose = "Sub_Choose"
){
  # 如果输入了sub_choose, 就计算rob_choose和sub_choose的匹配度
  if (is.character(sub_choose)) {
    # 重新命名成Sub_Choose
    colnames(data)[colnames(data) == sub_choose] <- "Sub_Choose"
  }
  
  # 计算ACC
  data$ACC <- NA
  
  for (i in 1:nrow(data)){
    if (data$Sub_Choose[i] == data$Rob_Choose[i]) {
      data$ACC[i] <- 1
    } else if (data$Sub_Choose[i] != data$Rob_Choose[i]) {
      data$ACC[i] <- 0
    } else {
      data$ACC[i] <- "ERROR"
    }
  }
  
  # 计算logL
  data$L_dir <- NA
  data$R_dir <- NA
  
  for (i in 1:nrow(data)){
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
  }
  
  # logL
  data$L_logl <- data$L_dir * log(data$L_prob + 1e-10)
  data$R_logl <- data$R_dir * log(data$R_prob + 1e-10)
  
  return(data)
}
