#' model_fit
#'
#' @param data A dataframe resulting from the 'step5' process of the `decision_making` function. 
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
#' e.g., `sub_choose = "Choose"`
#'
#' @returns step6
#' @export
#'
#' @examples
#' data <- TAFC[TAFC$Subject == 1, ]
#' 
#' step1 <- unique_choice(
#'   data = data,
#'   L_choice = "L_choice", 
#'   R_choice = "R_choice"
#'  )
#'  
#' step2 <- arrange_data(
#'   data = step1[[1]],
#'   time_line = c("Block", "Trial")
#' )
#' 
#' step3 <- add_NA(
#'   data = step2
#' )
#' 
#' step4 <- set_initial_value(
#'   data = step3, 
#'   options = step1[["options"]], 
#'   initial_value = NA
#' )
#' step5 <- decision_making(
#'   data = step4,
#'   options = step1[["options"]],
#'   L_choice = "L_choice", R_choice = "R_choice",
#'   L_reward = "L_reward", R_reward = "R_reward",
#'   softmax = TRUE,
#'   threshold = 1,
#'   initial_value = NA,
#'   
#'   lambda = NA,
#'   gamma = 1,
#'   eta = c(0.3, 0.7),
#'   epsilon = NA,
#'   tau = 0.5
#' )
#' 
#' step6 <- model_fit(
#'   data = step5, 
#'   L_choice = "L_choice",
#'   R_choice = "R_choice",
#'   sub_choose = "Choose"
#' )
#' 
model_fit <- function(
  data, 
  L_choice = "L_choice", 
  R_choice = "R_choice", 
  sub_choose = "Choose"
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