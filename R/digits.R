#' digits
#'
#' @param data A dataframe resulting from the 'step6' process of the `model_fit` function. 
#' 
#' @param options all alternative options
#' 
#' @param digits_1 The number of decimal places to retain for values related to the value function. 
#' The default is 2.
#' 
#' @param digits_2 The number of decimal places to retain for values related to the action function. 
#' The default is 5.
#'
#' @returns step7
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
#' step7 <- digits(
#'   data = step6, 
#'   options = step1[["options"]],
#'   digits_1 = 2, 
#'   digits_2 = 5
#' )
#' 
digits <- function(data, options, digits_1 = 2, digits_2 = 5){
  # round
  for (name in options) {
    data[[name]] <- round(data[[name]], digits_1)
  }
  
  data$L_value <- round(data$L_value, digits_1)
  data$R_value <- round(data$R_value, digits_1)
  
  data$V_value <- round(data$V_value, digits_1)
  data$V_update <- round(data$V_update, digits_1)
  
  data$L_prob <- round(data$L_prob, digits_2)
  data$R_prob <- round(data$R_prob, digits_2)
  
  data$L_logl <- round(data$L_logl, digits_2)
  data$R_logl <- round(data$R_logl, digits_2)
  
  return(data)
}