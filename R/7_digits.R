#' digits
#'
#' @param data A data frame resulting from the 'step6' process of the `model_fit` function. 
#' 
#' @param options all alternative options from 'step1' `unique_choice`
#' 
#' @param digits_1 The number of decimal places to retain for values related to the value function. 
#' The default is 2.
#' e.g., `Value_Option_A = 20.00` 
#' 
#' @param digits_2 The number of decimal places to retain for values related to the action function. 
#' The default is 5.
#' e.g., `L_logl = -123.12345` 
#'
#' @returns data frame:
#'   \itemize{
#'     \item{\code{data}: step6 + round(col, digits).}
#'   }
#' @export
#'
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
