#' Title
#'
#' @param data a list resulting from the 'step7' process of the `output` function. 
#' @param back TRUE or FALSE, for model recovery
#' @param sub_choose colname 
#' @param rob_choose colname
#' @param raw_cols selected cols
#'
#' @returns raw data
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
#' step8 <- output(
#'   data = step7,
#'   n_params = 2,
#'   n_trials = 288,
#'   lambda = NA,
#'   gamma = 1,
#'   eta = c(0.3, 0.7),
#'   epsilon = NA,
#'   tau = 0.5
#'  )
#'  
#' step9 <- back (
#'   data = step8,
#'   back = TRUE
#' ) 
back <- function(
  data, 
  back = FALSE,
  sub_choose = "Choose",
  rob_choose = "Rob_Choose",
  raw_cols = c(
    "Subject", "Block", "Trial",
    "L_choice", "R_choice", "L_reward", "R_reward",
    "Choose", "Reward"
  )
){
  if (back == TRUE){
    data[[1]][[sub_choose]] <- data[[1]][[rob_choose]]
    data[[1]] <- data[[1]][, raw_cols]
  } else {
    data <- data
  }
  
  return(data)
}