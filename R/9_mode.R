#' Pretend to be Raw Data
#'
#' @param data [list] a list resulting from the 'step7' process of the `output` function. 
#' 
#' @param mode [character] 'fit' or 'simulate' whether to generate raw data.
#'  Defaults to FALSE. Set to TRUE to generate fake data.
#'  This produces a data frame with the same format as
#'  the actual raw data.
#' 
#' @param sub_choose [character] column name of choices made by the subject. 
#'  e.g., `sub_choose = "Choose"`
#' 
#' @param rob_choose [character] column name of choices made by the model. 
#'  e.g., `rob_choose = "Rob_Choose"`
#'  you should ignore this argument
#' 
#' @param raw_cols [vector] default: c("Subject", "Block", "Trial", 
#'  "L_choice", "R_choice", "L_reward", "R_reward", "Choose", "Reward")
#'  These are the column names of the raw data. 
#'  Only required when `back = 'simulate'`.
#'
#' @returns binaryRL[list]:
#'   \itemize{
#'     \item{\code{data}: new raw data (decision made by robot)}
#'     \item{\code{params}: all parameters value}
#'     \item{\code{numeric}: ACC}
#'     \item{\code{numeric}: LogL}
#'     \item{\code{numeric}: AIC}
#'     \item{\code{numeric}: BIC}
#'   }
#'   
#' @noRd
#' 
mode <- function(
  data, 
  mode = "fit",
  sub_choose = "Sub_Choose",
  rob_choose = "Rob_Choose",
  raw_cols = c(
    "Subject", "Block", "Trial", 
    "L_choice", "R_choice", 
    "L_reward", "R_reward", 
    "Sub_Choose"
  )
){
  switch(
    mode, 
    "fit" = {
      data <- data
    }, 
    "simulate" = {
      data[[1]][[sub_choose]] <- data[[1]][[rob_choose]]
      data[[1]] <- data[[1]][, raw_cols]
    },
    "review" = {
      data <- data
    }, 
  )
  
  return(data)
}
