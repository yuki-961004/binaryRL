#' ex_func_prob
#'
#' @param L_value The value of the left option
#' @param R_value The value of the right option
#' @param tau The τ parameter in the soft-max function, with a default value of 1
#' @param beta Other parameters that you think might influence the softmax function
#' @param LR Are you calculating the probability for the left option or the right option?
#'
#' @return example soft-max function
#' @export
#'
ex_func_prob <- function(
    L_value,
    R_value,
    tau = 1,
    beta,
    LR
){
  if (!(LR %in% c("L", "R"))) {
    stop("LR = 'L' or 'R'")
  }
  else if (LR == "L") {
    prob <- 1 / (1 + exp(-(L_value - R_value) * tau))
  }
  else if (LR == "R") {
    prob <- 1 / (1 + exp(-(R_value - L_value) * tau))
  }
  else {
    prob <- "ERROR"
  }
  return(prob)
}