#' Model: Utility
#'
#' @description
#'  \deqn{U(R) = {R}^{\gamma}}
#'  \deqn{V_{new} = V_{old} + \eta \cdot (U(R) - V_{old})}
#' 
#' @param params [vector] 
#' 
#' algorithm packages accept only one argument
#'
#' @return loss [numeric] 
#' 
#' algorithm packages accept only one return
#' 
#' @examples
#' \dontrun{
#' Utility <- function(params) {
#'   res <- binaryRL::run_m(
#'     data = data,
#'     id = id,
#'     eta = c(params[1]),
#'     gamma = c(params[2]),
#'     tau = c(params[3]),
#'     priors = priors,
#'     n_params = n_params,
#'     n_trials = n_trials,
#'     mode = mode,
#'     policy = policy
#'   )
#'
#'   assign(x = "binaryRL.res", value = res, envir = binaryRL.env)
#'   loss <- switch(EXPR = estimate, "MLE" = -res$ll, "MAP" = -res$lpo)
#'   switch(EXPR = mode, "fit" = loss, "simulate" = res, "replay" = res)
#' }
#' }

Utility <- function(params){
  
  res <- binaryRL::run_m(
    data = data,                   
    id = id,                        
    eta = c(params[1]),
    gamma = c(params[2]),
    tau = c(params[3]),
    priors = priors,
    n_params = n_params,                   
    n_trials = n_trials,
    mode = mode,
    policy = policy                
  )
  
  assign(x = "binaryRL.res", value = res, envir = binaryRL.env)
  loss <- switch(EXPR = estimate, "MLE" = -res$ll, "MAP" = -res$lpo)
  switch(EXPR = mode, "fit" = loss, "simulate" = res, "replay" = res)
}
