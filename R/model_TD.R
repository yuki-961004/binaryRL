#' Model: TD
#' 
#' @description
#'  \deqn{V_{new} = V_{old} + \eta \cdot (R - V_{old})}
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
#' TD <- function(params) {
#'   res <- binaryRL::run_m(
#'     data = data,
#'     id = id,
#'     eta = c(params[1]),
#'     tau = c(params[2]),
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

TD <- function(params){
  
  res <- binaryRL::run_m(
    data = data,                   
    id = id,                        
    eta = c(params[1]), 
    tau = c(params[2]),
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
