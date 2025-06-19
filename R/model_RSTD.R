#' Model: RSTD
#'
#' @description
#'  \deqn{V_{new} = V_{old} + \eta_{+} \cdot (R - V_{old}), Reward > V_{old}}
#'  \deqn{V_{new} = V_{old} + \eta_{-} \cdot (R - V_{old}), Reward < V_{old}}
#' 
#' @param params [vector] algorithm packages accept only one argument
#'
#' @return [numeric] algorithm packages accept only one return
#' 
#' @examples
#' \dontrun{
#' RSTD <- function(params){
#'
#'   res <- binaryRL::run_m(
#'     data = data,
#'     id = id,
#'     eta = c(params[1], params[2]),
#'     tau = c(params[3]),
#'     n_params = n_params,
#'     n_trials = n_trials,
#'     mode = mode
#'   )
#'
#'   assign(x = "binaryRL.res", value = res, envir = binaryRL.env)
#'
#'   switch(mode, "fit" = -res$ll, "simulate" = res, "replay" = res)
#' }
#' }

RSTD <- function(params){
  
  res <- binaryRL::run_m(
    data = data,                   
    id = id,                        
    eta = c(params[1], params[2]), 
    tau = c(params[3]),
    n_params = n_params,                   
    n_trials = n_trials,
    mode = mode
  )
  
  assign(x = "binaryRL.res", value = res, envir = binaryRL.env)
  
  switch(mode, "fit" = -res$ll, "simulate" = res, "replay" = res)
}
