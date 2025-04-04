#' TD model for fit
#'
#' @param params [vector] algorithm packages accept only one argument
#'
#' @returns negative log likelihood
#' @export
#'

TD <- function(params){
  
  res <- binaryRL::run_m(
    data = data,                   
    id = id,                        
    eta = c(params[1]), 
    tau = c(params[2]),
    n_params = n_params,                   
    n_trials = n_trials,
    mode = mode
  )
  
  assign(x = "binaryRL.res", value = res, envir = binaryRL.env)
  
  switch(mode, "fit" = return(-res$ll), "simulate" = return(res))
}
