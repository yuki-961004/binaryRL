binaryRLStartupMessage <- function()
{
  # Startup message obtained as 
  # > figlet GA
  msg <- c(paste0(
    "
    Developed by 
    
     Y   Y         K   K    
      Y Y   u   u  K  K   i   
       Y    u   u  KKK       
       Y    u   u  K  K   i 
       Y     uuu   K   K  i   
       
    A Reinforcement Learning Package for TAFC Tasks   
    ", 
    "\n    Version: ", utils::packageVersion("binaryRL")))
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  requireNamespace("data.table", quietly = TRUE)
  
  # startup message
  msg <- binaryRLStartupMessage()
  if(!interactive())
    msg[1] <- paste(
      "Package 'binaryRL' version",
      utils::packageVersion("binaryRL")
    )
  packageStartupMessage(msg)
  invisible()
}

utils::globalVariables(c("fit_data"))