binaryRLStartupMessage <- function()
{
  # Startup message obtained as 
  # > figlet GA
  msg <- c(paste0(
  "
                                                  .                
                                                  :                 
                                            '.___/*\\___.'          
  A Model-Free Reinforcement Learning Tool    \\* \\ / */         
                                               >--X--<      
  for Two-Alternative Force Choice Tasks      /*_/ \\_*\\ 
                                            .'   \\*/   '.
                                                  :
                                                  '
  "))
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # startup message
  if(!interactive()){
    msg <- paste(
      "Package 'binaryRL' Version: ", utils::packageVersion("binaryRL")
    )
  }
  else {
    msg <- binaryRLStartupMessage()
  }
  
  packageStartupMessage(msg)
  
  invisible()
}

