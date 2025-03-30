binaryRLStartupMessage <- function()
{
  # Startup message obtained as 
  # > figlet GA
  msg <- c(paste0(
  "
                                      ╔══════════════════════════╗
                                      ║ ╔----------╗             ║
                                      ║ | ██████╗  |   ██╗       ║
 |     _)                             ║ | ██╔══██╗ |   ██║       ║
 __ \\   |  __ \\    _` |   __|  |   |  ║ | ██████╔╝ |   ██║       ║
 |   |  |  |   |  (   |  |     |   |  ║ | ██╔══██╗ |   ██║       ║
_.__/  _| _|  _| \\__,_| _|    \\__, |  ║ | ██║  ██║ |   ███████╗  ║
                              ____/   ║ | ╚═╝  ╚═╝ |   ╚══════╝  ║
                                      ║ ╚----------╝             ║
                                      ╚══════════════════════════╝


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

# fit_p会生成一个全局变量fit_data
utils::globalVariables(c("fit_data"))

