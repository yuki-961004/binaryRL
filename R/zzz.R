binaryRLStartupMessage <- function()
{
  # Startup message obtained as 
  # > figlet GA
  msg <- c(paste0(
  "
                                     +------------------------+
   ___                               |  +---------+           |
  |  _ \\                             |  |  RRRR   |   L       |
  | |_) (_)_ __   __ _ _ __ _   _    |  |  R   R  |   L       |
  |  _ \\| | '_ \\ / _` | '__| | | |   |  |  RRRR   |   L       |
  | |_) | | | | | (_| | |  | |_| |   |  |  R R    |   L       |
  |____/|_|_| |_|\\__,_|_|   \\__, |   |  |  R  RR  |   LLLLLL  |
                            |___/    |  +---------+           |
                                     +------------------------+  
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

