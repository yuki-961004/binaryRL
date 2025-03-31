utils::globalVariables("fit_env")

binaryRLStartupMessage <- function()
{
  # Startup message obtained as 
  # > figlet GA
  msg <- c(paste0(
  "
                                      \u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557
                                      \u2551 \u2554----------\u2557             \u2551
                                      \u2551 | \u2588\u2588\u2588\u2588\u2588\u2588\u2557  |   \u2588\u2588\u2557       \u2551
 |     _)                             \u2551 | \u2588\u2588\u2554\u2550\u2550\u2588\u2588\u2557 |   \u2588\u2588\u2551       \u2551
 __ \\   |  __ \\    _` |   __|  |   |  \u2551 | \u2588\u2588\u2588\u2588\u2588\u2588\u2554\u255D |   \u2588\u2588\u2551       \u2551
 |   |  |  |   |  (   |  |     |   |  \u2551 | \u2588\u2588\u2554\u2550\u2550\u2588\u2588\u2557 |   \u2588\u2588\u2551       \u2551
_.__/  _| _|  _| \\__,_| _|    \\__, |  \u2551 | \u2588\u2588\u2551  \u2588\u2588\u2551 |   \u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2557  \u2551
                              ____/   \u2551 | \u255A\u2550\u255D  \u255A\u2550\u255D |   \u255A\u2550\u2550\u2550\u2550\u2550\u2550\u255D  \u2551
                                      \u2551 \u255A----------\u255D             \u2551
                                      \u255A\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255D


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
