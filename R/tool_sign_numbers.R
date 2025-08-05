sign_numbers <- function(x){
  if(sign(x) == 1){
    sign_char <- "+"
  }
  else {
    sign_char <- ""
  }
  
  return(sign_char)
}
