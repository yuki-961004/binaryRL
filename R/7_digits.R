digits <- function(data, options, digits_1 = 2, digits_2 = 5){
  # round
  for (name in options) {
    data[[name]] <- round(data[[name]], digits_1)
  }
  
  data$L_value <- round(data$L_value, digits_1)
  data$R_value <- round(data$R_value, digits_1)
  
  data$L_bias <- round(data$L_bias, digits_1)
  data$R_bias <- round(data$R_bias, digits_1)
  
  data$V_value <- round(data$V_value, digits_1)
  data$V_update <- round(data$V_update, digits_1)
  
  data$L_prob <- round(data$L_prob, digits_2)
  data$R_prob <- round(data$R_prob, digits_2)
  
  data$L_logl <- round(data$L_logl, digits_2)
  data$R_logl <- round(data$R_logl, digits_2)
  
  return(data)
}
