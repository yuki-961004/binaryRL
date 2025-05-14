data <- binaryRL::Mason_2024_Exp1

res <- binaryRL::run_m(
  data = data,
  id = 1,
  eta = c(0.321, 0.765),
  n_params = 2, 
  n_trials = 360
)

summary(res)