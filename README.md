# yukiRL
This package is suitable for binary-choice decision tasks and allows you to customize your reinforcement learning model.  

I divide reinforcement learning into two parts:

 - `Value Function`: updating the value you assign to a stimulus based on the current reward.  
 - `Soft-max Function`, calculating the probability of choosing a certain option based on the values of the two available options.

In addition, we need to determine the optimal parameters. Here, I will use `Genetic Algorithms` to find the optimal solution for the model.
## How to cite 
not yet

## NAMESPACE
```{r}
importFrom("DiagrammeR", "%>%")
importFrom("dplyr", "bind_rows")
importFrom("tidyr", "pivot_wider")
importFrom("purrr", "map")
```

## Install
```{r}
devtools::install_github("yuki-961004/yukiRL") 
```

## Examples
### Load Packages
```{r simulated data}
library(dplyr)
library(yukiRL)
library(GA)
```
### Example Value Function
```{r}
print(yukiRL::ex_func_eta)

#> function (value, reward, occurrence, params) 
#> {
#>     if (is.na(reward)) {
#>         stop()
#>     }
#>     else if (reward > value) {
#>         eta <- params[1]
#>     }
#>     else if (reward <= value) {
#>         eta <- params[2]
#>     }
#>     else {
#>         eta <- "ERROR"
#>     }
#>     return(eta)
#> }
```

### Example Soft-Max Function
```{r}
print(yukiRL::ex_func_prob)

#> function (L_value, R_value, tau = 1, beta, LR) 
#> {
#>     if (!(LR %in% c("L", "R"))) {
#>         stop("LR = 'L' or 'R'")
#>     }
#>     else if (LR == "L") {
#>         prob <- 1/(1 + exp(-(L_value - R_value) * tau))
#>     }
#>     else if (LR == "R") {
#>         prob <- 1/(1 + exp(-(R_value - L_value) * tau))
#>     }
#>     else {
#>         prob <- "ERROR"
#>     }
#>     return(prob)
#> }
```

### Read example data
```{r simulated data}
raw <- your_raw_data
```
Your dataset needs to include the following columns. The `Block` and `Trial` columns are not mandatory, but there must be a column that represents the sequence of the experiment.
```
| Subject | Block | Trial | L_choice | R_choice | Choose | Reward |
|---------|-------|-------|----------|----------|--------|--------|
| 1       | 1     | 1     | A        | B        | A      | 5      |
| 1       | 1     | 2     | A        | B        | B      | 3      |
| 2       | 2     | 1     | X        | Y        | X      | 4      |
| 2       | 2     | 2     | X        | Y        | Y      | 2      |
```

### Object Function
```{r}
obj_func <- function(params){
################################## [ Raw ] #####################################
  # The original dataset needs to be in the global variables.
  data <- raw
################################## [Step 1] ####################################
  # Value Function
  step1 <- yukiRL::loop_update_v(
    data = data, 
    # column name
    sub = "Subject",
    choose = "Choose",
    time_line = c("Block", "Trial"),
    # subject numbers to be analyzed
    n = 1,
    initial_value = 0,
    params = c(params[1], params[2]),
    # your value function
    eta_func = yukiRL::ex_func_eta
  ) 
################################## [Step 2] ####################################
  # Soft-Max Function
  step2 <- yukiRL::loop_action_c(

    data = step1,
    # column name
    L_choice = "DL",
    R_choice = "DR",
    sub = "Subject",
    # subject numbers to be analyzed
    n = 1,
    initial_value = 0,
    seed = 123,
    softmax = TRUE,
    tau = params[3],
    beta = NA,
    prob_func = yukiRL::ex_func_prob  
  )
################################################################################  
  Log_Likelihood <- sum(step2$L_logl) + sum(step2$R_logl)
  mean_ACC <- mean(step2$ACC)
  
  return(Log_Likelihood)
}
```

### Genetic Algorithms
```{r}
cl <- parallel::makeCluster(parallel::detectCores() - 2)
doParallel::registerDoParallel(cores = cl)

ga_result <- GA::ga(
  type = "real-valued",
  fitness = function(x) obj_func(x),
  lower = c(0, 0, 0),  # lower bounds of parameters
  upper = c(2, 2, 2),  # upper bounds of parameters
  popSize = 50,        # Initial population size
  maxiter = 5,         # Maximum number of iterations
  run = 20,            # Number of iterations without improvement before stopping
  parallel = TRUE,          
  seed = 123                
)

parallel::stopCluster(cl)
foreach::registerDoSEQ()
rm(cl)
```

# Output
```{r}
############################# model fit parameters #############################
n_params <- ncol(ga_result@solution)
n_trials <- 288
Log_Likelihood <- ga_result@fitnessValue
############################## print parameters ################################
cat("Number of parameters (n_params):", n_params, "\n")
cat("Number of trials (n_trials):", n_trials, "\n")
cat("η+:", ga_result@solution[1,1], "\n")
cat("η-:", ga_result@solution[1,2], "\n")
cat("τ:", ga_result@solution[1,3], "\n")
cat("Log-Likelihood:", Log_Likelihood, "\n")
cat("AIC:", 2 * n_params - 2 * Log_Likelihood, "\n")
cat("BIC:", n_params * log(n_trials) - 2 * Log_Likelihood, "\n")
################################################################################
#> Number of parameters (n_params): 3   
#> Number of trials (n_trials): 288   
#> η+: 0.8274487   
#> η-: 0.6870329   
#> τ: 0.02093422   
#> #> Log-Likelihood: -154.14  
#> AIC: 314.28  
#> BIC: 325.2689  
```