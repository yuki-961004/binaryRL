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
importFrom("dplyr", "%>%", "bind_rows")
importFrom("tidyr", "pivot_wider")
importFrom("purrr", "map")
importFrom("utils", "capture.output")
```

## Install
```{r}
devtools::install_github("yuki-961004/yukiRL") 
```

## Examples
### Load Pacakge
```{r}
library(yukiRL)
library(GA)
```
### Example Value Function
#### Discount Rate β
```{r}
print(yukiRL::ex_func_eta)

#> ex_func_beta <- function(value, temp, reward, occurrence, beta = 1, epsilon = NA){
#>   if (any(is.na(epsilon))) {
#>     beta <- beta
#>     temp <- beta * reward
#>   }
#>   else {
#>     temp <- "ERROR" 
#>   }
#>   return(list(beta, temp))
#> }
```

#### Learning Rate η
```{r}
print(yukiRL::ex_func_eta)

#> function (value, temp, reward, occurrence, eta) 
#> {
#>     if (is.na(temp)) {
#>         stop()
#>     }
#>     else if (temp > value) {
#>         eta <- eta[1]
#>     }
#>     else if (temp <= value) {
#>         eta <- eta[2]
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

#> function (L_value, R_value, tau = 1, params, LR) 
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

### Read your Raw Data
```{r simulated data}
raw <- [your_raw_data]
```
Make sure the global environment contains the raw data.   
Your dataset needs to include the following columns.   
`Block` and `Trial` columns are not mandatory, but there must be a column that represents the sequence of the experiment.
```
| Subject | Block | Trial | L_choice | R_choice | Choose | Reward |
|---------|-------|-------|----------|----------|--------|--------|
| 1       | 1     | 1     | A        | B        | A      | 5      |
| 1       | 1     | 2     | A        | B        | B      | 3      |
| 2       | 2     | 1     | X        | Y        | X      | 4      |
| 2       | 2     | 2     | X        | Y        | Y      | 2      |
```

### Creat a Object Function for `GA::ga`
Create a function that contains only the `params` argument, used for `GA::ga` to find the optimal solution.  
  
If you have already created your `value function` and `softmax function`, then here you only need to fill in the `[column names]` from your dataset into the corresponding arguments.   
```
 - sub = "your_col_name[sub]"
```
Most importantly, replace the `function` with your custom function.
```
 - eta_func = your_eta_func  
 - prob_func = your_prob_func
 ```
#### Example obj_func
```{r}
obj_func <- function(params){
################################## [ Raw ] #####################################
  # The original dataset needs to be in the global environment.
  data <- raw
################################## [Step 1] ####################################
  # Value Function
  step1 <- yukiRL::loop_update_v(
    data = data, 
    # column name
    sub = "Subject",
    choose = "Choose",
    time_line = c("Block", "Trial"),
    initial_value = 0,
    # subject numbers to be analyzed
    n = 1,
    # parameters
    beta = c(params[1]),
    epsilon = NA,
    eta = c(params[2], params[3]),
    # your value function
    beta_func = ex_func_beta,
    eta_func = ex_func_eta
  ) 
################################## [Step 2] ####################################
  # Soft-Max Function
  step2 <- yukiRL::loop_action_c(
    data = step1,
    # column name
    L_choice = "DL",
    R_choice = "DR",
    sub = "Subject",
    initial_value = 0,
    # subject numbers to be analyzed
    n = 1,
    seed = 123,
    softmax = TRUE,
    # your soft-max function
    prob_func = yukiRL::ex_func_prob,  
    # params in your soft-max function
    tau = params[4],
    params = NA
  )
################################## [Step 3] ####################################  
  mean_ACC <- round(mean(step2$ACC), 4) * 100
  cat("Mean Accuracy:", mean_ACC, "%", "\n")
  
  Log_Likelihood <- sum(step2$L_logl) + sum(step2$R_logl)
  return(Log_Likelihood)
}
```

### Genetic Algorithms
```{r}
cl <- parallel::makeCluster(parallel::detectCores() - 2)
doParallel::registerDoParallel(cores = cl)

ga_result <- GA::ga(
  type = "real-valued",
  fitness = function(x) obj_func(x),  # obj_func(params)
  lower = c(0, 0, 0, 1),                 # lower bounds of parameters
  upper = c(1, 1, 1, 1),                 # upper bounds of parameters
  popSize = 50,                       # Initial population size
  maxiter = 5,                        # Maximum number of iterations
  run = 20,                           # Number of iterations without improvement before stopping
  parallel = TRUE,          
  seed = 123                
)

parallel::stopCluster(cl)
foreach::registerDoSEQ()
rm(cl)
```

### Output
```{r}
yukiRL::output(
  ga_result = ga_result, 
  obj_func = obj_func,
  n_trials = 288,
  params_name = c("beta", "η+", "η-", "τ")
)

#> Number of Parameters: 3 
#> Number of Trials: 288 
#> 
#> Mean Accuracy: 69.44 %  
#> Log-Likelihood: -154.14  
#> AIC: 314.28  
#> BIC: 325.2689  
#> 
#> β: 0.1234567
#> η+: 0.8274487   
#> η-: 0.6870329   
#> τ: 0.02093422   
```