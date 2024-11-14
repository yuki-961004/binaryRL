# yukiRL
This package is suitable for binary-choice decision tasks and allows you to customize your reinforcement learning model.  

- *Assumption: value updates between different stimuli will not affect each other*  

I divide reinforcement learning into four steps:
## Step 1: Update values based on the value function.
 - `Value Function`, updating the value you assign to a stimulus based on the current reward.  

    **Learning Rates ($\eta$)**: It is a parameter that controls how quickly an agent updates its value estimates based on new information. The closer $\eta$ is to 1, the faster the learning rate.  
    **Subjective Utility ($\beta$)**: People's subjective perception of objective rewards: $\beta$ > 1 means exaggerating the received reward, while $\beta$ < 1 means diminishing the received reward.  
    
    <br>




## Step 2: Make choices according to the softmax function.
 - `Soft-Max Function`, calculating the probability of choosing a certain option based on the values of the two available options.   

    **Sensitivity of Value Differences ($\tau$)**: This value represents people's sensitivity to value differences. The larger $\tau$, the more sensitive they are to the differences in value between the two options.
## Step 3: Calculate the consistency rate between the robot's choices and the human choices.
 - `Log Likelihood`, $LL$ = $\sum$ $B_{L}$ $\times$ $\log P_{L}$ + $\sum$ $B_{R}$ $\times$ $\log P_{R}$   

    $B_{L}$ and $B_{R}$ the option that the subject chooses. ($B_{L} = 1$: subject chooses the left option; $B_{R} = 1$: subject chooses the right option)   
    $P_{L}$ and $P_{R}$ the probability of a subject selecting either a left or right option, as determined by a reinforcement learning model.   
    $N_{L}$ and $N_{R}$ the total number of trials the subject chose the left or right option.   

    Here, we seek to find the optimal parameters for the model by maximizing the `Log Likelihood (LL)` using `Genetic Algorithms`.
## Step 4: Generate simulated data based on the best parameters for each subject.
 - `Generate Simulated Data`: Given the `Value function` and the `Soft-Max function`, along with the corresponding parameters, simulate data.  

## How to cite 
...

## Install
```{r}
devtools::install_github("yuki-961004/yukiRL") 
```

## Classic Models

### 1. TD model ($\eta$, $\tau$)
*"The TD model is a standard temporal difference learning model (Barto, 1995; Sutton, 1988; Sutton and Barto, 1998)."*  
### 2. Risk-sensitive TD model ($\eta_{-}$, $\eta_{+}$, $\tau$)
"*In the risk-sensitive TD (RSTD) model, positive and negative prediction errors have asymmetric effects on learning (Mihatsch and Neuneier, 2002).*"  
### 3. Utility model ($\eta$, $\beta$, $\tau$)
*"The utility model is a TD learning model that incorporates nonlinear subjective utilities (Bernoulli, 1954)"*


<p align="center">
    <img src="./fig/rl_models.png" alt="RL Models" width="70%">
</p>

### References
Niv, Y., Edlund, J. A., Dayan, P., & O'Doherty, J. P. (2012). Neural prediction errors reveal a risk-sensitive reinforcement-learning process in the human brain. *Journal of Neuroscience, 32*(2), 551-562. https://doi.org/10.1523/JNEUROSCI.5498-10.2012

## Initial Value

Comparisons between the two learning rates generally revealed a positivity bias ($\alpha_{+}$ > $\alpha_{-}$)  
However, that on some occasions, studies failed to find a positivity bias or even reported a negativity bias ($\alpha_{+}$ < $\alpha_{-}$).  
Because Q-values initialization markedly affect learning rate and learning bias estimates.

### References
Palminteri, S., & Lebreton, M. (2022). The computational roots of positivity and confirmation biases in reinforcement learning. *Trends in Cognitive Sciences, 26*(7), 607-621. https://doi.org/10.1016/j.tics.2022.04.005

## Model Fit
$LL$ = $\sum$ $B_{L}$ $\times$ $\log P_{L}$ + $\sum$ $B_{R}$ $\times$ $\log P_{R}$   

$AIC$ =  $- 2 LL$ + $2 k$  
$BIC$ =  $- 2 LL$ + $k \times \log n$ 

$B_{L}$ and $B_{R}$ the option that the subject chooses. ($B_{L} = 1$: subject chooses the left option; $B_{R} = 1$: subject chooses the right option)   
$P_{L}$ and $P_{R}$ the probability of a subject selecting either a left or right option, as determined by a reinforcement learning model.   
$N_{L}$ and $N_{R}$ the total number of trials the subject chose the left or right option.   

${k}$ the number of free parameters in the model.   
${n}$ represents the total number of trials in the paradigm.
### References

Hampton, A. N., Bossaerts, P., & O'doherty, J. P. (2006). The role of the ventromedial prefrontal cortex in abstract state-based inference during decision making in humans. *Journal of Neuroscience, 26*(32), 8360-8367. https://doi.org/10.1523/JNEUROSCI.1010-06.2006

---

### My understanding
In my understanding, the value function in reinforcement learning for a two-alternative decision task can be written as:

$Value_{n}$ = $Value_{n-1}$ + $\eta$ $\times$ ($\beta$ $\times$ $Reward_{n}$ - $Value_{n-1}$)

- The `TD model` only consider **learning rates ($\eta$)** as a free parameter.   
- The `Risk-sensitive TD model` is based on `TD model` and assumes that the **learning rates ($\eta$)** are different for gains and losses.
- The `Utility model` introduces a **subjective utility ($\beta$)** for rewards based on this foundation. 

*NOTE*: 
1. Considering that the initial value has a significant impact on the parameter estimation of the **learning rates ($\eta$)**. When the initial value is not set (`initial_value = NA`), it is taken to be the reward received for that stimulus the first time.  
2. I assume that there is a linear relationship between subjective value and objective value. In fact, it may be in other forms: 
- $U(v) = \beta \times V$
- $U(v) = V^\beta$
- $U(v) = \beta \times V^2$
## Examples
### Load Pacakge
```{r}
library(yukiRL)
library(GA)
```
### Example Function

#### Learning Rate ($\eta$)   
```{r}
print(yukiRL::func_eta)
```
```
#> func_eta <- function (
#>   value, temp, reward, ev, frame, occurrence, eta, epsilon = NA
#> ){
#>   if (length(eta) == 1) {
#>     eta <- as.numeric(eta)
#>   }
#>   else if (length(eta) > 1 & temp < value) {
#>     eta <- eta[1]
#>   }
#>   else if (length(eta) > 1 & temp >= value) {
#>     eta <- eta[2]
#>   }
#>   else {
#>     eta <- "ERROR" 
#>   }
#>     return(eta)
#> }
```

#### Subjective Utility ($\beta$)  
```{r}
print(yukiRL::func_beta)
```
```
#> func_beta <- function(
#>   value, temp, reward, ev, frame, occurrence, beta = 1, epsilon = NA
#> ){
#>   if (length(beta) == 1) {
#>     beta <- beta
#>     temp <- beta * reward
#>   }
#>   else {
#>     temp <- "ERROR" 
#>   }
#>   return(list(beta, temp))
#> }
```

#### Sensitivity of Value Differences ($\tau$)
```{r}
print(yukiRL::func_prob)
```
```
#> func_prob <- function (
#>   L_value, R_value, ev, frame, tau = 1, params, LR 
#> ){
#>   if (!(LR %in% c("L", "R"))) {
#>       stop("LR = 'L' or 'R'")
#>   }
#>   else if (LR == "L") {
#>       prob <- 1/(1 + exp(-(L_value - R_value) * tau))
#>   }
#>   else if (LR == "R") {
#>       prob <- 1/(1 + exp(-(R_value - L_value) * tau))
#>   }
#>   else {
#>       prob <- "ERROR"
#>   }
#>   return(prob)
#> }
```

### Read your Raw Data
```{r simulated data}
raw <- [your_raw_data]
```
Make sure the global environment contains the raw data.   
Your dataset needs to include the following columns.   
`Block` and `Trial` columns are not mandatory, but there must be a column that represents the sequence of the experiment.
`EV` and `Frame` are also not mandatory columns. But if you need them in your function, you can enter their column names.
```
| Subject | Block | Trial | L_choice | R_choice | Choose | Reward |    | EV | Frame |
|---------|-------|-------|----------|----------|--------|--------|    |----|-------|
| 1       | 1     | 1     | A        | B        | A      | 5      |    | 80 |  High |
| 1       | 1     | 2     | A        | B        | B      | 3      |    | 80 |  High |
| 2       | 2     | 1     | X        | Y        | X      | 4      |    | 20 |  Low  |
| 2       | 2     | 2     | X        | Y        | Y      | 2      |    | 20 |  Low  |
```

### Creat a Object Function for `GA::ga`
Create a function that contains only the `params` argument, used for `GA::ga` to find the optimal solution.  
  
If you have already created your `value function` and `softmax function`, then here you only need to fill in the `[column names]` from your dataset into the corresponding arguments.   
```
 - sub <- "your_col_name[sub]"
```
Most importantly, replace the `function` with your custom function.
```
 - beta_func <- your_beta_func

 - eta_func <- your_eta_func  

 - prob_func <- your_prob_func
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
    sub = <col name [character] of subject id>
    choose = <col name [character] of subject's choice>,
    time_line = # <col name [vector], of block and trial>,
    expected_value = <col name [character] of expected value>
    decision_frame = <col name [character] of decision frame>
    n = 1, # subject id that will be analyzed
    # parameters
    initial_value = NA, 
    beta = 1,
    epsilon = NA,
    eta = c(params[1], params[2]),
    # your value function
    beta_func = yukiRL::func_beta,
    eta_func = yukiRL::func_eta
  ) 
################################## [Step 2] ####################################
  # Soft-Max Function
  step2 <- yukiRL::loop_action_c(
    data = step1,
    L_choice = <col name [character] of left choice>,
    R_choice = <col name [character] of right choice>,
    sub = <col name [character] of subject id>
    expected_value = <col name [character] of expected value>
    decision_frame = <col name [character] of decision frame>
    initial_value = NA,
    n = 1, # the params of subjects should be calculated one by one
    seed = 123,
    softmax = TRUE,
    # your soft-max function
    prob_func = yukiRL::func_prob,  
    # params in your soft-max function
    tau = c(params[3]),
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
  fitness = function(x) obj_func(x), 
  lower = c(0, 0, 0), # lower bounds of parameters
  upper = c(1, 1, 1), # upper bounds of parameters
  popSize = 50,       # Initial population size
  maxiter = 999,      # Maximum number of iterations
  run = 20,           # Number of iterations without improvement before stopping
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
  params_name = c("eta_neg", ""eta_pos", "tau")
)
```
```
|                 name|   value|
|---------------------|--------|
| Number of Parameters|    3.00|
|     Number of Trials|  288.00|
|             Accuracy|   85.42|
|       Log-Likelihood| -105.88|
|                  AIC|  215.76|
|                  BIC|  223.09|

|       name|    value|
|-----------|---------|
|  eta_neg  | 0.30344 |
|  eta_pos  | 0.57334 |
|    tau    | 0.03575 |
```

### Generate Decisions
Similar to the previous dataset, this time the data also requires rewards for both the left option and the right option.
```
| Subject | Block | Trial | L_choice | R_choice | L_reward | R_reward |
|---------|-------|-------|----------|----------|----------|----------|
| 1       | 1     | 1     | A        | B        | 1        | 5        |
| 1       | 1     | 2     | A        | B        | 2        | 3        |
| 2       | 2     | 1     | X        | Y        | 3        | 4        |
| 2       | 2     | 2     | X        | Y        | 4        | 2        |
```

```
################################## [Step 4] #################################### 
yukiRL::generate_d(
  data = <your data>,
  L_choice = <col_name [character] of left choice>,
  R_choice = <col_name [character] of right choice>,
  L_reward = <col_name [character] of left reward>,
  R_reward = <col_name [character] of right reward>,
  expected_value = <col name [character] of expected value>
  decision_frame = <col name [character] of decision frame>
  time_line = <col name [vector], of block and trial>,
  initial_value = 0,
  softmax = TRUE,
  seed = 123,
  beta = 1,
  epsilon = NA,
  eta = c(0.30344, 0.57334),
  tau = c(0.03575),
  params = NA,
  beta_func = yukiRL::func_beta,
  eta_func = yukiRL::func_eta,
  prob_func = yukiRL::func_prob
)
```

The reinforcement learning model will generate a column called `Rob_Choose`, indicating what the reinforcement learning algorithm would choose when faced with this option.