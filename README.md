# binaryRL
This package is designed to simplify the process of building reinforcement learning models. It allows beginners to easily construct a model with just an `if-else` statement, making model creation more accessible.

Before using this package, please make sure you agree with these assumptions.

1. The paradigm is a binary choice decision task.
2. When decision-makers encounter a new stimulus, they will try it once. 
3. Learning for different stimuli is independent, meaning rewards for one stimulus do not influence the learning process for others.

If you agree with these three points, I will introduce the process of my package.

## Step 1: Update Values Based on the Value Function

**Value Function** updating the value you assign to a stimulus based on the current reward.

$$  
V_{n} = V_{n-1} + \eta \cdot [U(R_{n}) - V_{n-1}]  
$$  

- **Learning Rates ($\eta$)**: This parameter controls how quickly an agent updates its value estimates based on new information. The closer $\eta$ is to 1, the faster the learning rate.

- **Utility Function ($\beta$)**: People's subjective perception of objective rewards. If you believe the relationship between objective value and subjective value is linear, represented by the equation:

$$  
U(R) = \beta \cdot R  
\quad \quad \Rightarrow \quad \quad
V_{n} = V_{n-1} + \eta \cdot (\beta \cdot R_{n} - V_{n-1})  
$$  
 

  *NOTE:* this relationship can take any form. It just represents how the objective value is transformed into subjective value.

## Step 2: Make choices according to the softmax function.
**Soft-Max Function** calculating the probability of choosing a certain option based on the values of the two available options.   

  $$
  P_{L} = \frac{1}{1 + e^{-(V_{L} - V_{R}) \cdot \tau}}
  \quad \quad
  P_{R} = \frac{1}{1 + e^{-(V_{R} - V_{L}) \cdot \tau}}
  $$
  
 - **Sensitivity of Value Differences ($\tau$)**: This value represents people's sensitivity to value differences. The larger $\tau$, the more sensitive they are to the differences in value between the two options.

## Step 3: Calculate the consistency rate between the robot's choices and the human choices.
**Log Likelihood** representing how similar human behavior is to robot behavior

  $$
  LL = \sum B_{L} \times \log P_{L} + \sum B_{R} \times \log P_{R}
  $$   

  *NOTE:* $B_{L}$ and $B_{R}$ the option that the subject chooses. ($B_{L} = 1$: subject chooses the left option; $B_{R} = 1$: subject chooses the right option); $P_{L}$ and $P_{R}$ represent the probabilities of selecting the left or right option, as predicted by the reinforcement learning model.   

  - Here, we seek to find the optimal parameters for the model by maximizing the **Log Likelihood (LL)** using Genetic Algorithms (`GA::ga`).

## Step 4: Generate simulated data based on the optimal parameters for each subject.
**Generate Simulated Data**: Given the **Value Function** and the **Soft-Max function**, along with the optimal parameters, simulate data.  

$$
binaryRL(\hat\eta, \hat\beta, \hat\tau) \quad \Rightarrow \quad Y \sim \text{data.frame}
$$

# How to cite 
Hu, M., & L, Z. (2025). binaryRL: A Package for Building Reinforcement Learning Models in R. *Journal*(7), 123-123. https://doi.org/


# Install
```{r}
devtools::install_github("yuki-961004/binaryRL") 
```

# Examples
## Load Pacakge
```{r}
library(binaryRL)
library(GA)
```
## Example Function

### Learning Rate ($\eta$)   
```{r}
print(binaryRL::func_eta)
```
```
#> func_eta <- function (
#>   value, temp, reward, var1, var2, occurrence, eta, epsilon = NA
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

### Subjective Utility ($\beta$)  
```{r}
print(binaryRL::func_beta)
```
```
#> func_beta <- function(
#>   value, temp, reward, var1, var2, occurrence, beta = 1, epsilon = NA
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

### Sensitivity of Value Differences ($\tau$)
```{r}
print(binaryRL::func_prob)
```
```
#> func_prob <- function (
#>   L_value, R_value, var1, var2, tau = 1, params, LR 
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

## Read your Raw Data
```{r simulated data}
raw <- [your_raw_data]
```
Make sure the global environment contains the raw data.   
Your dataset needs to include the following columns.   
`Block` and `Trial` columns are not mandatory, but there must be a column that represents the sequence of the experiment.
You can also add two additional variables as factors that the model needs to consider.
```
| Subject | Block | Trial | L_choice | R_choice | Choose | Reward |    | var1 | var2 |
|---------|-------|-------|----------|----------|--------|--------|    |------|------|
| 1       | 1     | 1     | A        | B        | A      | 5      |    |  ..  |  ..  |
| 1       | 1     | 2     | A        | B        | B      | 3      |    |  ..  |  ..  |
| 2       | 2     | 1     | X        | Y        | X      | 4      |    |  ..  |  ..  |
| 2       | 2     | 2     | X        | Y        | Y      | 2      |    |  ..  |  ..  |
```

## Creat a Object Function for `GA::ga`
Create a function that contains only the `params` argument, used for `GA::ga` to find the optimal solution.  
  
If you have already created your `value function` and `softmax function`, then here you only need to fill in the `[column names]` from your dataset into the corresponding arguments.   
```
> sub = "Subject"
> choose = "Choose"
> time_line = c("Block", "Trial")
```
Most importantly, replace the `function` with your custom function. Alternatively, you can just use the default function, which can run the three basic models.
```
> beta_func = your_beta_func
> eta_func = your_eta_func  
> prob_func = your_prob_func
 ```
### Example obj_func
```{r}
library(binaryRL)

obj_func <- function(params){
################################## [ Raw ] #####################################
  # The original dataset needs to be in the global environment.
  data <- raw
################################## [Step 1] ####################################
  # Value Function
  step1 <- binaryRL::loop_update_v(
    data = data, 
    sub = <col name [character] of subject id>
    choose = <col name [character] of subject's choice>,
    time_line = # <col name [vector], of block and trial>,
    var1 = <col name [character] of var1>
    var2 = <col name [character] of var2>
    n = 1, # subject id that will be analyzed
    # parameters
    initial_value = NA, 
    beta = 1,
    epsilon = NA,
    eta = c(params[1], params[2]),
    # your value function
    beta_func = binaryRL::func_beta,
    eta_func = binaryRL::func_eta
  ) 
################################## [Step 2] ####################################
  # Soft-Max Function
  step2 <- binaryRL::loop_action_c(
    data = step1,
    L_choice = <col name [character] of left choice>,
    R_choice = <col name [character] of right choice>,
    sub = <col name [character] of subject id>
    var1 = <col name [character] of var1>
    var2 = <col name [character] of var2>
    initial_value = NA,
    n = 1, # the params of subjects should be calculated one by one
    seed = 123,
    softmax = TRUE,
    # your soft-max function
    prob_func = binaryRL::func_prob,  
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

## Genetic Algorithms
```{r}
library(GA)

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

## Output
```{r}
binaryRL::output(
  ga_result = ga_result, 
  obj_func = obj_func,
  n_trials = 288,
  params_name = c("eta_neg", ""eta_pos", "tau"),
  digits = 5
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

## Generate Decisions
Unlike the previous dataset, this time the input dataset requires the rewards for both the left and right options.
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
binaryRL::generate_d(
  data = <your data>,
  L_choice = <col_name [character] of left choice>,
  R_choice = <col_name [character] of right choice>,
  L_reward = <col_name [character] of left reward>,
  R_reward = <col_name [character] of right reward>,
  time_line = <col name [vector], of block and trial>,
  var1 = <col name [character] of var1>
  var2 = <col name [character] of var2>
  initial_value = 0,
  softmax = TRUE,
  seed = 123,
  beta = 1,
  epsilon = NA,
  eta = c(0.30344, 0.57334),
  tau = c(0.03575),
  params = NA,
  beta_func = binaryRL::func_beta,
  eta_func = binaryRL::func_eta,
  prob_func = binaryRL::func_prob
)
```

The reinforcement learning model will generate a column called `Rob_Choose`, indicating what the reinforcement learning algorithm would choose when faced with this option.

# Classic Models

## 1. TD Model ($\eta$, $\tau$)
> *"The TD model is a standard temporal difference learning model (Barto, 1995; Sutton, 1988; Sutton and Barto, 1998)."*  
## 2. Risk-Sensitive TD Model ($\eta_{-}$, $\eta_{+}$, $\tau$)
> "*In the risk-sensitive TD (RSTD) model, positive and negative prediction errors have asymmetric effects on learning (Mihatsch and Neuneier, 2002).*"  
## 3. Utility Model ($\eta$, $\beta$, $\tau$)
> *"The utility model is a TD learning model that incorporates nonlinear subjective utilities (Bernoulli, 1954)"*


<p align="center">
    <img src="./fig/rl_models.png" alt="RL Models" width="70%">
</p>

### References
Niv, Y., Edlund, J. A., Dayan, P., & O'Doherty, J. P. (2012). Neural prediction errors reveal a risk-sensitive reinforcement-learning process in the human brain. *Journal of Neuroscience, 32*(2), 551-562. https://doi.org/10.1523/JNEUROSCI.5498-10.2012

## Initial Value

> *"Comparisons between the two learning rates generally revealed a positivity bias ($\alpha_{+} > \alpha_{-}$)"*  
> *"However, that on some occasions, studies failed to find a positivity bias or even reported a negativity bias ($\alpha_{+} < \alpha_{-}$)."*  
> *"Because Q-values initialization markedly affect learning rate and learning bias estimates."*

### References
Palminteri, S., & Lebreton, M. (2022). The computational roots of positivity and confirmation biases in reinforcement learning. *Trends in Cognitive Sciences, 26*(7), 607-621. https://doi.org/10.1016/j.tics.2022.04.005

## Model Fit
$$
LL = \sum B_{L} \times \log P_{L} + \sum B_{R} \times \log P_{R}
$$   

$$
AIC =  - 2 LL + 2 k
$$

$$
BIC =  - 2 LL + k \times \log n
$$ 

*NOTE:* ${k}$ the number of free parameters in the model; ${n}$ represents the total number of trials in the paradigm.
### References

Hampton, A. N., Bossaerts, P., & O'doherty, J. P. (2006). The role of the ventromedial prefrontal cortex in abstract state-based inference during decision making in humans. *Journal of Neuroscience, 26*(32), 8360-8367. https://doi.org/10.1523/JNEUROSCI.1010-06.2006

---

# My understanding
In my understanding, the value function in reinforcement learning for a two-alternative decision task can be written as:

$$
V_{n} = V_{n-1} + \eta \cdot [U(R_{n}) - V_{n-1}]
$$

- The `TD model` only consider **learning rates ($\eta$)** as a free parameter.   
- The `Risk-Sensitive TD model` is based on `TD model` and assumes that the **learning rates ($\eta$)** are different for gains and losses.
- The `Utility model` introduces a **utility function ($\beta$)** for rewards based on this foundation. 

## Utility Function
- I assume that there is a linear relationship between subjective value and objective value ($U(R) = \beta \cdot R$). In fact, it may be in other forms. 

## Initial Value
- Considering that the initial value has a significant impact on the parameter estimation of the **learning rates ($\eta$)** When the initial value is not set (`initial_value = NA`), it is taken to be the reward received for that stimulus the first time.

