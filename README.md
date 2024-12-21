# binaryRL
This package is designed to simplify the process of building reinforcement learning models. It allows beginners to easily construct a model with just an `if-else` statement, making model creation more accessible.

Before using this package, please make sure you agree with this assumptions.

> The paradigm is a binary choice decision task. And the learning for different stimuli is independent, meaning rewards for one stimulus do not influence the learning process for others.

If you agree with this assumptions, I will introduce how to use this package.

<!---------------------------------------------------------->

# How to cite 
Hu, M., & L, Z. (2025). binaryRL: A Package for Building Reinforcement Learning Models in R. *Journal*(7), 100-123. https://doi.org/


# Tutorial
## Install and Load Pacakge
```r
devtools::install_github("yuki-961004/binaryRL") 
library(binaryRL)
```

<!---------------------------------------------------------->

## Read your Raw Data
```r
# a data frame including these columns
data <- [your_data]
```

| Subject | Block | Trial | L_choice | R_choice | Choose | Reward | var1 | var2 |
|---------|-------|-------|----------|----------|--------|--------|------|------|
| 1       | 1     | 1     | A        | B        | A      | 5      |...   |...   |
| 1       | 1     | 2     | A        | B        | B      | 3      |...   |...   |
| 2       | 2     | 1     | X        | Y        | X      | 4      |...   |...   |
| 2       | 2     | 2     | X        | Y        | Y      | 2      |...   |...   | 
| ...     | ...   | ...   | ...      | ...      | ...    | ...    |...   |...   |

<details>
<summary>NOTES</summary>

1. Your dataset needs to include these **columns**.  
2. Make sure the **global environment** contains the raw data.   
3. `Block` and `Trial` columns are not mandatory, but there must be a column that represents the sequence of the experiment.  
4. You can also add two **additional variables** (var1, var2) as factors that the model needs to consider.

</details>

<!---------------------------------------------------------->

## Creat a Object Function for Algorithm Packages
Create a function that contains only the `params` argument.   

### Object Function
```r
obj_func <- function(params){
  res <- binaryRL::rl_run_m(
    data = data,                    # your data
    id = 18,                        # Subject ID
    eta = c(params[1], params[2]),  # RSTD parameters
    n_params = 2,                   # the number of free parameters
    n_trials = 288                  # the number of total trials
  )

  # pass the result out of the function (global environment)
  # make it easier to get the optimal parameters later
  binaryRL_res <<- res
  
  # if the algorithm is solving a minimization problem, return -ll
  invisible(-res$ll)
  # if the algorithm is solving a maximization problem, return ll
  # invisible(res$ll)
}
```

<!---------------------------------------------------------->

If your column names are different from my example, you need to fill in the column names in the argument of `binaryRL::rl_run_m`

<details>
<summary>Custom Column Names</summary>

```r
obj_func <- function(params){
  res <- binaryRL::rl_run_m(
    data = data,                    # your data
    id = 18,                        # Subject ID
    eta = c(params[1], params[2]),  # RSTD parameters
    n_params = 2,                   # the number of free parameters
    n_trials = 288,                 # the number of total trials

    # column names
    sub = "Subject",
    L_choice = "LC",
    R_choice = "RC",
    choose = "Choose",
    reward = "Reward",
    time_line = c("Block", "Trial"),
    var1 = "extra_Var1",
    var2 = "extra_Var2"
  )

  # pass the result out of the function (global environment)
  # make it easier to get the best parameters later
  binaryRL_res <<- res
  
  # if the algorithm is solving a minimization problem, return -ll
  invisible(-res$ll)
  # if the algorithm is solving a maximization problem, return ll
  # invisible(res$ll)
}
```

</details>  

<!---------------------------------------------------------->

You can also customize the `value function` and `action function`. The default function is applicable to three basic models. 

<details>
<summary>Custom Functions</summary>

```r
obj_func <- function(params){
  res <- binaryRL::rl_run_m(
    data = data,                    # your data
    id = 18,                        # Subject ID
    eta = c(params[1], params[2]),  # RSTD parameters
    n_params = 2,                   # the number of free parameters
    n_trials = 288,                 # the number of total trials

    # column names
    sub = "Subject",
    L_choice = "LC",
    R_choice = "RC",
    choose = "Choose",
    reward = "Reward",
    time_line = c("Block", "Trial"),
    var1 = "extra_Var1",
    var2 = "extra_Var2",

    # functions
    util_func = your_util_func,
    rate_func = your_rate_func,  
    expl_func = your_expl_func,
    prob_func = your_prob_func
  )

  # pass the result out of the function (global environment)
  # make it easier to get the best parameters later
  binaryRL_res <<- res
  
  # if the algorithm is solving a minimization problem, return -ll
  invisible(-res$ll)
  # if the algorithm is solving a maximization problem, return ll
  # invisible(res$ll)
}
```

</details>  

<!---------------------------------------------------------->

The following are the four basic functions used by default in the program. You can customize them based on this

<!---------------------------------------------------------->

<details>
<summary>Utility Function (γ)</summary>

```r
print(binaryRL::func_gamma)
```

```r
func_gamma <- function(
  value, utility, reward, occurrence, var1, var2, gamma, lambda
){
  if (length(gamma) == 1) {
    gamma <- gamma
    utility <- gamma * reward
  }
  else {
    utility <- "ERROR" 
  }
  return(list(gamma, utility))
}
```
</details>

<!---------------------------------------------------------->

<details>
<summary>Learning Rate Function (η)</summary>

```r
print(binaryRL::func_eta)
```

```r
func_eta <- function (
  value, utility, reward, occurrence, var1, var2, eta, lambda
){
  if (length(eta) == 1) {
    eta <- as.numeric(eta)
  }
  else if (length(eta) > 1 & utility < value) {
    eta <- eta[1]
  }
  else if (length(eta) > 1 & utility >= value) {
    eta <- eta[2]
  }
  else {
    eta <- "ERROR" 
  }
    return(eta)
}
```
</details>

<!---------------------------------------------------------->

<details>
<summary>Exploration Function (ε)</summary>

```r
print(binaryRL::func_epsilon)
```

```r
func_epsilon <- function(
  i, var1, var2, threshold, epsilon
){
  if (i <= threshold) {
    try <- 1
  } 
  else if (i > threshold & !(is.na(epsilon))){
    try <- sample(
      c(1, 0),
      prob = c(epsilon, 1 - epsilon),
      size = 1
    )
  } 
  else if (i > threshold & is.na(epsilon)) {
    try <- 0
  } 
  else {
    try <- "ERROR"
  }
  return(try)
}
```
</details>

<!---------------------------------------------------------->

<details>
<summary>Soft-Max Function (τ)</summary>

```r
print(binaryRL::func_tau)
```

```r
func_tau <- function (
  try, L_value, R_value, var1, var2, tau = 1, params, LR 
){
  if (!(LR %in% c("L", "R"))) {
    stop("LR = 'L' or 'R'")
  }
  else if (try == 0 & LR == "L") {
    prob <- 1 / (1 + exp(-(L_value - R_value) * tau))
  }
  else if (try == 0 & LR == "R") {
    prob <- 1 / (1 + exp(-(R_value - L_value) * tau))
  }
  else if (try == 1) {
    prob <- 0.5
  } 
  else {
    prob <- "ERROR"
  } 
  return(prob)
}
```

</details>

<!---------------------------------------------------------->

### Example Algorithms
Here is an example using `optim` from the `stats` (which is a default package in R), though it has issues with both runtime and accuracy.

<!---------------------------------------------------------->

```r
library(stats)

set.seed(123)
result <- stats::optim(
  par = c(0.5, 0.5),
  method = "L-BFGS-B",
  fn = obj_func,
  lower = c(0, 0),
  upper = c(1, 1),
  control = list(
    maxit = 10
  )
)

obj_func(params = as.vector(result$par))
summary(binaryRL_res)
```

```r
#> Results of the Reinforcement Learning Model:
#> 
#> Parameters:
#>    λ:  NA  
#>    γ:  1 
#>    η:  0.321 0.765 
#>    ε:  NA 
#>    τ:  0.5

#> Model Fit:
#>    Accuracy:  82.64 %
#>    LogL:  -115.30 
#>    AIC:  236.60 
#>    BIC:  247.59 
```

The following three algorithms require additional packages to run. You can click to check them out if you're interested.

<!---------------------------------------------------------->

<details>
<summary>Simulated Annealing (GenSA)</summary>

```r
install.packages("GenSA")
library(GenSA)

result <- GenSA::GenSA(
  fn = obj_func,
  lower = c(0, 0),
  upper = c(1, 1),
  control = list(
    maxit = 10,
    seed = 123
  )
)

obj_func(params = as.vector(result$par))
summary(binaryRL_res)
```

</details>

<!---------------------------------------------------------->

<details>
<summary>Genetic Algorithm (GA)</summary>

```r
install.packages("GA")
library(GA)

result <- GA::ga(
  type = "real-valued",
  fitness = function(x) obj_func(x),
  lower = c(0, 0),
  upper = c(1, 1),
  maxiter = 10,                     
  parallel = TRUE,          
  seed = 123                
)

obj_func(params = as.vector(result@solution))
summary(binaryRL_res)
```

</details>

<!---------------------------------------------------------->

<details>
<summary>Differential Evolution (DEoptim)</summary>

```r
install.packages("DEoptim")
library(DEoptim)

result <- DEoptim::DEoptim(
  fn = obj_func,
  lower = c(0, 0),
  upper = c(1, 1),
  control = DEoptim::DEoptim.control(
    itermax = 10,
    parallelType = c("parallel"),
    packages = c("binaryRL"),
    parVar = c("data")
  )
)

obj_func(params = as.vector(result$optim$bestmem))
summary(binaryRL_res)
```

</details>

<!---------------------------------------------------------->

## Generate Decisions
Unlike the previous dataset, this time the input dataset requires the rewards for both the left and right options. (The "Choose" column, as before, represents how the human made their choice in this context.) In addition, when generating simulation data, only one subject's data can be input at a time.

| Subject | Block | Trial | L_choice | R_choice | Choose | L_reward | R_reward |
|---------|-------|-------|----------|----------|--------|----------|--------- |
| 1       | 1     | 1     | A        | B        | A      | 1        | 5        |
| 1       | 1     | 2     | A        | B        | B      | 2        | 3        |
| 1       | 2     | 1     | X        | Y        | X      | 3        | 4        |
| 1       | 2     | 2     | X        | Y        | Y      | 4        | 2        |
| ...     | ...   | ...   | ...      | ...      | ...    | ...      | ...      |

The reinforcement learning model will generate a column called `Rob_Choose`, indicating what the reinforcement learning algorithm would choose when faced with this option. 

```r
simulated <- binaryRL::rl_generate_d(
  data = data,
  id = 18,
  eta = c(0.321, 0.765),
  n_params = 2, 
  n_trials = 288
)
summary(simulated)
```

```r
#> Results of the Reinforcement Learning Model:
#> 
#> Parameters:
#>    λ:  NA  
#>    γ:  1 
#>    η:  0.321 0.765 
#>    ε:  NA 
#>    τ:  0.5

#> Model Fit:
#>    Accuracy:  73.26 %
#>    LogL:  -197.80 
#>    AIC:  401.60  
#>    BIC:  412.59 
```

<!---------------------------------------------------------->

If your column names are different from my example, you need to fill in the column names in the argument of `binaryRL::rl_generate_d`

<details>
<summary>Custom Column Names</summary>

```r
simulated <- binaryRL::rl_generate_d(
  data = data,                    # your data
  id = 18,                        # Subject ID
  eta = c(params[1], params[2]),  # RSTD parameters
  n_params = 2,                   # the number of free parameters
  n_trials = 288,                 # the number of total trials

  # column names
  sub = "Subject",
  L_choice = "LC",
  R_choice = "RC",
  L_reward = "LR",
  R_reward = "RR",
  time_line = c("Block", "Trial"),
  sub_choose = "Choose",
  var1 = "extra_Var1",
  var2 = "extra_Var2"
)

summary(simulated)

```

</details>  

<!---------------------------------------------------------->

You can also customize the `value function` and `action function`. The default function is applicable to three basic models. 

<details>
<summary>Custom Functions</summary>

```r
simulated <- binaryRL::rl_generate_d(
  data = data,                    # your data
  id = 18,                        # Subject ID
  eta = c(params[1], params[2]),  # RSTD parameters
  n_params = 2,                   # the number of free parameters
  n_trials = 288,                 # the number of total trials

  # column names
  sub = "Subject",
  L_choice = "LC",
  R_choice = "RC",
  L_reward = "LR",
  R_reward = "RR",
  time_line = c("Block", "Trial"),
  sub_choose = "Choose",
  var1 = "extra_Var1",
  var2 = "extra_Var2",

  # functions
  util_func = your_util_func,
  rate_func = your_rate_func,  
  expl_func = your_expl_func,
  prob_func = your_prob_func
)

summary(simulated)
```

</details>  

<!---------------------------------------------------------->

---

<!---------------------------------------------------------->
# How the Package Works

## Step 1: Value Function

**Value Function** independently updating the value associated with each stimulus.

- **Utility Function ($\gamma$)**: Some also refer to it as the _discount rate_ (for example, in the R package `ReinforcementLearning`), but I believe expressing it as people's subjective perception of objective rewards is more accurate. This is because the relationship between physical quantities and psychological quantities is not necessarily always a linear discount function; it could also be another type of power function relationship (Stevens' Power Law).   
  - If you agree the relationship between objective value and subjective value is linear, represented by the equation:

$$  
U(R) = \gamma \cdot R  
\quad \quad \Rightarrow \quad \quad
V_{n} = V_{n-1} + \eta \cdot (\gamma \cdot R_{n} - V_{n-1})  
$$  

- **Learning Rates ($\eta$)**: This parameter $\eta$ controls how quickly an agent updates its value estimates based on new information. The closer $\eta$ is to 1, the faster the learning rate.

$$  
V_{n} = V_{n-1} + \eta \cdot [U(R_{n}) - V_{n-1}]  
$$  

## Step 2: Action Function
**Action Function** reflecting how individuals make choices based on the value of the options.  

 - **Exploration Function ($\epsilon$)**: The parameter $\epsilon$ represents the probability of participants engaging in exploration (random choosing). In addition A threshold ensures participants always explore during the initial trials, after which the likelihood of exploration is determined by $\epsilon$..   

$$
P(x) =
\begin{cases} 
\epsilon, &  x = 1 \quad \text{(random choosing)} \\
1 - \epsilon, &  x = 0 \quad \text{(value-based choosing)}
\end{cases}
$$

 - **Soft-Max Function ($\tau$)**: The parameter $\tau$ represents people's sensitivity to value differences. The larger $\tau$, the more sensitive they are to the differences in value between the two options.

$$
P_{L} = \frac{1}{1 + e^{-(V_{L} - V_{R}) \cdot \tau}}
\quad \quad
P_{R} = \frac{1}{1 + e^{-(V_{R} - V_{L}) \cdot \tau}}
$$

<!---------------------------------------------------------->

## Step 3: Robot vs. Human Consistency
**Log Likelihood** representing how similar robot behavior is to human behavior

  $$
  LL = \sum B_{L} \times \log P_{L} + \sum B_{R} \times \log P_{R}
  $$   

  *NOTE:* $B_{L}$ and $B_{R}$ the option that the subject chooses. ($B_{L} = 1$: subject chooses the left option; $B_{R} = 1$: subject chooses the right option); $P_{L}$ and $P_{R}$ represent the probabilities of selecting the left or right option, as predicted by the reinforcement learning model.   

<!---------------------------------------------------------->

## Step 4: Simulated Data Generation
**Generate Simulated Data**: Given the **Value Function** and the **Action Selection Function**, along with the optimal parameters, simulate data.  

$$
binaryRL(\hat\lambda, \hat\gamma, \hat\eta, \hat\epsilon, \hat\tau) \quad \Rightarrow \quad Y \sim \text{data.frame}
$$

---

# Classic Models

The default function can run the three classic models here. Setting different parameters in `rl_run_m` means running different RL models.
<!---------------------------------------------------------->

## 1. TD Model ($\eta$)
> "The TD model is a standard temporal difference learning model (Barto, 1995; Sutton, 1988; Sutton and Barto, 1998)."  

**if only ONE $\eta$ is set as a free paramters, it represents the TD model.**

```r
# TD Model
binaryRL::rl_run_m(
  ...,
  eta = c(params[1]),              # free parameter: learning rate
  gamma = 1,                       # fixed parameter: utility, default as 1
  n_params = 1,                    # the number of free parameters
  ...
)
```

## 2. Risk-Sensitive TD Model ($\eta_{-}$, $\eta_{+}$)
> "In the risk-sensitive TD (RSTD) model, positive and negative prediction errors have asymmetric effects on learning (Mihatsch and Neuneier, 2002)."  

**If TWO $\eta$ are set as free parameters, it represents the RSTD model.**

```r
# RSTD Model
binaryRL::rl_run_m(
  ...,
  eta = c(params[1], params[2]),   # free parameter: learning rate
  gamma = 1,                       # fixed parameter: utility, default as 1
  n_params = 2,                    # the number of free parameters
  ...
)
```

## 3. Utility Model ($\eta$, $\gamma$)
> "The utility model is a TD learning model that incorporates nonlinear subjective utilities (Bernoulli, 1954)"

**If ONE $\eta$ and ONE $\gamma$ are set as free parameters, it represents the utility model.**

```r
# Utility Model
binaryRL::rl_run_m(
  ...,
  eta = c(params[1]),              # free parameter: learning rate
  gamma = c(params[2]),            # free parameter: utility
  n_params = 2,                    # the number of free parameters
  ...
)
```

<p align="center">
    <img src="./fig/rl_models.png" alt="RL Models" width="70%">
</p>

### References
Niv, Y., Edlund, J. A., Dayan, P., & O'Doherty, J. P. (2012). Neural prediction errors reveal a risk-sensitive reinforcement-learning process in the human brain. *Journal of Neuroscience, 32*(2), 551-562. https://doi.org/10.1523/JNEUROSCI.5498-10.2012

<!---------------------------------------------------------->

## Initial Value
In `rl_run_m`, there is an argument called initial_value. Considering that the initial value has a significant impact on the parameter estimation of the **learning rates ($\eta$)** When the initial value is not set (`initial_value = NA`), it is taken to be the reward received for that stimulus the first time.

> "Comparisons between the two learning rates generally revealed a positivity bias ($\alpha_{+} > \alpha_{-}$)"  
> "However, that on some occasions, studies failed to find a positivity bias or even reported a negativity bias ($\alpha_{+} < \alpha_{-}$)."  
> "Because Q-values initialization markedly affect learning rate and learning bias estimates."

```r
binaryRL::rl_run_m(
  ...,
  initial_value = NA,
  ...
)
```

### References
Palminteri, S., & Lebreton, M. (2022). The computational roots of positivity and confirmation biases in reinforcement learning. *Trends in Cognitive Sciences, 26*(7), 607-621. https://doi.org/10.1016/j.tics.2022.04.005

<!---------------------------------------------------------->
## Utility Function
The subjective value of objective rewards is a topic that requires discussion, as different scholars may have different perspectives. This can be traced back to the `Weber-Fechner Law`. In this model, you can customize your utility function. By default, I believe there is a linear relationship between subjective value and objective value.

$$
U(R) = \gamma \cdot R
\quad | \quad
U(R) = \gamma \cdot R^2
\quad | \quad
U(R) = R ^ \gamma
\quad | \quad
U(R) = log_\gamma R
$$

```r
func_gamma <- function(
  value, utility, reward, occurrence, var1, var2, gamma, lambda
){
  if (length(gamma) == 1) {
    gamma <- gamma
    utility <- reward * gamma
    # Custom your utility function
    # utility <- (reward ^ 2) * gamma
    # utility <- reward ^ gamma
    # utility <- log(reward, base = gamma)
    # ...
  }
  else {
    utility <- "ERROR" 
  }
  return(list(gamma, utility))
}
```

<!---------------------------------------------------------->

## $\epsilon$-Greedy
Participants in the experiment may not always choose based on the value of the options, but instead select randomly on some trials. This is known as $\epsilon$-greedy. (e.g., when epsilon = 0.1 (*default: NA*), it means that the participant has a 10% probability of randomly selecting an option and a 90% probability of choosing based on the currently learned value of the options.)

- In my opinion, I think that participants tend to randomly select options during the early stages of the experiment to estimate the value of each option. Therefore, I added an argument called `threshold`, which specifies the number of trials during which participants will make completely random choices. The default value is set to 20. **(if you disagree with my assumption, you can set `threshold = 0`)**. 

```r
binaryRL::rl_run_m(
  ...,
  threshold = 20,      
  epsilon = 0.1,
  ...
)
```

### References
Ganger, M., Duryea, E., & Hu, W. (2016). Double Sarsa and double expected Sarsa with shallow and deep learning. Journal of Data Analysis and Information Processing, 4(04), 159. https://doi.org/10.4236/jdaip.2016.44014

<!---------------------------------------------------------->

## Soft-Max Function
The closer $\tau$ is to 1 (*default: 0.5*), the more sensitive the subjects become to the values of the left and right options. In other words, even a slight difference in value will lead the subjects to choose the option with the higher value.  

If you add $\tau$ to your model as a extra free parameter, you will generally achieve better model fit. In fact, some articles have already incorporated the parameter in the softmax function into reinforcement learning models (e.g., Niv et al., 2012; Rosenbaum et al.,2022).

```r
# RSTD Model
binaryRL::rl_run_m(
  ...,
  eta = c(params[1], params[2]),   # free parameter: learning rate
  gamma = 1,                       # fixed parameter: utility, default as 1
  tau = c(params[3]),              # free paramters: sensitivity to value differences
  n_params = 3,                    # the number of free parameters
  ...
)
```

### References
Niv, Y., Edlund, J. A., Dayan, P., & O'Doherty, J. P. (2012). Neural prediction errors reveal a risk-sensitive reinforcement-learning process in the human brain. *Journal of Neuroscience, 32*(2), 551-562. https://doi.org/10.1523/JNEUROSCI.5498-10.2012  
Rosenbaum, G. M., Grassie, H. L., & Hartley, C. A. (2022). Valence biases in reinforcement learning shift across adolescence and modulate subsequent memory. *ELife, 11*, e64620. https://doi.org/10.7554/eLife.64620

<!---------------------------------------------------------->

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


