# binaryRL
This package is designed to simplify the process of building reinforcement learning models. It allows beginners to easily construct a model with just an `if-else` statement, making model creation more accessible.

Before using this package, please make sure you agree with these assumptions.

1. The paradigm is a binary choice decision task.
2. When decision-makers encounter a new stimulus, they will try it once. 
3. Learning for different stimuli is independent, meaning rewards for one stimulus do not influence the learning process for others.

If you agree with these three points, I will introduce the process of my package.

<!---------------------------------------------------------->

## Step 1: Value Function

**Value Function** independently updating the value associated with each stimulus.

- **Learning Rates ($\eta$)**: This parameter $\eta$ controls how quickly an agent updates its value estimates based on new information. The closer $\eta$ is to 1, the faster the learning rate.

$$  
V_{n} = V_{n-1} + \eta \cdot [U(R_{n}) - V_{n-1}]  
$$  

- **Utility Function ($\gamma$)**: Some also refer to it as the _discount rate_ (for example, in the R package `ReinforcementLearning`), but I believe expressing it as people's subjective perception of objective rewards is more accurate. This is because the relationship between physical quantities and psychological quantities is not necessarily always a linear discount function; it could also be another type of power function relationship (Stevens' Power Law).   
  - If you believe the relationship between objective value and subjective value is linear, represented by the equation:

$$  
U(R) = \gamma \cdot R  
\quad \quad \Rightarrow \quad \quad
V_{n} = V_{n-1} + \eta \cdot (\gamma \cdot R_{n} - V_{n-1})  
$$  

<!---------------------------------------------------------->

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

<!---------------------------------------------------------->

# How to cite 
Hu, M., & L, Z. (2025). binaryRL: A Package for Building Reinforcement Learning Models in R. *Journal*(7), 100-123. https://doi.org/


# Install
```{r}
devtools::install_github("yuki-961004/binaryRL") 
```

# Examples
## Load Pacakge
```r
library(binaryRL)
```

<!---------------------------------------------------------->

## Read your Raw Data
```r
data <- [your_data]
```
Make sure the global environment contains the raw data.   
Your dataset needs to include the following columns.   
`Block` and `Trial` columns are not mandatory, but there must be a column that represents the sequence of the experiment.
You can also add two additional variables as factors that the model needs to consider.

| Subject | Block | Trial | L_choice | R_choice | Choose | Reward | var1 | var2 |
|---------|-------|-------|----------|----------|--------|--------|------|------|
| 1       | 1     | 1     | A        | B        | A      | 5      |...   |...   |
| 1       | 1     | 2     | A        | B        | B      | 3      |...   |...   |
| 2       | 2     | 1     | X        | Y        | X      | 4      |...   |...   |
| 2       | 2     | 2     | X        | Y        | Y      | 2      |...   |...   | 
| ...     | ...   | ...   | ...      | ...      | ...    | ...    |...   |...   |

<!---------------------------------------------------------->

## Creat a Object Function for Algorithm Packages
Create a function that contains only the `params` argument.   
  
If you have already created your `value function` and `action function`, then here you only need to fill in the `[column names]` from your dataset into the corresponding arguments.   

```r
> sub = "Subject"
> choose = "Choose"
> reward = "Reward"
> time_line = c("Block", "Trial")
> ...
```

Most importantly, replace the `function` with your custom function. Alternatively, you can just use the default function, which can run the three basic models.

```r
> util_func = your_util_func
> rate_func = your_rate_func  
> expl_func = your_expl_func
> prob_func = your_prob_func
 ```

<!---------------------------------------------------------->

### Example Function

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

### Object Function
```r
obj_func <- function(params){
  res <- binaryRL::rl_run_m(
    data = data,
    id = 18,
    eta = c(params[1], params[2]),
    tau = c(params[3]),
    n_params = 3,
    n_trials = 288
  )

  binaryRL_res <<- res
  
  invisible(-res$ll)
}
```

<!---------------------------------------------------------->

### Example Algorithms
There are several methods available for estimating the optimal parameters based on likelihood values. In this example, I will demonstrate four methods: the "L-BFGS-B" algorithm (`stats::optim`), a gradient-based method; `GenSA`, a package for Simulated Annealing; `GA`, a package for Genetic Algorithms; and `DEoptim`, a package for Differential Evolution.

The first two methods, L-BFGS-B and GenSA, are single-threaded algorithms, while the latter two, GA and DEoptim, are multi-threaded algorithms. Among them, DEoptim has the shortest runtime and produces the smallest value of -logL.

<!---------------------------------------------------------->

<details>
<summary>L-BFGS-B (stats::optim)</summary>

```r
library(stats)

set.seed(123)
gb_result <- stats::optim(
  par = c(0.5, 0.5, 0.5),
  method = "L-BFGS-B",
  fn = obj_func,
  lower = c(0, 0, 0),
  upper = c(1, 1, 1),
  control = list(
    maxit = 10
  )
)
```

</details>

<!---------------------------------------------------------->

<details>
<summary>Simulated Annealing (GenSA::GenSA)</summary>

```r
library(GenSA)

sa_result <- GenSA::GenSA(
  fn = obj_func,
  lower = c(0, 0, 0),
  upper = c(1, 1, 1),
  control = list(
    maxit = 10,
    seed = 123
  )
)
```

</details>

<!---------------------------------------------------------->

<details>
<summary>Genetic Algorithm (GA::ga)</summary>

```r
library(GA)

ga_result <- GA::ga(
  type = "real-valued",
  fitness = function(x) obj_func(x),
  lower = c(0, 0, 0),
  upper = c(1, 1, 1),
  maxiter = 10,                     
  parallel = TRUE,          
  seed = 123                
)
```

</details>

<!---------------------------------------------------------->

<details>
<summary>Differential Evolution (DEoptim::DEoptim)</summary>

```r
library(DEoptim)

de_result <- DEoptim::DEoptim(
  fn = obj_func,
  lower = c(0, 0, 0),
  upper = c(1, 1, 1),
  control = DEoptim::DEoptim.control(
    itermax = 10,
    parallelType = c("parallel"),
    packages = c("binaryRL"),
    parVar = c("data")
  )
)
```

</details>

<!---------------------------------------------------------->

## Output
```r
obj_func(params = as.vector(result$params))
summary(binaryRL_res)
```

```r
#> Results of the Reinforcement Learning Model:
#> 
#> Parameters:
#>    λ:  NA  
#>    γ:  1 
#>    η:  0.016 0.446 
#>    ε:  NA 
#>    τ:  0.056

#> Model Fit:
#>    Accuracy:  82.64 %
#>    LogL:  -115.30 
#>    AIC:  236.60 
#>    BIC:  247.59 
```

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
  eta = c(0.016, 0.446),
  tau = c(0.056),
  n_params = 3, 
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
#>    η:  0.016 0.446 
#>    ε:  NA 
#>    τ:  0.056

#> Model Fit:
#>    Accuracy:  73.26 %
#>    LogL:  -197.80 
#>    AIC:  401.60  
#>    BIC:  412.59 
```

<!---------------------------------------------------------->

---

# Classic Models

<!---------------------------------------------------------->

## 1. TD Model ($\eta$, $\tau$)
> "The TD model is a standard temporal difference learning model (Barto, 1995; Sutton, 1988; Sutton and Barto, 1998)."  
## 2. Risk-Sensitive TD Model ($\eta_{-}$, $\eta_{+}$, $\tau$)
> "In the risk-sensitive TD (RSTD) model, positive and negative prediction errors have asymmetric effects on learning (Mihatsch and Neuneier, 2002)."  
## 3. Utility Model ($\eta$, $\gamma$, $\tau$)
> "The utility model is a TD learning model that incorporates nonlinear subjective utilities (Bernoulli, 1954)"

<p align="center">
    <img src="./fig/rl_models.png" alt="RL Models" width="70%">
</p>

### References
Niv, Y., Edlund, J. A., Dayan, P., & O'Doherty, J. P. (2012). Neural prediction errors reveal a risk-sensitive reinforcement-learning process in the human brain. *Journal of Neuroscience, 32*(2), 551-562. https://doi.org/10.1523/JNEUROSCI.5498-10.2012

<!---------------------------------------------------------->

## Initial Value

> "Comparisons between the two learning rates generally revealed a positivity bias ($\alpha_{+} > \alpha_{-}$)"  
> "However, that on some occasions, studies failed to find a positivity bias or even reported a negativity bias ($\alpha_{+} < \alpha_{-}$)."  
> "Because Q-values initialization markedly affect learning rate and learning bias estimates."

### References
Palminteri, S., & Lebreton, M. (2022). The computational roots of positivity and confirmation biases in reinforcement learning. *Trends in Cognitive Sciences, 26*(7), 607-621. https://doi.org/10.1016/j.tics.2022.04.005

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

<!---------------------------------------------------------->

---

# My understanding
In my understanding, the value function in reinforcement learning for a two-alternative decision task can be written as:

$$
V_{n} = V_{n-1} + \eta \cdot [U(R_{n}) - V_{n-1}]
$$

- The `TD model` only consider **learning rates ($\eta$)** as a free parameter.   
- The `Risk-Sensitive TD model` is based on `TD model` and assumes that the **learning rates ($\eta$)** are different for gains and losses.
- The `Utility model` introduces a **utility function ($\gamma$)** for rewards based on this foundation. 

## Utility Function
- I assume that there is a linear relationship between subjective value and objective value ($U(R) = \gamma \cdot R$). In fact, it may be in other forms. 

## Initial Value
- Considering that the initial value has a significant impact on the parameter estimation of the **learning rates ($\eta$)** When the initial value is not set (`initial_value = NA`), it is taken to be the reward received for that stimulus the first time.

## Exploration Function
- Participants will always make a choice when encountering a new stimulus. Additionally, if a threshold is set, participants will make random choices until the specified number of trials is reached.
