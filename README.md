# binaryRL
This package is designed to simplify the process of building **model-free** reinforcement learning models. It allows beginners to easily construct a model with just an `if-else` statement, making model creation more accessible.

Before using this package, please make sure you agree with this assumptions.

> This experiment employs a Two-Alternative Forced Choice (TAFC) paradigm. Learning for each stimulus is independent, meaning reward outcomes for one stimulus do not affect learning for other stimuli.

If you agree with this assumptions, I will introduce how to use this package.

<!---------------------------------------------------------->

[![GitHub release (latest by date)](https://img.shields.io/github/v/release/yuki-961004/binaryRL?logo=github&logoColor=white&color=white)](https://github.com/yuki-961004/binaryRL/releases/latest)
[![R-CMD-check](https://github.com/yuki-961004/binaryRL/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yuki-961004/binaryRL/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/yuki-961004/binaryRL/branch/main/graph/badge.svg)](https://app.codecov.io/gh/yuki-961004/binaryRL)
[![CRAN-Version](https://www.r-pkg.org/badges/version/binaryRL?color=%231E63B5)](https://CRAN.R-project.org/package=binaryRL)
[![CRAN-Downloads](https://cranlogs.r-pkg.org/badges/grand-total/binaryRL?color=%23FA812F)](https://CRAN.R-project.org/package=binaryRL)


## How to cite 
YuKi. (2025). binaryRL: Reinforcement Learning Tools for Two-Alternative Forced Choice Tasks. R package version 0.8.0. https://CRAN.R-project.org/package=binaryRL

Hu, M., & Liu, Z. (2025). binaryRL: A Package for Building Reinforcement Learning Models in R. *Journal*(7), 100-123. https://doi.org/

## Install and Load Pacakge
```r
# Install the stable version from CRAN  
install.packages("binaryRL")
# Install the latest version from GitHub
remotes::install_github("yuki-961004/binaryRL@*release")

library(binaryRL)
```

<pre>
                                      ╔══════════════════════════╗
                                      ║ ╔----------╗             ║
                                      ║ | ██████╗  |   ██╗       ║
 |     _)                             ║ | ██╔══██╗ |   ██║       ║
 __ \   |  __ \    _` |   __|  |   |  ║ | ██████╔╝ |   ██║       ║
 |   |  |  |   |  (   |  |     |   |  ║ | ██╔══██╗ |   ██║       ║
_.__/  _| _|  _| \__,_| _|    \__, |  ║ | ██║  ██║ |   ███████╗  ║
                              ____/   ║ | ╚═╝  ╚═╝ |   ╚══════╝  ║
                                      ║ ╚----------╝             ║
                                      ╚══════════════════════════╝
</pre>

<!---------------------------------------------------------->

## Algorithms for Parallel Data Fitting
While this R package is primarily designed for constructing reinforcement learning (RL) models (with `run_m` at its core), its flexibility extends further. 

The key functions, `rcv_d` and `fit_p`, provide a unified interface to seamlessly integrate a diverse range of optimization algorithms. Crucially, they offer a parallel solution for tasks like parameter optimization, parameter recovery, and model recovery.

This means you can leverage this package not only for building and fitting RL models, but also as a versatile algorithm library for fitting other **"black-box functions"** in parallel for each subject. This significantly reduces processing time, provided your function's parameters can be optimized independently for each subject.

**Base R Optimization**  
  - L-BFGS-B (from `stats::optim`)   

**Specialized External Optimization**
  - Simulated Annealing (`GenSA`)  
  - Genetic Algorithm (`GA`)  
  - Differential Evolution (`DEoptim`).   
  - Particle Swarm Optimization (`pso`)
  - Bayesian Optimization (`mlrMBO`)
  - Covariance Matrix Adapting Evolutionary Strategy (`cmaes`)

**Optimization Library**
  - Nonlinear Optimization (`nloptr`)

<br>

*NOTE:* If you want to use an algorithm other than `L-BFGS-B`, you must install the corresponding package. 

<!---------------------------------------------------------->

## Read your Raw Data

Our package includes a minimally processed version of a publicly available dataset from Mason et al. [(2024)](https://doi.org/10.3758/s13423-023-02415-x) as example data. This dataset represents a classic Two-Armed Bandit task, a typical example of a Two-Alternative Forced Choice (TAFC) paradigm.

```r
# An open data from Mason et. al. (2024) https://osf.io/hy3q4/
head(Mason_2024_Exp2)
```

| Subject | Block | Trial | L_choice | R_choice | L_reward | R_reward | Sub_Choose |
|---------|-------|-------|----------|----------|----------|----------|------------|
| 1       | 1     | 1     | A        | B        | 36       | 40       | A          |
| 1       | 1     | 2     | B        | A        | 0        | 36       | B          |
| 1       | 1     | 3     | C        | D        | -36      | -40      | C          |
| 1       | 1     | 4     | D        | C        | 0        | -36      | D          |
| ...     | ...   | ...   | ...      | ...      | ...      | ...      | ...        |

*NOTES*

1. Your dataset needs to include these **columns**.   
2. You can also add two **additional variables** as factors that the model needs to consider.

### References
Mason, A., Ludvig, E. A., Spetch, M. L., & Madan, C. R. (2024). Rare and extreme outcomes in risky choice. Psychonomic Bulletin & Review, 31(3), 1301-1308. https://doi.org/10.3758/s13423-023-02415-x
<!---------------------------------------------------------->

# Tutorial
*-* The development and usage workflow of this R package adheres to the **four** stages (ten rules) recommended by Wilson & Collins [(2019)](https://doi.org/10.7554/eLife.49547).  
*-* The **three** basic models built into this R package are referenced from Niv et al. [(2012)](https://doi.org/10.1523/JNEUROSCI.5498-10.2012).

<p align="center">
    <img src="./fig/rl_process.png" alt="RL Process" width="34.25%" style="display: inline;">
    <img src="./fig/rl_models.png" alt="RL Models" width="61%" style="display: inline;">
</p>


### References  
Wilson, R. C., & Collins, A. G. (2019). Ten simple rules for the computational modeling of behavioral data. *Elife*, 8, e49547. https://doi.org/10.7554/eLife.49547  
Niv, Y., Edlund, J. A., Dayan, P., & O'Doherty, J. P. (2012). Neural prediction errors reveal a risk-sensitive reinforcement-learning process in the human brain. *Journal of Neuroscience, 32*(2), 551-562. https://doi.org/10.1523/JNEUROSCI.5498-10.2012
<!---------------------------------------------------------->

## 1. Run Model

Create a function with **ONLY ONE** argument, `params`

```r
Model <- function(params){

  res <- binaryRL::run_m(
    data = data,
    id = id,
    mode = mode,
    n_trials = n_trials,
    n_params = n_params,
# ╔═══════════════════════════════════╗ #
# ║ You only need to modify this part ║ #
# ║ -------- free parameters -------- ║ #
    eta = c(params[1], params[2]),
    gamma = c(params[3], params[4]),
    epsilon = c(params[5], params[6]),
    tau = c(params[7], params[8]),
    lambda = c(params[9], params[...]),
# ║ -------- core functions --------- ║ #
    util_func = your_util_func,
    rate_func = your_rate_func,  
    expl_func = your_expl_func,
    prob_func = your_prob_func,
# ║ --------- column names ---------- ║ #
    sub = "Subject",
    time_line = c("Block", "Trial"),
    L_choice = "L_choice",
    R_choice = "R_choice",
    L_reward = "L_reward",
    R_reward = "R_reward",
    sub_choose = "Sub_Choose",
    var1 = "extra_Var1",
    var2 = "extra_Var2"
# ║ You only need to modify this part ║ #
# ╚═══════════════════════════════════╝ # 
  )

  assign(x = "binaryRL.res", value = res, envir = binaryRL.env)
  switch(mode, "fit" = -res$ll, "simulate" = res, "review" = res)
}
```

<!---------------------------------------------------------->

### Custom Functions

<!---------------------------------------------------------->

You can also customize all four functions to build your own RL model. 
- The default function is applicable to three basic models (`TD`, `RSTD`, `Utility`). 

<!---------------------------------------------------------->

<details>
<summary> -  Utility Function (γ)</summary>

```r
print(binaryRL::func_gamma)
```

```r
func_gamma <- function(
  # variables
  value, utility, reward, occurrence, var1, var2, 
  # parameters
  gamma, lambda
){
  if (length(gamma) == 1) {
    gamma <- gamma
    utility <- sign(reward) * (abs(reward) ^ gamma)
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
<summary> -  Learning Rate Function (η)</summary>

```r
print(binaryRL::func_eta)
```

```r
func_eta <- function (
  # variables
  value, utility, reward, occurrence, var1, var2, 
  # parameters
  eta, lambda
){
  if (length(eta) == 1) {
    eta <- as.numeric(eta)
  }
  else if (length(eta) > 1 & utility <  value) {
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
<summary> -  Exploration Function (ε)</summary>

```r
print(binaryRL::func_epsilon)
```

```r
func_epsilon <- function(
  # variables
  i, var1, var2, 
  # parameters
  threshold, epsilon, lambda
){
  if (i <= threshold) {
    try <- 1
  } 
  else if (i > threshold & is.na(epsilon)) {
    try <- 0
  } 
  else if (i > threshold & !(is.na(epsilon))){
    try <- sample(
      c(1, 0),
      prob = c(epsilon, 1 - epsilon),
      size = 1
    )
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
<summary> -  Soft-Max Function (τ)</summary>

```r
print(binaryRL::func_tau)
```

```r
func_tau <- function (
  # variables
  LR, try, L_value, R_value, var1, var2, 
  # parameters
  tau, lambda 
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


## Three Classic RL Models

These basic models are built into the package. You can use them using `binaryRL::TD.fit`, `binaryRL::RSTD.fit`, `binaryRL::Utility.fit`.

<!---------------------------------------------------------->

### 1. TD Model ($\eta$)

> "The TD model is a standard temporal difference learning model (Barto, 1995; Sutton, 1988; Sutton and Barto, 1998)."  

**if only ONE $\eta$ is set as a free paramters, it represents the TD model.**

```r
# TD Model
binaryRL::run_m(
  ...,
  eta = c(params[1]),              # free parameter: learning rate
  gamma = 1,                       # fixed parameter: utility, default as 1
  tau = c(params[2]),
  ...
)
```

### 2. Risk-Sensitive TD Model ($\eta_{-}$, $\eta_{+}$)

> "In the risk-sensitive TD (RSTD) model, positive and negative prediction errors have asymmetric effects on learning (Mihatsch and Neuneier, 2002)."  

**If TWO $\eta$ are set as free parameters, it represents the RSTD model.**

```r
# RSTD Model
binaryRL::run_m(
  ...,
  eta = c(params[1], params[2]),   # free parameter: learning rate
  gamma = 1,                       # fixed parameter: utility, default as 1
  tau = c(params[3]),
  ...
)
```

### 3. Utility Model ($\eta$, $\gamma$)

> "The utility model is a TD learning model that incorporates nonlinear subjective utilities (Bernoulli, 1954)"

**If ONE $\eta$ and ONE $\gamma$ are set as free parameters, it represents the utility model.**

```r
# Utility Model
binaryRL::run_m(
  ...,
  eta = c(params[1]),              # free parameter: learning rate
  gamma = c(params[2]),            # free parameter: utility
  tau = c(params[3]),
  ...
)
```

<!---------------------------------------------------------->

## 2. Parameter and Model Recovery

Here, using the publicly available data from Mason et al. [(2024)](https://doi.org/10.3758/s13423-023-02415-x), we demonstrate how to perform parameter recovery and model recovery following the method suggested by Wilson & Collins [(2019)](https://doi.org/10.7554/eLife.49547).  

  1. Notably, Wilson & Collins [(2019)](https://doi.org/10.7554/eLife.49547) recommend increasing the softmax parameter $\tau$ by 1 during model recovery, as this can help reduce the amount of noise in behavior.  
  2. Additionally, different algorithms and varying number of iterations can also influence the results of both parameter recovery and model recovery. You should adjust these settings based on your specific needs and circumstances.   

<!---------------------------------------------------------->

```r
recovery <- binaryRL::rcv_d(
  data = binaryRL::Mason_2024_Exp2,
  id = 1,
  n_trials = 360,
  #funcs = c("my_util_func", "my_rate_func", "my_expl_func", "my_prob_func"),
  model_names = c("TD", "RSTD", "Utility"),
  simulate_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
  simulate_lower = list(c(0, 1), c(0, 0, 1), c(0, 0, 1)),
  simulate_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
  fit_models = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
  fit_lower = list(c(0, 1), c(0, 0, 1), c(0, 0, 1)),
  fit_upper = list(c(1, 5), c(1, 1, 5), c(1, 1, 5)),
  initial_params = NA,
  initial_size = 50,
  seed = 123,
  iteration_s = 50,
  iteration_f = 50,
  nc = 1,
  # Base R Optimization  
  algorithm = "L-BFGS-B"   # Gradient-Based (stats)

  # Specialized External Optimization
  #algorithm = "GenSA"     # Simulated Annealing (GenSA)
  #algorithm = "GA"        # Genetic Algorithm (GA)
  #algorithm = "DEoptim"   # Differential Evolution (DEoptim)
  #algorithm = "PSO"       # Particle Swarm Optimization (pso)
  #algorithm = "Bayesian"  # Bayesian Optimization (mlrMBO)
  #algorithm = "CMA-ES"    # Covariance Matrix Adapting (`cmaes`)

  # Optimization Library (nloptr)
  #algorithm = c("NLOPT_GN_MLSL", "NLOPT_LN_BOBYQA")
)

result <- dplyr::bind_rows(recovery) %>%
  dplyr::select(simulate_model, fit_model, iteration, everything())

write.csv(result, file = "../OUTPUT/result_recovery.csv", row.names = FALSE)
```

<pre>
Simulating Model: TD | Fitting Model: TD

  |================                                                   |  10%
</pre>
  
Check the Example Result: [result_recovery.csv](./demo/OUTPUT/result_recovery.csv)


<!---------------------------------------------------------->

`binaryRL::rcv_d()` is the for-loop version of `binaryRL::simulate_list()` and `binaryRL::recovery_data()`, allowing you to generate simulated data using all models at once and then fit all models to the simulated data.
We also encourage advanced users to use `binaryRL::simulate_list()` and `binaryRL::recovery_data()` separately for parameter recovery and model recovery. Below is an example code.

<details>
<summary>binaryRL::simulate_list()</summary>

```r
list_simulated <- binaryRL::simulate_list(
  data = binaryRL::Mason_2024_Exp2,
  id = 1,
  obj_func = binaryRL::RSTD,
  n_params = 3, 
  n_trials = 288,
  lower = c(0, 0, 1),
  upper = c(1, 1, 1),
  seed = 1,
  iteration = 30
)
```

</details>

<!---------------------------------------------------------->

<details>
<summary>binaryRL::recovery_data()</summary>

```r
df_recovery <- binaryRL::recovery_data(
  list = list_simulated,
  id = 1,
  fit_model = binaryRL::RSTD,
  model_name = "RSTD",
  n_params = 3,
  n_trials = 360,
  lower = c(0, 0, 1),
  upper = c(1, 1, 5),
  iteration = 30,
  nc = 1,
  algorithm = "L-BFGS-B" 
)
```

</details>

<!---------------------------------------------------------->

### Parameter Recovery [[Example Code]](./demo/CODE/test_2_rcv_d.Rmd) 

> "Before reading too much into the best-fitting parameter values, $\theta_{m}^{MLE}$,  it is important to check whether the fitting procedure gives meaningful parameter values in the best case scenario, -that is, when fitting fake data where the 'true' parameter values are known (Nilsson et al., 2011). Such a procedure is known as 'Parameter Recovery', and is a crucial part of any model-based analysis."

<!---------------------------------------------------------->

<p align="center">
    <img src="./demo/FIGURE/param_rcv/lower_0/1_TD_eta.png" alt="RL Models" width="45%" style="display: inline;">
    <img src="./demo/FIGURE/param_rcv/lower_0/1_TD_tau.png" alt="RL Models" width="45%" style="display: inline;">
</p>

The value of the softmax parameter $\tau$ affects the recovery of other parameters in the model. Under the assumption that $\tau \sim \text{Exp}(1)$, adding 1 to all $\tau$ values improves the recovery of the learning rate  $\eta$, but decreases the recovery accuracy of  $\tau$ itself.


<p align="center">
    <img src="./demo/FIGURE/param_rcv/lower_1/1_TD_eta.png" alt="RL Models" width="45%" style="display: inline;">
    <img src="./demo/FIGURE/param_rcv/lower_1/1_TD_tau.png" alt="RL Models" width="45%" style="display: inline;">
</p>

<!---------------------------------------------------------->

### Model Recovery [[Example Code]](./demo/CODE/test_2_rcv_d.Rmd)  

> "More specifically, model recovery involves simulating data from all models (with a range of parameter values carefully selected as in the case of parameter recovery) and then fitting that data with all models to determine the extent to which fake data generated from model A is best fit by model A as opposed to model B. This process can be summarized in a confusion matrix that quantifies the probability that each model is the best fit to data generated from the other models, that is, *p*(*fit model* = B | *simulated model* = A)."

<!---------------------------------------------------------->

<p align="center">
    <img src="./demo/FIGURE/model_rcv/lower_0/matrix_confusion.png" alt="RL Models" width="45%" style="display: inline;">
    <img src="./demo/FIGURE/model_rcv/lower_0/matrix_inversion.png" alt="RL Models" width="45%" style="display: inline;">
</p>

> "In panel B, all of the softmax parameters $b$ and $b_{c}$ were increased by 1. This has the effect of reducing the amount of noise in the behavior, which makes the models more easily identifiable and the corresponding confusion matrix more diagonal."

<p align="center">
    <img src="./demo/FIGURE/model_rcv/lower_1/matrix_confusion.png" alt="RL Models" width="45%" style="display: inline;">
    <img src="./demo/FIGURE/model_rcv/lower_1/matrix_inversion.png" alt="RL Models" width="45%" style="display: inline;">
</p>

<!---------------------------------------------------------->

## 3. Fit Parameters
Following the recommendations of Wilson & Collins [(2019)](https://doi.org/10.7554/eLife.49547), a model is only worth fitting to real data if it demonstrates good performance in the parameter recovery and model recovery.

> Once all the previous steps have been completed, you can finally move on to modeling your empirical data. 

> If the model-independent analyses do not show evidence of the expected results, there is almost no point in fitting the model. Instead, you should go back to the beginning, either re-thinking the computational models if the analyses show interesting patterns of behavior, or re-thinking the experimental design or even the scientific question you are trying to answer.

```r
comparison <- binaryRL::fit_p(
  data = binaryRL::Mason_2024_Exp2,
  n_trials = 360,
  id = unique(binaryRL::Mason_2024_Exp2$Subject),
  #funcs = c("my_util_func", "my_rate_func", "my_expl_func", "my_prob_func"),
  fit_model = list(binaryRL::TD, binaryRL::RSTD, binaryRL::Utility),
  model_name = c("TD", "RSTD", "Utility"),
  lower = list(c(0, 0), c(0, 0, 0), c(0, 0, 0)),
  upper = list(c(1, 10), c(1, 1, 10), c(1, 1, 10)),
  iteration = 10,
  seed = 123,
  nc = 1,
  # Base R Optimization  
  algorithm = "L-BFGS-B"   # Gradient-Based (stats)

  # Specialized External Optimization
  #algorithm = "GenSA"     # Simulated Annealing (GenSA)
  #algorithm = "GA"        # Genetic Algorithm (GA)
  #algorithm = "DEoptim"   # Differential Evolution (DEoptim)
  #algorithm = "PSO"       # Particle Swarm Optimization (pso)
  #algorithm = "Bayesian"  # Bayesian Optimization (mlrMBO)
  #algorithm = "CMA-ES"    # Covariance Matrix Adapting (`cmaes`)

  # Optimization Library (nloptr)
  #algorithm = c("NLOPT_GN_MLSL", "NLOPT_LN_BOBYQA")
)

result <- dplyr::bind_rows(comparison)

write.csv(result, "../OUTPUT/result_comparison.csv", row.names = FALSE)
```  

<pre>
Fitting Model: TD

  |===                                                                |   5%
</pre>

Check the Example Result: [result_comparison.csv](./demo/OUTPUT/result_comparison.csv)

<!---------------------------------------------------------->

`binaryRL::fit_p()` is the for-loop version of `binaryRL::optimize_para()`, allowing you to fit all models to all subjects at once.   
If you prefer to fit a single model to a single subject at a time or want to speed up fitting multiple subjects using parallel computing, consider using `binaryRL::optimize_para()`.   
Below is an example of how to use it. We encourage advanced users to take advantage of this function.

<!---------------------------------------------------------->

<details>
<summary>binaryRL::optimize_para()</summary>

```r
binaryRL.res <- binaryRL::optimize_para(
  data = Mason_2024_Exp2,
  id = 1,
  n_params = 3,
  n_trials = 360,
  obj_func = binaryRL::RSTD,
  lower = c(0, 0, 0),
  upper = c(1, 1, 10),
  iteration = 30,
  seed = 123,
  algorithm = "L-BFGS-B"
)

summary(binaryRL.res)
```

<pre>
#> Results of the Reinforcement Learning Model:
#> 
#> Parameters:
#>    λ:  NA 
#>    γ:  1 
#>    η:  0.001 0 
#>    ε:  NA 
#>    τ:  0.029 
#> 
#> Model Fit:
#>    Accuracy:  56.39 %
#>    LogL:  -243.69 
#>    AIC:  493.38 
#>    BIC:  505.04 
</pre>

</details>

## Model Comparison

Using the result file generated by `binaryRL::fit_p`, you can compare models and select the best one.

<!---------------------------------------------------------->
### Visualizing Model Comparison [[Example Code]](./demo/CODE/test_3_fit_p.Rmd)

<p align="center">
    <img src="./demo/FIGURE/model_comparison(abs).png" alt="RL Models" width="45%" style="display: inline;">
    <img src="./demo/FIGURE/model_comparison(rel).png" alt="RL Models" width="45%" style="display: inline;">
</p>


<!---------------------------------------------------------->

## 4. Experimental Effect
Users can use the simple code snippet below to load the model fitting results, retrieve the best-fitting parameters, and feed them back into the model to reproduce the behavioral data generated by each reinforcement learning model.

```r
list <- list()

list[[1]] <- dplyr::bind_rows(
  binaryRL::rev_e(
    data = binaryRL::Mason_2024_Exp2,
    result = read.csv("../OUTPUT/result_comparison.csv"), 
    model = binaryRL::TD,
    model_name = "TD", 
    param_prefix = "param_",
    n_trials = 360
  )
)

list[[2]] <- dplyr::bind_rows(
  binaryRL::rev_e(
    data = binaryRL::Mason_2024_Exp2,
    result = read.csv("../OUTPUT/result_comparison.csv"), 
    model = binaryRL::RSTD,
    model_name = "RSTD", 
    param_prefix = "param_",
    n_trials = 360
  )
)

list[[3]] <- dplyr::bind_rows(
  binaryRL::rev_e(
    data = binaryRL::Mason_2024_Exp2,
    result = read.csv("../OUTPUT/result_comparison.csv"), 
    model = binaryRL::Utility,
    param_prefix = "param_",
    model_name = "Utility", 
    n_trials = 360
  )
)
```

<!---------------------------------------------------------->

### Visualizing Experimental Effect [[Example Code]](./demo/CODE/test_4_rev_e.Rmd)

<p align="center">
    <img src="./demo/FIGURE/Exp_Effect(Frame).png" alt="RL Models" width="45%" style="display: inline;">
    <img src="./demo/FIGURE/Exp_Effect(Block).png" alt="RL Models" width="45%" style="display: inline;">
</p>

<!---------------------------------------------------------->

---

# Other Arguments
## Paralell
```r
binaryRL::fit_p(
  ...,
  funcs = c("my_util_func", "my_rate_func", "my_expl_func", "my_prob_func"),
  nc = 4,
  ...
)

binaryRL::rcv_d(
  ...,
  funcs = c("my_util_func", "my_rate_func", "my_expl_func", "my_prob_func"),
  nc = 4,
  ...
)
```
Both `rcv_d` and `fit_p` support parallel computation, meaning they can fit multiple participants' datasets simultaneously as long as `nc > 1`.  
Since parallel execution runs in a separate environment, if you have customized any of the four core functions, you must explicitly pass the function names to `binaryRL` via the `funcs` argument.

## Initial Value
In `run_m`, there is an argument called `initial_value`. Considering that the initial value has a significant impact on the parameter estimation of the **learning rates ($\eta$)** When the initial value is not set (`initial_value = NA`), it is taken to be the reward received for that stimulus the first time.

> "Comparisons between the two learning rates generally revealed a positivity bias ($\alpha_{+} > \alpha_{-}$)"  
> "However, that on some occasions, studies failed to find a positivity bias or even reported a negativity bias ($\alpha_{+} < \alpha_{-}$)."  
> "Because Q-values initialization markedly affect learning rate and learning bias estimates."

```r
binaryRL::run_m(
  ...,
  initial_value = NA,
  ...
)
```

### References
Palminteri, S., & Lebreton, M. (2022). The computational roots of positivity and confirmation biases in reinforcement learning. *Trends in Cognitive Sciences, 26*(7), 607-621. https://doi.org/10.1016/j.tics.2022.04.005

<!---------------------------------------------------------->
## Utility Function
The subjective value of objective rewards is a topic that requires discussion, as different scholars may have different perspectives. This can be traced back to the `Stevens's Power Law`. In this model, you can customize your utility function. By default, I use a power function based on Stevens' power law to model the relationship between subjective and objective value.

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
    utility <- sign(reward) * (abs(reward) ^ gamma)
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

## Epsilon-Greedy
Participants in the experiment may not always choose based on the value of the options, but instead select randomly on some trials. This is known as $\epsilon$-greedy. (e.g., when epsilon = 0.1 (*default: NA*), it means that the participant has a 10% probability of randomly selecting an option and a 90% probability of choosing based on the currently learned value of the options.)

- In my opinion, I think that participants tend to randomly select options during the early stages of the experiment to estimate the value of each option. Therefore, I added an argument called `threshold`, which specifies the number of trials during which participants will make completely random choices. The default value is set to 1.

```r
binaryRL::run_m(
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
During the recovery process, the last element of both `simulate_lower` and `simulate_upper` corresponds to the $\tau$ parameter used in the softmax function.

- `simulate_lower` represents a fixed positive increment applied to all $\tau$ values.   
if this value is set to 1, it means that 1 is added to every $\tau$ during simulation.

- `simulate_upper` specifies the rate parameter of an exponential distribution from which $\tau$ is sampled.  
if this value is 1, then $\tau$ is drawn from an exponential distribution with a rate of 10, i.e., $\tau \sim \text{Exp}(1)$.


```r
binaryRL::rcv_d(
  ...
  simulate_lower = list(c(0, 1), c(0, 0, 1), c(0, 0, 1)),
  simulate_upper = list(c(1, 1), c(1, 1, 1), c(1, 1, 1)),
  ...
)
```

### References  
Wilson, R. C., & Collins, A. G. (2019). Ten simple rules for the computational modeling of behavioral data. *Elife*, 8, e49547. https://doi.org/10.7554/eLife.49547

<!---------------------------------------------------------->

## Model Fit

LL represents the similarity between the model's choices and human choices. The larger this value, the better the model.

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

<!---------------------------------------------------------->

# Functions

<!---------------------------------------------------------->

## Value Function

**Value Function** independently updating the value associated with each stimulus.

- **Utility Function ($\gamma$)**: Some also refer to it as the _discount rate_ (for example, in the R package `ReinforcementLearning`), but I believe expressing it as people's subjective perception of objective rewards is more accurate. This is because the relationship between physical quantities and psychological quantities is not necessarily always a linear discount function; it could also be another type of power function relationship (Stevens' Power Law).  

$$  
U(R) = R^{\gamma}  
\quad \quad \Rightarrow \quad \quad
V_{n} = V_{n-1} + \eta \cdot (R_{n}^{\gamma} - V_{n-1})  
$$  

- **Learning Rates ($\eta$)**: This parameter $\eta$ controls how quickly an agent updates its value estimates based on new information. The closer $\eta$ is to 1, the faster the learning rate.

$$  
V_{n} = V_{n-1} + \eta \cdot [U(R_{n}) - V_{n-1}]  
$$  

<!---------------------------------------------------------->

## Action Function
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

## Loss Function
**Loss Function** quantifies the error between predicted and actual values in a machine learning model. It guides the model's training by indicating how well it's performing.  

- **Log Likelihood Function** representing how similar robot behavior is to human behavior

$$
LL = \sum B_{L} \times \log P_{L} + \sum B_{R} \times \log P_{R}
$$   

*NOTE:* $B_{L}$ and $B_{R}$ the option that the subject chooses. ($B_{L} = 1$: subject chooses the left option; $B_{R} = 1$: subject chooses the right option); $P_{L}$ and $P_{R}$ represent the probabilities of selecting the left or right option, as predicted by the reinforcement learning model.   

<!---------------------------------------------------------->
