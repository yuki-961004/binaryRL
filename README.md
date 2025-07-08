# binaryRL <a href="https://yuki-961004.github.io/binaryRL/"><img src="./fig/logo.png" alt="LOGO" align="right" width="120"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/yuki-961004/binaryRL/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yuki-961004/binaryRL/actions/workflows/R-CMD-check.yaml)
[![Code Coverage](https://codecov.io/gh/yuki-961004/binaryRL/graph/badge.svg)](https://app.codecov.io/gh/yuki-961004/binaryRL)
[![CRAN-Version](https://www.r-pkg.org/badges/version/binaryRL?color=%231E63B5)](https://CRAN.R-project.org/package=binaryRL)
[![CRAN-Downloads](https://cranlogs.r-pkg.org/badges/grand-total/binaryRL?color=%23FA812F)](https://CRAN.R-project.org/package=binaryRL)

<!-- badges: end -->

## Overview
This package is designed to help users build the Rescorla-Wagner Model for Two-Alternative Forced Choice (TAFC) tasks, which is the simplest reinforcement learning model(the multi-armed bandit), where the reward for each action is received immediately, and each choice can be considered an independent and identically distributed (i.i.d.) probabilistic event. Beginners can define models using simple `if-else` logic, making model construction more accessible.

* [Step 1](./articles/binaryRL.html#id_1-run-model): Build Reinforcement Learning Models `run_m`
* [Step 2](./articles/binaryRL.html#id_2-recovery): Parameter and Model Recovery `rcv_d`
* [Step 3](./articles/binaryRL.html#id_3-fit-real-data): Fit Real Data `fit_p`
* [Step 4](./articles/binaryRL.html#id_4-replay-the-experiment): Replay the Experiment `rpl_e` 

<!---------------------------------------------------------->

## How to cite 
YuKi. (2025). binaryRL: Reinforcement Learning Tools for Two-Alternative Forced Choice Tasks. R package version 0.8.10. https://CRAN.R-project.org/package=binaryRL

Hu, M., & Liu, Z. (2025). binaryRL: A Package for Building Reinforcement Learning Models in R. *Journal(7)*, 100-123. https://doi.org/

## Installation
```r
# Install the stable version from CRAN  
install.packages("binaryRL")
# Install the latest version from GitHub
remotes::install_github("yuki-961004/binaryRL@*release")

# Load package
library(binaryRL)
# Obtain help document
?binaryRL
```

<pre>
                                      ╔═════════════════════════╗
                                      ║ ╔----------╗            ║
                                      ║ | ██████╗  |  ██╗       ║
 |     _)                             ║ | ██╔══██╗ |  ██║       ║
 __ \   |  __ \    _` |   __|  |   |  ║ | ██████╔╝ |  ██║       ║
 |   |  |  |   |  (   |  |     |   |  ║ | ██╔══██╗ |  ██║       ║
_.__/  _| _|  _| \__,_| _|    \__, |  ║ | ██║  ██║ |  ███████╗  ║
                              ____/   ║ | ╚═╝  ╚═╝ |  ╚══════╝  ║
                                      ║ ╚----------╝            ║
                                      ╚═════════════════════════╝
</pre>

<!---------------------------------------------------------->

# Tutorial
*-* The development and usage workflow of this R package adheres to the **four** stages (ten rules) recommended by Wilson & Collins [(2019)](https://doi.org/10.7554/eLife.49547).  
*-* The **three** basic models built into this R package are referenced from Niv et al. [(2012)](https://doi.org/10.1523/JNEUROSCI.5498-10.2012).

<p align="center">
    <img src="./fig/rl_process.png" alt="RL Process" width="34.25%" style="display: inline;">
    <img src="./fig/rl_models.png" alt="RL Models" width="61%" style="display: inline;">
</p>

**Reference**  
Wilson, R. C., & Collins, A. G. (2019). Ten simple rules for the computational modeling of behavioral data. *Elife, 8*, e49547. https://doi.org/10.7554/eLife.49547  
Niv, Y., Edlund, J. A., Dayan, P., & O'Doherty, J. P. (2012). Neural prediction errors reveal a risk-sensitive reinforcement-learning process in the human brain. *Journal of Neuroscience, 32*(2), 551-562. https://doi.org/10.1523/JNEUROSCI.5498-10.2012

<!---------------------------------------------------------->

## [Example Data](./demo/DATA/binaryRL_data.html)

Our package includes a minimally processed version of a publicly available dataset from Mason et al. [(2024)](https://doi.org/10.3758/s13423-023-02415-x) as example data. This dataset represents a classic Two-Armed Bandit task, a typical example of a Two-Alternative Forced Choice (TAFC) paradigm.

```r
# An open data from Mason et. al. (2024) https://osf.io/hy3q4/
head(binaryRL::Mason_2024_Exp2)
```

| Subject | Block | Trial | L_choice | R_choice | L_reward | R_reward | Sub_Choose | - |
|---------|-------|-------|----------|----------|----------|----------|------------|---|
| 1       | 1     | 1     | A        | B        | 36       | 40       | A          |...|
| 1       | 1     | 2     | B        | A        | 0        | 36       | B          |...|
| 1       | 1     | 3     | C        | D        | -36      |-40       | C          |...|
| 1       | 1     | 4     | D        | C        | 0        |-36       | D          |...|
| ...     | ...   | ...   | ...      | ...      | ...      | ...      | ...        |...|

**Reference**  
Mason, A., Ludvig, E. A., Spetch, M. L., & Madan, C. R. (2024). Rare and extreme outcomes in risky choice. *Psychonomic Bulletin & Review, 31*(3), 1301-1308. https://doi.org/10.3758/s13423-023-02415-x


<pre>
                                      .
                                        .
                                    . ;.
                                      .;
                                      ;;.
                                    ;.;;
                                    ;;;;.
                                    ;;;;;
                                    ;;;;;
                                  ..;;;;;...
                                    ':::::'
                                      ':`
</pre>

## [Example Result](./demo/DATA/binaryRL_result.html)

```r
binaryRL::run_m(
  mode = "replay",
  data = binaryRL::Mason_2024_Exp2,
  id = 1,
  eta = 0.5, tau = 0.5,
  n_params = 2, n_trials = 360
)
```

| A | B | C | D | - | L_porb | R_prob | - | Rob_Choose | - | Reward | - | ACC | - |
|---|---|---|---|---|--------|--------|---|------------|---|--------|---|-----|---|
| 36| 0 | 0 | 0 |...| 0.50   | 0.50   |...| A          |...| 36     |...| 1   |...|
| 36| 40| 0 | 0 |...| 0.50   | 0.50   |...| B          |...| 40     |...| 1   |...|
| 36| 40| 0 |-40|...| 0.50   | 0.50   |...| D          |...|-40     |...| 0   |...|
| 36| 40|-36|-40|...| 0.50   | 0.50   |...| C          |...|-36     |...| 0   |...|
|...|...|...|...|...| ...    | ...    |...| ...        |...|...     |...| ... |...|

<!---------------------------------------------------------->

# Parallel Data Fitting

While this R package is primarily designed for constructing **Reinforcement Learning (RL)** models (with `binaryRL::run_m` at its core), its flexibility extends further. 

The key functions, `binaryRL::rcv_d` and `binaryRL::fit_p`, provide a unified interface to seamlessly integrate a diverse range of optimization algorithms. Crucially, they offer a parallel solution for tasks like parameter optimization, parameter recovery, and model recovery.

This means you can leverage this package not only for building and fitting RL models, but also as a versatile algorithm library for fitting other **"black-box functions"** in parallel for each subject. This significantly reduces processing time, provided your function's parameters can be optimized independently for each subject.

**Base R Optimization**  
  - L-BFGS-B (from `stats::optim`)   

**Specialized External Optimization**  
  - Simulated Annealing (`GenSA::GenSA`)  
  - Genetic Algorithm (`GA::ga`)  
  - Differential Evolution (`DEoptim::DEoptim`).   
  - Particle Swarm Optimization (`pso::psoptim`)  
  - Bayesian Optimization (`mlrMBO::mbo`)  
  - Covariance Matrix Adapting Evolutionary Strategy (`cmaes::cma_es`)  

**Optimization Library**  
  - Nonlinear Optimization (`nloptr::nloptr`)
  </p>

*NOTE:* If you want to use an algorithm other than `L-BFGS-B`, you must install the corresponding package. 

<!---------------------------------------------------------->




