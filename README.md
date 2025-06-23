# binaryRL <a href="https://yuki-961004.github.io/binaryRL/"><img src="./fig/logo.png" alt="LOGO" align="right" width="120"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/yuki-961004/binaryRL/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yuki-961004/binaryRL/actions/workflows/R-CMD-check.yaml)
[![Code Coverage](https://codecov.io/gh/yuki-961004/binaryRL/graph/badge.svg)](https://app.codecov.io/gh/yuki-961004/binaryRL)
[![CRAN-Version](https://www.r-pkg.org/badges/version/binaryRL?color=%231E63B5)](https://CRAN.R-project.org/package=binaryRL)
[![CRAN-Downloads](https://cranlogs.r-pkg.org/badges/grand-total/binaryRL?color=%23FA812F)](https://CRAN.R-project.org/package=binaryRL)

<!-- badges: end -->

## Homepage
https://yuki-961004.github.io/binaryRL/

## Overview
This package is designed to help users build the **Rescorla-Wagner Model** for Two-Alternative Forced Choice (TAFC) tasks, which could be the simplest reinforcement learning models, assuming that reward outcomes are independent and identically distributed (i.i.d.) across trials. Beginners can define models using simple `if-else` logic, making model construction more accessible.

* [Step 1](./articles/binaryRL.html#1-run-model): Build Reinforcement Learning Models `run_m`
* [Step 2](./articles/binaryRL.html#2-recovery): Parameter and Model Recovery `rcv_d`
* [Step 3](./articles/binaryRL.html#3-fit-real-data): Fit Real Data `fit_p`
* [Step 4](./articles/binaryRL.html#4-replay-the-experiment): Replay the Experiment `rpl_e` 

<!---------------------------------------------------------->

## How to cite 
YuKi. (2025). binaryRL: Reinforcement Learning Tools for Two-Alternative Forced Choice Tasks. R package version 0.8.0. https://CRAN.R-project.org/package=binaryRL

Hu, M., & Liu, Z. (2025). binaryRL: A Package for Building Reinforcement Learning Models in R. *Journal*(7), 100-123. https://doi.org/

## Install and Load Pacakge
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

### Reference
Mason, A., Ludvig, E. A., Spetch, M. L., & Madan, C. R. (2024). Rare and extreme outcomes in risky choice. Psychonomic Bulletin & Review, 31(3), 1301-1308. https://doi.org/10.3758/s13423-023-02415-x
<!---------------------------------------------------------->


