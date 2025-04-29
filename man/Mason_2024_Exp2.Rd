\name{Mason_2024_Exp2}
\alias{Mason_2024_Exp2}
\title{Experiment 2 from Mason et al. (2024)}
\description{
  This dataset is from Experiment 1 of Mason et al. (2024).
  (Rare and extreme outcomes in risky choice).
  Data is publicly available on OSF: https://osf.io/hy3q4/.
  We performed basic cleaning to meet our package needs. 
}
\format{
  A data frame with 45000 rows and 11 columns:
  \describe{
    \item{Subject}{Subject ID, an integer (16 to 144)}
    \item{Block}{Block number, an integer (1 to 6)}
    \item{Trial}{Trial number, an integer (1 to 60)}
    \item{L_choice}{
      Left choice, 
      A = 100\% gain 36, 
      B = 90\% gain 40 and 10\% gain 0,  
      C = 100\% lose 36,
      D = 90\% lose 40 and 10\% lose 0.  
    }
    \item{R_choice}{
      Right choice, 
      A = 100\% gain 36, 
      B = 90\% gain 40 and 10\% gain 0,  
      C = 100\% lose 36,
      D = 90\% lose 40 and 10\% lose 0.  
    }
    \item{L_reward}{Reward associated with the left choice.}
    \item{R_reward}{Reward associated with the right choice.}
    \item{Sub_Choose}{The chosen option, either L_choice or R_choice.}
    \item{Frame}{Type of frame, "Gain", "Loss", "Catch".}
    \item{NetWorth}{The participant's net worth at the end of each trial.}
    \item{RT}{The participant's reaction time (in milliseconds) for each trial.}
  }
}
\examples{
  # Load the Mason_2024_Exp2 dataset
  data(Mason_2024_Exp2)
  head(Mason_2024_Exp2)
}
