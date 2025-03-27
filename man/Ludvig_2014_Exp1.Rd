\name{Ludvig_2014_Exp1}
\alias{Ludvig_2014_Exp1}
\title{Experiment 1 from Ludvig et al. (2014)}
\description{
  This dataset is from Experiment 1 of Ludvig et al. (2014).
  (Extreme Outcomes Sway Risky Decisions from Experience).
  Data is publicly available on OSF: https://osf.io/eagcd/.
  We performed basic cleaning to meet our package needs. 
  Unchosen option rewards were randomly generated due to missing 
  original values.
}
\format{
  A data frame with 11495 rows and 11 columns:
  \describe{
    \item{Subject}{Subject ID, an integer (1 to 40)}
    \item{Block}{Block number, an integer (1 to 6)}
    \item{Trial}{Trial number, an integer (1 to 48)}
    \item{L_choice}{
      Left choice, 
      1 = 100\% gain 20, 
      2 = 50\% gain 0 and 50\% gain 40,  
      4 = 100\% lose 20,
      5 = 50\% lose 0 and 50\% lose 40.  
    }
    \item{R_choice}{
      Right choice, 
      1 = 100\% gain 20, 
      2 = 50\% gain 0 and 50\% gain 40,  
      4 = 100\% lose 20,
      5 = 50\% lose 0 and 50\% lose 40.
    }
    \item{L_reward}{Reward associated with the left choice.}
    \item{R_reward}{Reward associated with the right choice.}
    \item{Sub_Choose}{The chosen option, either L_choice or R_choice.}
    \item{Frame}{Type of frame, "Gain", "Loss", "Catch", "Single".}
    \item{NetWorth}{The participant's net worth at the end of each trial.}
    \item{RT}{The participant's reaction time (in milliseconds) for each trial.}
  }
}
\examples{
  # Load the Ludvig_2014_Exp1 dataset
  data(Ludvig_2014_Exp1)
  head(Ludvig_2014_Exp1)
}
