\name{Mason_2024_Exp2}
\alias{Mason_2024_Exp2}
\title{Experiment 2 from Mason et al. (2024)}
\description{
  This dataset originates from Experiment 2 of Mason et al. (2024),
  titled "Rare and extreme outcomes in risky choice"
  (\doi{10.3758/s13423-023-02415-x}).
  The raw data is publicly available on the Open Science Framework (OSF)
  at \url{https://osf.io/hy3q4/}.
  For the purposes of this package, we've performed
  basic cleaning and preprocessing of the original dataset.
}
\format{
  A data frame with 45000 rows and 11 columns:
  \describe{
    \item{Subject}{Subject ID, an integer (total of 143).}
    \item{Block}{Block number, an integer (1 to 6).}
    \item{Trial}{Trial number, an integer (1 to 60).}
    \item{L_choice}{
      Left choice, a character indicating the option presented.
      The possible options are:
      \itemize{
        \item A: 100\% gain 36.
        \item B: 90\% gain 40 and 10\% gain 0.
        \item C: 100\% lose 36.
        \item D: 90\% lose 40 and 10\% lose 0.
      }
    }
    \item{R_choice}{
      Right choice, a character indicating the option presented.
      The possible options are:
      \itemize{
        \item A: 100\% gain 36.
        \item B: 90\% gain 40 and 10\% gain 0.
        \item C: 100\% lose 36.
        \item D: 90\% lose 40 and 10\% lose 0.
      }
    }
    \item{L_reward}{Reward associated with the left choice.}
    \item{R_reward}{Reward associated with the right choice.}
    \item{Sub_Choose}{The chosen option, either \code{L_choice} or \code{R_choice}.}
    \item{Frame}{Type of frame, a character string (e.g., "Gain", "Loss", "Catch").}
    \item{NetWorth}{The participant's net worth at the end of each trial.}
    \item{RT}{The participant's reaction time (in milliseconds) for each trial.}
  }
}
\examples{
  # Load the Mason_2024_Exp2 dataset
  data(Mason_2024_Exp2)
  head(Mason_2024_Exp2)
}
