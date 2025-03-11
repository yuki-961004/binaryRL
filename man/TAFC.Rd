\name{TAFC}
\alias{TAFC}
\title{A Simulated Dataset for Reinforcement Learning Task}
\description{
  This is a simulated dataset for a reinforcement learning task.
  The dataset contains information from 30 subjects, with 6 blocks, 
  each containing 48 trials. The dataset includes columns for subject ID, 
  block number, trial number, frame type (Gain or Loss), 
  left and right choices, corresponding rewards, and the selected choice.
}
\format{
  A data frame with 8640 rows and 10 columns:
  \describe{
    \item{Subject}{Subject ID, an integer (1 to 30)}
    \item{Block}{Block number, an integer (1 to 6)}
    \item{Trial}{Trial number, an integer (1 to 48)}
    \item{Frame}{Type of frame, "Gain" or "Loss"}
    \item{L_choice}{Left choice, one of "A", "B", "C", or "D"}
    \item{R_choice}{Right choice, one of "A", "B", "C", or "D"}
    \item{L_reward}{Reward associated with the left choice}
    \item{R_reward}{Reward associated with the right choice}
    \item{Choose}{The chosen option, either L_choice or R_choice}
    \item{Reward}{Reward corresponding to the chosen option}
  }
}
\examples{
  # Load the TAFC dataset
  data(TAFC)
  head(TAFC)
}
