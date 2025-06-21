#' @keywords internal
"_PACKAGE"
#' @aliases binaryRL
#' 
#' @title binaryRL: Reinforcement Learning Tools for Two-Alternative Forced Choice Tasks.
#'
#' @section Steps:
#'  \itemize{
#'    \item \code{\link[binaryRL]{run_m}}: 
#'      Step 1: Building reinforcement learning model
#'    \item \code{\link[binaryRL]{rcv_d}}: 
#'      Step 2: Generating fake data for parameter and model recovery
#'    \item \code{\link[binaryRL]{fit_p}}: 
#'      Step 3: Optimizing parameters to fit real data
#'    \item \code{\link[binaryRL]{rpl_e}}: 
#'      Step 4: Replaying the experiment with optimal parameters
#' }
#' 
#' @section Models:
#'  \itemize{
#'    \item \code{\link[binaryRL]{TD}}: 
#'      TD Model
#'    \item \code{\link[binaryRL]{RSTD}}: 
#'      RSTD Model
#'    \item \code{\link[binaryRL]{Utility}}: 
#'      Utility Model
#' }
#' 
#' @section Functions:
#' \itemize{
#'   \item \code{\link[binaryRL]{func_gamma}}: Utility Function
#'   \item \code{\link[binaryRL]{func_eta}}: Learning Rate 
#'   \item \code{\link[binaryRL]{func_epsilon}}: Exploration Strategy
#'   \item \code{\link[binaryRL]{func_tau}}: Soft-Max
#' }
#' 
#' @section Processes: 
#'  \itemize{
#'    \item \code{\link[binaryRL]{optimize_para}}: optimizing free parameters
#'    \item \code{\link[binaryRL]{simulate_list}}: simulating fake datasets
#'    \item \code{\link[binaryRL]{recovery_data}}: parameter and model recovery
#' }
#'
#' @section Summary:
#' \itemize{
#'   \item \code{\link[binaryRL]{summary.binaryRL}}: summary(binaryRL.res)
#' }
#'
#' @name binaryRL-package
NULL