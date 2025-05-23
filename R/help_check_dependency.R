#' Check Package Dependencies
#'
#' @description
#' Checks if one or more specified R packages are installed and available.
#' If any package is missing, it stops execution with an informative error
#' message guiding the user on how to install them.
#'
#' @param pkg_names [vector] A character vector containing the names of the packages
#'  to check.
#' @param algorithm_name [character] Optional: The name of the algorithm or feature
#'  that requires these packages. Used to make the error message more specific.
#'  Defaults to "this functionality".
#'
#' @returns Returns `invisible(TRUE)` if all specified packages are available,
#'   otherwise it stops the execution via `stop()`.
#'   
#' @noRd
#' 
check_dependency <- function(pkg_names, algorithm_name) {
  
  # Check availability for each package without loading them
  # sapply returns a named logical vector
  is_available <- sapply(pkg_names, requireNamespace, quietly = TRUE)
  
  # Identify the names of missing packages
  missing_pkgs <- names(is_available[!is_available])
  
  # Proceed only if there are missing packages
  if (length(missing_pkgs) > 0) {
    
    # Format package names for the error message
    missing_pkgs_str <- paste(missing_pkgs, collapse = ", ") 
    # e.g., "GA, DEoptim"
    install_cmd_pkgs <- paste0("'", missing_pkgs, "'", collapse = ", ") 
    # e.g., "'GA', 'DEoptim'"
    
    # Construct a helpful error message
    error_message <- sprintf(
      "Package(s) required for %s are missing: %s.\n
      Please install %s using:\n\ninstall.packages(c(%s))",
      algorithm_name,
      missing_pkgs_str,
      ifelse(length(missing_pkgs) > 1, "them", "it"), 
      # Use "them" or "it" appropriately
      install_cmd_pkgs
    )
    
    # Stop execution and display the message
    # call. = FALSE prevents the function call trace from being part of the error message
    stop(error_message, call. = FALSE)
  }
  
  # If the check passes for all packages, return TRUE invisibly
  invisible(TRUE)
}