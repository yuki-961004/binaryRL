#' Detect Subject Information from Data
#'
#' @description
#' Analyzes a dataframe to extract subject-related information.
#'  It auto-detects a subject ID column (looking for names containing "sub"),
#'  then determines:
#'  1. The name of this subject ID column.
#'  2. The number of rows (trials) for one randomly selected subject.
#'  3. The ID of this randomly selected subject (coerced to numeric).
#'  4. A vector of all unique subject IDs found in the column.
#' The function provides messages about its progress and issues warnings
#'  or errors for common problems (e.g., multiple or no suitable subject
#'  columns found, or no valid subject IDs within the chosen column).
#'
#' @param data A dataframe. This dataframe must contain a column that can
#'   be identified as a subject identifier. The function searches for
#'   column names containing "sub" (case-insensitive) to find this
#'   identifier column.
#'
#' @returns
#' A named list with four elements:
#' \itemize{
#'   \item{\code{subject_column_name}}{ (Character) The name of the
#'     auto-detected subject identifier column.}
#'   \item{\code{n_rows_for_selected_id}}{ (Numeric) The number of rows
#'     (often corresponding to trials) for the randomly selected subject.}
#'   \item{\code{selected_id_numeric}}{ (Numeric) The ID of the randomly
#'     selected subject, coerced to a numeric type. This may result in
#'     \code{NA} if the original ID cannot be meaningfully converted to a
#'     number (e.g., if IDs are "S01", "P02").}
#'   \item{\code{all_valid_ids}}{ (Vector) A vector containing all unique,
#'     non-NA subject IDs found in the identified subject column. The
#'     type of elements in this vector will match their original type in
#'     the dataframe (e.g., character or numeric).}
#' }
#' @export
#'
#' @noRd
#'
detect_information <- function(data, seed) {
  # 1. Find column names containing "sub" 
  # (case-insensitive)
  col_names <- names(data)
  subject_col_candidates <- col_names[grepl("sub", col_names, ignore.case = TRUE)]
  
  if (length(subject_col_candidates) == 0) {
    stop(
      "Error: Could not find a subject ID column containing 'sub' in the data. 
      Please check the column names."
    )
  }
  
  subject_col_name <- subject_col_candidates[1] # Default to the first one found
  if (length(subject_col_candidates) > 1) {
    warning(paste(
      "Found multiple columns containing 'sub': ",
      paste(subject_col_candidates, collapse = ", "),
      ". Using the first one found: '", subject_col_name, "'", sep = ""
    ))
  } else {
    message(paste(
      "Automatically detected subject ID column as: '", 
      subject_col_name, 
      "'", sep = ""
    ))
  }
  
  # 2. Randomly select an existing subject ID from the column 
  # (ensure NA values are excluded)
  # First, get all IDs from the column, then remove NAs
  all_ids_in_column <- data[[subject_col_name]]
  valid_subject_ids <- unique(all_ids_in_column[!is.na(all_ids_in_column)])
  
  if (length(valid_subject_ids) == 0) {
    stop(paste(
      "Error: No valid subject IDs found in column '", 
      subject_col_name, 
      "' (column might only contain NAs, be empty, 
      or all values became NA after processing).", 
      sep = ""
    ))
  }
  
  set.seed(seed)
  random_subject_id <- sample(valid_subject_ids, 1)
  message(paste(
    "Randomly selected subject ID: '", 
    random_subject_id, 
    "' (from column '", 
    subject_col_name, 
    "') for row count.", 
    sep = ""
  ))
  
  # 3. Count the number of rows for this subject ID
  # Using which() to get matching row indices, then length() to count.
  # This method correctly handles NAs that could arise from comparing random_subject_id 
  # with data[[subject_col_name]] (if data[[subject_col_name]] itself contains NAs).
  # (although random_subject_id itself will not be NA)
  subject_rows_indices <- which(data[[subject_col_name]] == random_subject_id)
  n_rows <- length(subject_rows_indices)
  
  message(paste(
    "Found ", 
    n_rows, 
    " rows for subject ID '", 
    random_subject_id, 
    "'.", 
    sep=""
  ))
  
  # 构建包含四个命名元素的列表
  res <- list(
    # 第一个元素: subject那一列的列名 (字符串)
    sub_col_name = subject_col_name,
    # 第二个元素: 为随机选取的ID找到的行数
    n_trials = n_rows,
    # 第三个元素: 随机选取的ID (尝试转为数值型)
    random_id = as.numeric(random_subject_id),
    # 第四个元素: subject列所有存在的ID向量
    all_ids = valid_subject_ids
  )
  
  return(res)
}
