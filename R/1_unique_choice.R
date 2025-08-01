unique_choice <- function(data, L_choice = "L_choice", R_choice = "R_choice"){
  # 用于获取左右选项中的备选项.
  # 以选项的名称创建新列, 用于储存每一次决策后的价值更新
  
  # 获取 L_choice 和 R_choice 的唯一值
  unique_L <- unique(data[[L_choice]])
  unique_R <- unique(data[[R_choice]])
  
  # 记录所有选项
  options <- sort(unique(c(unique_L, unique_R)))
  
  # 把所有备选项以列名创建, 方便存放价值更新
  for (name in options) {
    data[[name]] <- NA
  }
  
  res <- list(data, options)
  names(res)[1] <- "data"
  names(res)[2] <- "options"
  
  return(res)
}
