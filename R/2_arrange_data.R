arrange_data <- function(data, time_line = c("Block", "Trial")){
  # 基于time_line这个向量, 录入排序向量
  order_vector <- lapply(time_line, function(col) as.numeric(data[[col]]))
  
  # 基于排序向量对输入数据集进行排序
  data <- data[do.call(order, order_vector), ]
  
  return(data)
}
