mode <- function(
  data, 
  mode = "fit",
  sub_choose = "Sub_Choose",
  rob_choose = "Rob_Choose",
  raw_cols = c(
    "Subject", "Block", "Trial", 
    "L_choice", "R_choice", 
    "L_reward", "R_reward", 
    "Sub_Choose"
  )
){
  switch(
    mode, 
    # fit中机器人直接抄答案, 让rob_choose直接取sub_choose, 旨在理解人类行为
    "fit" = {
      data <- data
    }, 
    # 基于输入的参数, 生成和原始数据列数相同的假数据
    "simulate" = {
      data[["data"]][[sub_choose]] <- data[["data"]][[rob_choose]]
      data[["data"]] <- data[["data"]][, raw_cols]
    },
    # replay中的机器人完全基于参数自主答题. 旨在复现人类行为. 
    "replay" = {
      data <- data
    }, 
  )
  
  return(data)
}
