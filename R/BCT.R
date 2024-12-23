# 设置随机种子以便复现结果
set.seed(123)

# 定义函数来生成数据集
generate_BCT_data <- function(n_subjects = 30, n_blocks = 6, n_trials_per_block = 48) {
  
  # 初始化空的结果数据框
  BCT <- data.frame()
  
  # 循环遍历每个被试
  for (subject in 1:n_subjects) {
    
    # 创建每个被试的数据
    for (block in 1:n_blocks) {
      
      # 创建 Trial 列
      trials <- 1:n_trials_per_block
      
      # 创建 Frame 列（50% Gain, 50% Loss）
      frame <- rep(c("Gain", "Loss"), each = n_trials_per_block / 2)
      frame <- sample(frame)  # 随机打乱顺序
      
      # 创建 L_choice 和 R_choice 列，按 Frame 的类型分配
      l_choice <- character(n_trials_per_block)
      r_choice <- character(n_trials_per_block)
      
      for (i in 1:n_trials_per_block) {
        if (frame[i] == "Gain") {
          # Gain 框架，选项是 A 和 B
          options_gain <- c("A", "B")
        } else {
          # Loss 框架，选项是 C 和 D
          options_gain <- c("C", "D")
        }
        
        # 打乱左右选择
        choices <- sample(options_gain)
        l_choice[i] <- choices[1]
        r_choice[i] <- choices[2]
      }
      
      # 创建 L_reward 和 R_reward 列
      l_reward <- numeric(n_trials_per_block)
      r_reward <- numeric(n_trials_per_block)
      
      for (i in 1:n_trials_per_block) {
        # L_choice 奖励设置
        if (l_choice[i] == "A") {
          l_reward[i] <- 20
        } else if (l_choice[i] == "B") {
          l_reward[i] <- sample(c(0, 40), 1)  # 50%奖励
        } else if (l_choice[i] == "C") {
          l_reward[i] <- -20
        } else if (l_choice[i] == "D") {
          l_reward[i] <- sample(c(0, -40), 1)
        }
        
        # R_choice 奖励设置
        if (r_choice[i] == "A") {
          r_reward[i] <- 20
        } else if (r_choice[i] == "B") {
          r_reward[i] <- sample(c(0, 40), 1)
        } else if (r_choice[i] == "C") {
          r_reward[i] <- -20
        } else if (r_choice[i] == "D") {
          r_reward[i] <- sample(c(0, -40), 1)
        }
      }
      
      # 创建 Choose 列（随机选择 L_choice 或 R_choice）
      choose <- ifelse(runif(n_trials_per_block) < 0.5, l_choice, r_choice)
      
      # 创建 Reward 列（根据 Choose 列选择奖励）
      reward <- numeric(n_trials_per_block)
      for (i in 1:n_trials_per_block) {
        if (choose[i] == l_choice[i]) {
          reward[i] <- l_reward[i]
        } else {
          reward[i] <- r_reward[i]
        }
      }
      
      # 将数据添加到结果数据框
      BCT <- rbind(BCT, data.frame(
          Subject = subject,
          Block = block,
          Trial = trials,
          Frame = frame,
          L_choice = l_choice,
          R_choice = r_choice,
          L_reward = l_reward,
          R_reward = r_reward,
          Choose = choose,
          Reward = reward
        )
      )
    }
  }
  
  return(BCT)
}

# 生成数据集
BCT <- generate_BCT_data()

# 在R目录下创建一个脚本来保存数据集
usethis::use_data(BCT, overwrite = TRUE)
