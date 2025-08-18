#include <Rcpp.h>
#include <random>
#include <algorithm>  // std::find
#include <vector>
#include <string>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::DataFrame decision_making_cpp(
    std::string mode,
    std::string policy,
    Rcpp::DataFrame data,
    Rcpp::CharacterVector options,
    int seed,
    
    std::string sub_choose,
    std::string rob_choose,
    std::string L_choice,
    std::string R_choice,
    std::string L_reward,
    std::string R_reward,
    std::string var1,
    std::string var2,

    double initial_value,
    int threshold,
    double lapse,

    Rcpp::NumericVector gamma,
    Rcpp::NumericVector eta,
    Rcpp::NumericVector epsilon,
    Rcpp::NumericVector lambda,
    Rcpp::NumericVector pi,
    Rcpp::NumericVector tau,
    Rcpp::NumericVector alpha,
    Rcpp::NumericVector beta,

    Rcpp::Function util_func,
    Rcpp::Function rate_func,
    Rcpp::Function expl_func,
    Rcpp::Function bias_func,
    Rcpp::Function prob_func
) {
  // 随机生成0, 1之间的数字
  std::uniform_real_distribution <> runif(0.0, 1.0);

////////////////////////////////// [counts] ////////////////////////////////////

  // 用于记录每个选项作为刺激呈现次数的计数器
  std::unordered_map<std::string, int> stim_freq;
  for (const auto & name : options) {
    stim_freq[Rcpp::as<std::string>(name)] = 0;
  }
  
  // 用于记录每个选项被选择次数的计数器
  std::unordered_map<std::string, int> pick_counts;
  for (const auto & name : options) {
    pick_counts[Rcpp::as<std::string>(name)] = 0;
  }

////////////////////////////////// [vectors] ///////////////////////////////////

  // 表格中的逐行运算, 转化为向量运算

  Rcpp::CharacterVector L_choice_vec = data[L_choice];
  Rcpp::CharacterVector R_choice_vec = data[R_choice];

  Rcpp::NumericVector L_reward_vec = data[L_reward];
  Rcpp::NumericVector R_reward_vec = data[R_reward];

  Rcpp::NumericVector var1_vec(data.nrow());
  Rcpp::NumericVector var2_vec(data.nrow());
  if (var1 != "NA") {var1_vec = data[var1];} 
  if (var2 != "NA") {var2_vec = data[var2];} 

  Rcpp::CharacterVector Sub_Choose_vec = data[sub_choose];
  Rcpp::CharacterVector Rob_Choose_vec = data[rob_choose];

  Rcpp::NumericVector L_value = data["L_value"];
  Rcpp::NumericVector R_value = data["R_value"];

  Rcpp::NumericVector L_bias = data["L_bias"];
  Rcpp::NumericVector R_bias = data["R_bias"];
  
  Rcpp::NumericVector L_prob = data["L_prob"];
  Rcpp::NumericVector R_prob = data["R_prob"];

  Rcpp::NumericVector L_freq = data["L_freq"];
  Rcpp::NumericVector R_freq = data["R_freq"];

  Rcpp::NumericVector L_pick = data["L_pick"];
  Rcpp::NumericVector R_pick = data["R_pick"];

  Rcpp::NumericVector Occurrence = data["Occurrence"];
  
  Rcpp::NumericVector Reward = data["Reward"];
  Rcpp::NumericVector gamma_col = data["gamma"];
  Rcpp::NumericVector R_utility = data["R_utility"];
  
  Rcpp::NumericVector V_value = data["V_value"];
  Rcpp::NumericVector eta_col = data["eta"];
  Rcpp::NumericVector V_update = data["V_update"];
  
  Rcpp::NumericVector Try = data["Try"];

  // 创建一个list对象, 里面包含了每个选项作为向量
  Rcpp::List option_cols;
    for (const auto & name : options) {
        option_cols.push_back(data[std::string(name)]);
    }
    // 每个向量的名字就是options的名字
    option_cols.names() = options;

//////////////////////////// [update row by row] ///////////////////////////////

  for (int i = 1; i < data.nrow(); i = i + 1) { 
    // 记录当前试次左右选项的名字
    std::string L_name = Rcpp::as<std::string>(L_choice_vec[i]);
    std::string R_name = Rcpp::as<std::string>(R_choice_vec[i]);

    // 创建一个只包含不重复名字的字符向量
    Rcpp::CharacterVector shown_name = Rcpp::unique(
      Rcpp::CharacterVector::create(L_name, R_name)
    );

    // # 在计数器中给这两个刺激的呈现次数 +1, 仅针对不重复的情况
    for (const auto & name : shown_name) {
      // 这里可以直接通过名字作为键来访问 map 中的元素
      stim_freq[Rcpp::as<std::string>(name)] += 1;
    }

    // 查询此时左选项已经出现过几次了
    L_freq[i] = stim_freq[L_name];
    // 计算此时右选项已经出现过几次了
    R_freq[i] = stim_freq[R_name];

    // 计算此时左选项被选了几次
    L_pick[i] = pick_counts[L_name];
    // 计算此时右选项被选了几次
    R_pick[i] = pick_counts[R_name];

    // 在上一行找此时左右选项对应的心中的价值
    L_value[i] = Rcpp::as<Rcpp::NumericVector>(option_cols[L_name])[i - 1];
    R_value[i] = Rcpp::as<Rcpp::NumericVector>(option_cols[R_name])[i - 1];

///////////////////////////////// [ epsilon ] //////////////////////////////////

    // epsilon: 确定是否需要随机选择(探索)
    Try[i] = Rcpp::as<double>(expl_func(
        Rcpp::_["i"] = i,
        Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i],
        Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i],
        Rcpp::_["L_value"] = L_value[i], Rcpp::_["R_value"] = R_value[i],
        Rcpp::_["var1"] = var1_vec[i], Rcpp::_["var2"] = var2_vec[i],
        Rcpp::_["threshold"] = threshold,
        Rcpp::_["epsilon"] = epsilon,
        Rcpp::_["lambda"] = lambda,
        Rcpp::_["alpha"] = alpha,
        Rcpp::_["beta"] = beta
    ));
//////////////////////////////////// [ pi ] ////////////////////////////////////

    // pi: 对选项价值的偏差值, 默认和被被选次数成反比例
    L_bias[i] = Rcpp::as<double>(bias_func(
      Rcpp::_["i"] = i, 
      Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i], 
      Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i], 
      Rcpp::_["L_value"] = L_value[i], Rcpp::_["R_value"] = R_value[i], 
      Rcpp::_["var1"] = var1_vec[i], Rcpp::_["var2"] = var2_vec[i], 
      Rcpp::_["LR"] = "L",
      Rcpp::_["pi"] = pi, 
      Rcpp::_["alpha"] = alpha, 
      Rcpp::_["beta"] = beta
    ));
        
    R_bias[i] = Rcpp::as<double>(bias_func(
      Rcpp::_["i"] = i, 
      Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i], 
      Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i], 
      Rcpp::_["L_value"] = L_value[i], Rcpp::_["R_value"] = R_value[i], 
      Rcpp::_["var1"] = var1_vec[i], Rcpp::_["var2"] = var2_vec[i], 
      Rcpp::_["LR"] = "R",
      Rcpp::_["pi"] = pi, 
      Rcpp::_["alpha"] = alpha, 
      Rcpp::_["beta"] = beta
    ));
/////////////////////////////////// [ tau ] ////////////////////////////////////

    // tau: 左右选项备选的概率
    L_prob[i] = Rcpp::as<double>(prob_func(
      Rcpp::_["i"] = i, 
      Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i], 
      Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i], 
      Rcpp::_["L_value"] = L_value[i] + L_bias[i],
      Rcpp::_["R_value"] = R_value[i] + R_bias[i],
      Rcpp::_["var1"] = var1_vec[i], Rcpp::_["var2"] = var2_vec[i],
      Rcpp::_["try"] = Try[i], 
      Rcpp::_["LR"] = "L",
      Rcpp::_["lapse"] = lapse, 
      Rcpp::_["tau"] = tau,
      Rcpp::_["alpha"] = alpha, 
      Rcpp::_["beta"] = beta
    ));
    
    R_prob[i] = Rcpp::as<double>(prob_func(
        Rcpp::_["i"] = i, 
        Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i], 
        Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i],
        Rcpp::_["L_value"] = L_value[i] + L_bias[i],
        Rcpp::_["R_value"] = R_value[i] + R_bias[i],
        Rcpp::_["var1"] = var1_vec[i], Rcpp::_["var2"] = var2_vec[i],
        Rcpp::_["try"] = Try[i], 
        Rcpp::_["LR"] = "R",
        Rcpp::_["lapse"] = lapse, 
        Rcpp::_["tau"] = tau,
        Rcpp::_["alpha"] = alpha, 
        Rcpp::_["beta"] = beta
    ));

////////////////////////////// [ PASS VALUE ] //////////////////////////////////

    for (int j = 0; j < option_cols.size(); j = j + 1) {
      // 获取第 j 列
      Rcpp::NumericVector option_vec = option_cols[j]; 
      // 赋值：C++ 下标从 0 开始，所以 i-1 对应 R 的 i-1
      option_vec[i] = option_vec[i - 1]; 
      // 更新回 list
      option_cols[j] = option_vec;
    }
//////////////////////////// [ on/off policy ] /////////////////////////////////

    // 根據 on-policy 或 off-policy 決定機器人的選擇
    if (policy == "off") {
      // 如果是 off-policy, 直接使用人類的選擇
      Rob_Choose_vec[i] = Sub_Choose_vec[i];
    } 
    else if (policy == "on") {
      std::mt19937 engine(seed + i);
      Rob_Choose_vec[i] = (
        runif(engine) < L_prob[i] ? L_choice_vec[i] : R_choice_vec[i]
      );
    }

///////////////////////////// [ chosen count ] /////////////////////////////////

    // 记录被选的选项(字符串)
    std::string choose = Rcpp::as<std::string>(Rob_Choose_vec[i]);

    // 计算这次是第几次选了这个选项
    Occurrence[i] = pick_counts[choose];

    // # 在计数器中给这两个刺激的呈现次数 +1, 仅针对不重复的情况
    pick_counts[choose] = pick_counts[choose] + 1;

//////////////////////////////// [ Reward ] ////////////////////////////////////

    // 基于选择, 来给予奖励
    if (Rob_Choose_vec[i] == L_choice_vec[i]){
      // 选了左边, 给左的奖励
      Reward[i] = L_reward_vec[i];
    } 
    else if (Rob_Choose_vec[i] == R_choice_vec[i]) {
      // 选了右边, 给右的奖励
      Reward[i] = R_reward_vec[i];
    }

///////////////////////////////// [ gamma ] ////////////////////////////////////

    // 查询被选择选项的价值
    V_value[i] = Rcpp::as<Rcpp::NumericVector>(option_cols[choose])[i - 1];

    // gamma: 用幂函数将物理量reward转化成心理量utility
    Rcpp::List gamma_utility = util_func(
      Rcpp::_["i"] = i,
      Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i],
      Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i],
      Rcpp::_["L_value"] = L_value[i], Rcpp::_["R_value"] = R_value[i],
      Rcpp::_["value"] = V_value[i],
      Rcpp::_["utility"] = R_utility[i],
      Rcpp::_["reward"] = Reward[i],
      Rcpp::_["occurrence"] = Occurrence[i],
      Rcpp::_["gamma"] = gamma,
      Rcpp::_["alpha"] = alpha,
      Rcpp::_["beta"] = beta
    );

    // 提取 List 中的元素
    gamma_col[i] = Rcpp::as<double>(gamma_utility[0]);
    R_utility[i] = Rcpp::as<double>(gamma_utility[1]);

////////////////////////////////// [ eta ] /////////////////////////////////////

    // eta: 基于Rescorla-Wagner Model更新价值
    eta_col[i] = Rcpp::as<double>(rate_func(
      Rcpp::_["i"] = i, 
      Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i], 
      Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i], 
      Rcpp::_["L_value"] = L_value[i], Rcpp::_["R_value"] = R_value[i], 
      Rcpp::_["value"] = V_value[i],
      Rcpp::_["utility"] = R_utility[i], 
      Rcpp::_["reward"] = Reward[i],
      Rcpp::_["occurrence"] = Occurrence[i], 
      Rcpp::_["eta"] = eta,
      Rcpp::_["alpha"] = alpha, 
      Rcpp::_["beta"] = beta
    ));

//////////////////////////// [ 1st Learning Rate ] /////////////////////////////

    // 如果没有设置初始值, 且是第一次选这个选项
    if (Rcpp::NumericVector::is_na(initial_value) && Occurrence[i] == 0) {
      eta_col[i] = 1;
      V_update[i] = V_value[i] + eta_col[i] * (R_utility[i] - V_value[i]);
      // 传递V_update回到options_cols
      Rcpp::as<Rcpp::NumericVector>(option_cols[choose])[i] = V_update[i];
    } else {
      V_update[i] = V_value[i] + eta_col[i] * (R_utility[i] - V_value[i]);
      // 传递V_update回到options_cols
      Rcpp::as<Rcpp::NumericVector>(option_cols[choose])[i] = V_update[i];
    }
  }

////////////////////////////////// [ fill data ] ///////////////////////////////

  data[L_choice] = L_choice_vec;
  data[R_choice] = R_choice_vec;

  data[L_reward] = L_reward_vec;
  data[R_reward] = R_reward_vec;

  data[sub_choose] = Sub_Choose_vec;
  data[rob_choose] = Rob_Choose_vec;

  data["L_value"] = L_value;
  data["R_value"] = R_value;

  data["L_bias"] = L_bias;
  data["R_bias"] = R_bias;

  data["L_prob"] = L_prob;
  data["R_prob"] = R_prob;

  data["L_freq"] = L_freq;
  data["R_freq"] = R_freq;

  data["L_pick"] = L_pick;
  data["R_pick"] = R_pick;

  data["Occurrence"] = Occurrence;

  data["Reward"] = Reward;
  data["gamma"] = gamma_col;
  data["R_utility"] = R_utility;

  data["V_value"] = V_value;
  data["eta"] = eta_col;
  data["V_update"] = V_update;

  data["Try"] = Try;

  return data;
}