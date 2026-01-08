#include <Rcpp.h>
#include <random>

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
///////////////////////////////// [ counts ] ///////////////////////////////////
  
  std::uniform_real_distribution <> runif(0.0, 1.0);

  // 建立名称到索引的映射，这是规避 ATTRIB 报错的关键
  std::unordered_map<std::string, int> name_to_idx;
  std::unordered_map<std::string, int> stim_freq;
  std::unordered_map<std::string, int> pick_counts;
  
  for (int j = 0; j < options.size(); j = j + 1) {
    std::string nm = Rcpp::as<std::string>(options[j]);
    name_to_idx[nm] = j;
    stim_freq[nm] = 0;
    pick_counts[nm] = 0;
  }

///////////////////////////////// [ vectors ] //////////////////////////////////

  Rcpp::CharacterVector L_choice_vec = data[L_choice];
  Rcpp::CharacterVector R_choice_vec = data[R_choice];
  Rcpp::NumericVector L_reward_vec = data[L_reward];
  Rcpp::NumericVector R_reward_vec = data[R_reward];

  Rcpp::NumericVector var1_vec(data.nrow()), var2_vec(data.nrow());
  if (var1 != "NA") { var1_vec = data[var1]; } 
  if (var2 != "NA") { var2_vec = data[var2]; } 

  Rcpp::CharacterVector Sub_Choose_vec = data[sub_choose];
  Rcpp::CharacterVector Rob_Choose_vec = data[rob_choose];
  Rcpp::NumericVector L_value = data["L_value"], R_value = data["R_value"];
  Rcpp::NumericVector L_bias = data["L_bias"], R_bias = data["R_bias"];
  Rcpp::NumericVector L_prob = data["L_prob"], R_prob = data["R_prob"];
  Rcpp::NumericVector L_freq = data["L_freq"], R_freq = data["R_freq"];
  Rcpp::NumericVector L_pick = data["L_pick"], R_pick = data["R_pick"];
  Rcpp::NumericVector Occurrence = data["Occurrence"];
  Rcpp::NumericVector Reward = data["Reward"], gamma_col = data["gamma"];
  Rcpp::NumericVector R_utility = data["R_utility"], V_value = data["V_value"];
  Rcpp::NumericVector eta_col = data["eta"], V_update = data["V_update"];
  Rcpp::NumericVector Try = data["Try"];

  Rcpp::List option_cols(options.size());
  for (int j = 0; j < options.size(); j = j + 1) {
    option_cols[j] = data[Rcpp::as<std::string>(options[j])];
  }

/////////////////////////// [ update row by row ] //////////////////////////////

  for (int i = 1; i < data.nrow(); i = i + 1) { 
    std::string L_name = Rcpp::as<std::string>(L_choice_vec[i]);
    std::string R_name = Rcpp::as<std::string>(R_choice_vec[i]);

    // 规避 Rcpp::unique 的 API 风险
    stim_freq[L_name] += 1;
    if (L_name != R_name) { stim_freq[R_name] += 1; }

    L_freq[i] = stim_freq[L_name];
    R_freq[i] = stim_freq[R_name];
    L_pick[i] = pick_counts[L_name];
    R_pick[i] = pick_counts[R_name];

    // 使用整数索引访问 option_cols
    int L_idx = name_to_idx[L_name];
    int R_idx = name_to_idx[R_name];
    L_value[i] = Rcpp::as<Rcpp::NumericVector>(option_cols[L_idx])[i - 1];
    R_value[i] = Rcpp::as<Rcpp::NumericVector>(option_cols[R_idx])[i - 1];

///////////////////////////////// [ epsilon ] //////////////////////////////////

    Try[i] = Rcpp::as<double>(expl_func(
      Rcpp::_["i"] = i, 
      Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i], 
      Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i], 
      Rcpp::_["L_value"] = L_value[i], Rcpp::_["R_value"] = R_value[i], 
      Rcpp::_["var1"] = var1_vec[i], 
      Rcpp::_["var2"] = var2_vec[i], 
      Rcpp::_["threshold"] = threshold,
      Rcpp::_["epsilon"] = epsilon, 
      Rcpp::_["lambda"] = lambda,
      Rcpp::_["alpha"] = alpha, 
      Rcpp::_["beta"] = beta
    ));

//////////////////////////////////// [ pi ] ////////////////////////////////////

    L_bias[i] = Rcpp::as<double>(bias_func(
      Rcpp::_["i"] = i, 
      Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i], 
      Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i], 
      Rcpp::_["L_value"] = L_value[i], Rcpp::_["R_value"] = R_value[i], 
      Rcpp::_["var1"] = var1_vec[i], 
      Rcpp::_["var2"] = var2_vec[i], 
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
      Rcpp::_["var1"] = var1_vec[i], 
      Rcpp::_["var2"] = var2_vec[i], 
      Rcpp::_["LR"] = "R",
      Rcpp::_["pi"] = pi, 
      Rcpp::_["alpha"] = alpha, 
      Rcpp::_["beta"] = beta
    ));

/////////////////////////////////// [ tau ] ////////////////////////////////////

    L_prob[i] = Rcpp::as<double>(prob_func(
      Rcpp::_["i"] = i, 
      Rcpp::_["L_freq"] = L_freq[i], Rcpp::_["R_freq"] = R_freq[i], 
      Rcpp::_["L_pick"] = L_pick[i], Rcpp::_["R_pick"] = R_pick[i], 
      Rcpp::_["L_value"] = L_value[i] + L_bias[i],
      Rcpp::_["R_value"] = R_value[i] + R_bias[i], 
      Rcpp::_["var1"] = var1_vec[i], 
      Rcpp::_["var2"] = var2_vec[i], 
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
      Rcpp::_["var1"] = var1_vec[i], 
      Rcpp::_["var2"] = var2_vec[i], 
      Rcpp::_["try"] = Try[i], 
      Rcpp::_["LR"] = "R", 
      Rcpp::_["lapse"] = lapse, 
      Rcpp::_["tau"] = tau,
      Rcpp::_["alpha"] = alpha, 
      Rcpp::_["beta"] = beta
    ));

////////////////////////////// [ PASS VALUE ] //////////////////////////////////

    for (int j = 0; j < option_cols.size(); j = j + 1) {
      Rcpp::NumericVector option_vec = option_cols[j]; 
      option_vec[i] = option_vec[i - 1]; 
    }

//////////////////////////// [ on/off policy ] /////////////////////////////////

    if (policy == "off") { Rob_Choose_vec[i] = Sub_Choose_vec[i]; } 
    else if (policy == "on") {
      std::mt19937 engine(seed + i);
      Rob_Choose_vec[i] = (
        runif(engine) < L_prob[i] ? L_choice_vec[i] : R_choice_vec[i]
      );
    }

///////////////////////////// [ chosen count ] /////////////////////////////////

    std::string choose = Rcpp::as<std::string>(Rob_Choose_vec[i]);
    Occurrence[i] = pick_counts[choose];
    pick_counts[choose] = pick_counts[choose] + 1;

//////////////////////////////// [ Reward ] ////////////////////////////////////

    if (Rob_Choose_vec[i] == L_choice_vec[i]) { Reward[i] = L_reward_vec[i]; } 
    else if (Rob_Choose_vec[i] == R_choice_vec[i]) { Reward[i] = R_reward_vec[i]; }

///////////////////////////////// [ gamma ] ////////////////////////////////////

    int choose_idx = name_to_idx[choose];
    V_value[i] = Rcpp::as<Rcpp::NumericVector>(option_cols[choose_idx])[i - 1];

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

    gamma_col[i] = Rcpp::as<double>(gamma_utility[0]);
    R_utility[i] = Rcpp::as<double>(gamma_utility[1]);

////////////////////////////////// [ eta ] /////////////////////////////////////

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

////////////////////////// [ Rescorla-Wagner Model ] ///////////////////////////

    if (ISNA(initial_value) && Occurrence[i] == 0) { eta_col[i] = 1; }
    V_update[i] = V_value[i] + eta_col[i] * (R_utility[i] - V_value[i]);
    Rcpp::as<Rcpp::NumericVector>(option_cols[choose_idx])[i] = V_update[i];
  }

////////////////////////////////// [ fill data ] ///////////////////////////////

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