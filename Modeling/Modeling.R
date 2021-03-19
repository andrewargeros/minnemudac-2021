library(tidyverse)
library(tidymodels)
library(magrittr)
library(themis)
library(kknn)
library(glue)

## Data Loading and Munging -----------------------------------------------------------------------

games = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\brackets.csv")

stats = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\team_stats.csv")

data = games %>% 
  left_join(., stats %>% 
                select(-day_num) %>% 
                rename_with(~glue('team_1_{.x}')) %>% 
                rename('team_id' = 1,
                       'season' = 2), 
            by = c('season', 'team_1_id' = 'team_id')) %>% 
  left_join(., stats %>% 
                select(-day_num) %>% 
                rename_with(~glue('team_2_{.x}')) %>% 
                rename('team_id' = 1,
                       'season' = 2), 
            by = c('season', 'team_2_id' = 'team_id')) %>% 
  mutate(off_eff_diff = team_1_off_rate - team_2_off_rate,
         def_eff_diff = team_1_def_rate - team_2_def_rate,
         rate_diff_diff = team_1_rate_diff - team_2_rate_diff, 
         seed_diff = team_1_seed_num - team_2_seed_num) %>% 
  mutate(across(11:last_col(), .fns = list(log = ~ifelse(.x > 0, log(.x), .x),
                                           sqrt = ~ifelse(.x > 0, sqrt(.x), .x)))) %>%
  mutate(across(ends_with('_log'), ~replace_na(.x, 0)),
         across(ends_with('_sqrt'), ~replace_na(.x, 0))) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  transform(winner = as.factor(winner)) %>% 
  mutate(up = case_when(winner == 'team_1' & team_1_seed_num > team_2_seed_num ~ 'UPSET',
                        winner == 'team_2' & team_2_seed_num > team_1_seed_num ~ 'UPSET',
                        T ~ 'NORMAL') %>% as.factor()) %>%
  mutate_if(is.character, factor)

## Data Partitioning ------------------------------------------------------------------------------

not_valid = data %>% filter(season < 2019)
valid = data %>% filter(season == 2019) # Save 2019 Bracket as Final Validation

split = not_valid %>% initial_split(prop = 0.8)
train = training(split)
test = testing(split)

## Create Model Recipe ----------------------------------------------------------------------------

model_recipe = recipe(winner ~ ., data = train) %>% 
  step_rm(up) %>% 
  update_role(team_1_id, new_role = 'ID') %>% 
  update_role(team_2_id, new_role = 'ID') %>%
  update_role(game_slot, new_role = 'ID') %>%
  step_knnimpute(all_predictors()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep(train, retain = T)
  
## Model Attempt ----------------------------------------------------------------------------------

rf_model = rand_forest(mtry = 80, trees = 20000) %>% 
  set_engine("randomForest") %>% 
  set_mode("classification")

wf = workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(model_recipe)

model_fit = wf %>% fit(train)

## Evaluation -------------------------------------------------------------------------------------

random_predict = function(model, new_data, n){
  preds = tibble()
  for (row in 1:nrow(new_data)){
    
    dat = new_data %>% filter(row_number() == row)
    
    all = model %>% predict(dat, 
                            type = 'raw', 
                            opts = list(predict.all = T))
    
    s = all$individual %>% 
      t() %>% 
      as_tibble() %>% 
      rename('pred' = 1) %$% 
      sample(pred, n, replace = T) %>% 
      as_tibble() %>% 
      group_by(value) %>% 
      summarise(p = n(), .groups = 'drop') %>% 
      filter(p == max(p)) %>% 
      select(value) 
    
    preds %<>% bind_rows(s)
    
  }
  return(preds)
}

preds = model_fit %>% 
  random_predict(test, 15) %>% 
  bind_cols(model_fit %>% predict(test)) %>% 
  bind_cols(model_fit %>% predict(test, type = 'prob')) %>% 
  select(-`.pred_team_2`) %>% 
  rename('boot' = 1, 'full' = 2, 'rf_prob_team_1' = 3) %>% 
  bind_cols(test %>% select(winner, team_1_seed_num, team_2_seed_num, season, game_round, up)) %>% 
  transform(boot = as.factor(boot)) %>% 
  mutate(boot_acc = ifelse(boot == winner, "RIGHT", "WRONG"),
         full_acc = ifelse(full == winner, "RIGHT", "WRONG"))

model_fit %>% 
  predict(test) %>% 
  bind_cols(test %>% select(game_round, team_1_id, team_1_seed_num,
                            team_2_id, team_2_seed_num, winner)) %>% 
  mutate(correct = ifelse(winner == `.pred_class`, 1, 0)) %>% 
  group_by(correct) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  mutate(n_pct = n/sum(n))

model_fit %>% 
  predict(test) %>% 
  bind_cols(test %>% select(game_round, team_1_id, team_1_seed_num,
                            team_2_id, team_2_seed_num, winner)) %>% 
  mutate(correct = ifelse(winner == `.pred_class`, 1, 0),
         upset = case_when(winner == 'team_1' & team_1_seed_num > team_2_seed_num ~ 1,
                           winner == 'team_2' & team_2_seed_num > team_1_seed_num ~ 1,
                           T ~ 0)) %>% 
  group_by(upset, correct) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(upset) %>%
  mutate(n_pct = n/sum(n))

## Variable Importance ----------------------------------------------------------------------------

pull_workflow_fit(model_fit)$fit$variable.importance %>% 
  enframe() %>% 
  arrange(desc(value)) %>% 
  top_n(50, wt = value) %>% 
  ggplot() +
  aes(y = reorder(name,value), x = value, fill = value) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = 'magma', begin = 0.1, end = 0.8) + 
  theme_minimal() +
  labs(title = glue("Feature Importance: NCAA Model {Sys.Date()}"),
       x = "Relative Importance",
       y = "Variable") +
  theme(legend.position = 'none')

## Fit to Second Stage Data -----------------------------------------------------------------------

model_fit = wf %>% fit(not_valid)

model_fit %>% 
  predict(valid) %>% 
  bind_cols(valid %>% select(game_round, team_1_id, team_1_seed_num,
                            team_2_id, team_2_seed_num, winner)) %>% 
  mutate(correct = ifelse(winner == `.pred_class`, 1, 0)) %>% 
  group_by(correct) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  mutate(n_pct = n/sum(n))

model_fit %>% 
  predict(valid) %>% 
  bind_cols(valid %>% select(game_round, team_1_id, team_1_seed_num,
                            team_2_id, team_2_seed_num, winner)) %>% 
  mutate(correct = ifelse(winner == `.pred_class`, 1, 0),
         upset = case_when(winner == 'team_1' & team_1_seed_num > team_2_seed_num ~ 1,
                           winner == 'team_2' & team_2_seed_num > team_1_seed_num ~ 1,
                           T ~ 0)) %>% 
  filter(upset == 1) %>% 
  arrange(correct)

## Upset Prediction Model -------------------------------------------------------------------------

## Model Recipe -----------------------------------------------------------------------------------

upset_recipe = recipe(up ~ ., data = train) %>% 
  update_role(team_1_id, new_role = 'ID') %>% 
  update_role(team_2_id, new_role = 'ID') %>%
  update_role(game_slot, new_role = 'ID') %>%
  update_role(region, new_role = 'ID') %>%
  step_rm(seed_diff, seed_diff_log, seed_diff_sqrt, winner) %>% 
  # step_zv(all_predictors()) %>%
  # step_lincomb(all_predictors()) %>%
  step_knnimpute(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_smote(up, over_ratio = 1) %>%
  prep(train, retain = T)

## Modeling ----------------------------------------------------------------------------------------

knn_mod = nearest_neighbor(neighbors = 65) %>% 
  set_engine('kknn') %>% 
  set_mode('classification')

wf_up = workflow() %>% 
  add_model(knn_mod) %>% 
  add_recipe(upset_recipe)

upset_fit = wf_up %>% fit(train)

upset_fit %>% 
  predict(test) %>% 
  bind_cols(test %>% select(game_round, team_1_id, team_1_seed_num,
                            team_2_id, team_2_seed_num, up)) %>% 
  mutate(correct = ifelse(up == `.pred_class`, 1, 0)) %>% 
  group_by(correct) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  mutate(n_pct = n/sum(n))

upset_fit %>% 
  predict(test) %>% 
  bind_cols(test %>% select(game_round, team_1_id, team_1_seed_num,
                            team_2_id, team_2_seed_num, up)) %>% 
  mutate(correct = ifelse(up == `.pred_class`, 1, 0)) %>% 
  group_by(up, correct) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  group_by(up) %>% 
  mutate(n_pct = n/sum(n))

upset_fit = wf_up %>% fit(not_valid)

upset_fit %>% 
  predict(valid) %>% 
  bind_cols(valid %>% select(game_round, team_1_id, team_1_seed_num,
                            team_2_id, team_2_seed_num, up)) %>% 
  mutate(correct = ifelse(up == `.pred_class`, 1, 0)) %>% 
  group_by(up, correct) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  group_by(up) %>% 
  mutate(n_pct = n/sum(n))

upset_fit %>% 
  predict(valid, type = 'prob') %>%
  bind_cols(upset_fit %>% predict(valid)) %>% 
  bind_cols(valid %>% select(winner, up, team_1_seed_num, team_2_seed_num)) %>% 
  mutate(knn_pred = case_when(`.pred_class` == 'UPSET' & team_1_seed_num > team_2_seed_num ~ 'team_1',
                              `.pred_class` == 'UPSET' & team_2_seed_num >= team_1_seed_num ~ 'team_2',
                              `.pred_class` == 'NORMAL' & team_1_seed_num > team_2_seed_num ~ 'team_2',
                              `.pred_class` == 'NORMAL' & team_2_seed_num >= team_1_seed_num ~ 'team_1'),
         knn_team_1_prob = ifelse(`.pred_class` == 'UPSET' & knn_pred == 'team_1', `.pred_UPSET`, `.pred_NORMAL`)) %>% 
  select(knn_pred, knn_team_1_prob)

## Ensemble Model ---------------------------------------------------------------------------------
df = model_fit %>% 
  random_predict(not_valid, 15) %>% 
  bind_cols(model_fit %>% predict(not_valid)) %>% 
  bind_cols(model_fit %>% predict(not_valid, type = 'prob')) %>% 
  rename('boot' = 1, 
         'full' = 2, 
         'rf_prob_team_1' = 3,
         'rf_prob_team_2' = 4) %>% 
  mutate(certainty = ifelse(full == 'team_1', rf_prob_team_1, rf_prob_team_2)) %>% 
  select(-c(rf_prob_team_1, rf_prob_team_2)) %>% 
  bind_cols(upset_fit %>% 
              predict(not_valid, type = 'prob') %>%
              bind_cols(upset_fit %>% predict(not_valid)) %>% 
              bind_cols(not_valid %>% select(winner, up, team_1_seed_num, team_2_seed_num)) %>% 
              mutate(knn_pred = case_when(`.pred_class` == 'UPSET' & team_1_seed_num > team_2_seed_num ~ 'team_1',
                                          `.pred_class` == 'UPSET' & team_2_seed_num >= team_1_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_1_seed_num > team_2_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_2_seed_num >= team_1_seed_num ~ 'team_1'),
                     knn_certainty = ifelse(`.pred_class` == 'NORMAL', `.pred_NORMAL`, `.pred_UPSET`)) %>% 
              select(knn_pred, knn_certainty)) %>% 
  mutate_if(is.character, factor) %>% 
  bind_cols(not_valid %>% select(winner, team_1_id, team_2_id, up))

ens_rec = recipe(winner ~ ., data = df) %>% 
  update_role(ends_with('_id'), new_role = "ID") %>% 
  update_role(up, new_role = "ID") %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  prep(df, retain = T)

model = decision_tree() %>% 
  set_mode('classification') %>% 
  set_engine('rpart')

ens_wf = workflow() %>% 
  add_model(model) %>% 
  add_recipe(ens_rec)

final_mod = ens_wf %>% fit(df)

f = model_fit %>% 
  random_predict(valid, 15) %>% 
  bind_cols(model_fit %>% predict(valid)) %>% 
  bind_cols(model_fit %>% predict(valid, type = 'prob')) %>% 
  rename('boot' = 1, 
         'full' = 2, 
         'rf_prob_team_1' = 3,
         'rf_prob_team_2' = 4) %>% 
  mutate(certainty = ifelse(full == 'team_1', rf_prob_team_1, rf_prob_team_2)) %>% 
  select(-c(rf_prob_team_1, rf_prob_team_2)) %>% 
  bind_cols(upset_fit %>% 
              predict(valid, type = 'prob') %>%
              bind_cols(upset_fit %>% predict(valid)) %>% 
              bind_cols(valid %>% select(winner, up, team_1_seed_num, team_2_seed_num)) %>% 
              mutate(knn_pred = case_when(`.pred_class` == 'UPSET' & team_1_seed_num > team_2_seed_num ~ 'team_1',
                                          `.pred_class` == 'UPSET' & team_2_seed_num >= team_1_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_1_seed_num > team_2_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_2_seed_num >= team_1_seed_num ~ 'team_1'),
                     knn_certainty = ifelse(`.pred_class` == 'NORMAL', `.pred_NORMAL`, `.pred_UPSET`)) %>% 
              select(knn_pred, knn_certainty)) %>% 
  mutate_if(is.character, factor) %>% 
  bind_cols(valid %>% select(winner, team_1_id, team_2_id, up))

final_mod %>% 
  predict(f) %>% 
  bind_cols(f %>% select(winner)) %>% 
  mutate(n = ifelse(`.pred_class` == winner, 1,0)) %>% 
  group_by(n) %>% 
  summarise(n())


## Refit Models and Save --------------------------------------------------------------------------

wf %>%
  fit(data) %>% 
  write_rds('C:\\RScripts\\minnemudac-2021\\Models\\model_step_1.rds')

wf_up %>%
  fit(data) %>% 
  write_rds('C:\\RScripts\\minnemudac-2021\\Models\\model_step_2.rds')
