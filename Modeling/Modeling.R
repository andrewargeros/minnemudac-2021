library(tidyverse)
library(tidymodels)
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
  transform(winner = as.factor(winner))

## Data Partitioning ------------------------------------------------------------------------------

not_valid = data %>% filter(season < 2019)
valid = data %>% filter(season == 2019) # Save 2019 Bracket as Final Validation

split = not_valid %>% initial_split(prop = 0.8)
train = training(split)
test = testing(split)

## Create Model Recipe ----------------------------------------------------------------------------

model_recipe = recipe(winner ~ ., data = train) %>% 
  update_role(team_1_id, new_role = 'ID') %>% 
  update_role(team_2_id, new_role = 'ID') %>%
  update_role(game_slot, new_role = 'ID') %>%
  step_knnimpute(all_predictors()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep(train, retain = T)
  
## Model Attempt ----------------------------------------------------------------------------------

rf_model = rand_forest(mtry = 80, trees = 20000) %>% 
  set_engine("ranger", importance = 'impurity') %>% 
  set_mode("classification")

wf = workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(model_recipe)

model_fit = wf %>% fit(train)

## Evaluation -------------------------------------------------------------------------------------

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

upset = data %>% 
  mutate(up = case_when(winner == 'team_1' & team_1_seed_num > team_2_seed_num ~ 'UPSET',
                           winner == 'team_2' & team_2_seed_num > team_1_seed_num ~ 'UPSET',
                           T ~ 'NORMAL') %>% as.factor()) %>% 
  select(-winner) %>% 
  mutate_if(is.character, factor)

## Data Partitioning ------------------------------------------------------------------------------

not_valid = upset %>% filter(season < 2019)
valid = upset %>% filter(season == 2019) # Save 2019 Bracket as Final Validation

split = not_valid %>% initial_split(prop = 0.8)
train = training(split)
test = testing(split)

## Model Recipe -----------------------------------------------------------------------------------

upset_recipe = recipe(up ~ ., data = train) %>% 
  update_role(team_1_id, new_role = 'ID') %>% 
  update_role(team_2_id, new_role = 'ID') %>%
  update_role(game_slot, new_role = 'ID') %>%
  step_knnimpute(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_upsample(up) %>%
  prep(train, retain = T)

## Modeling ----------------------------------------------------------------------------------------

knn = nearest_neighbor(neighbors = 80) %>% 
  set_engine('kknn') %>% 
  set_mode('classification')

wf_up = workflow() %>% 
  add_model(knn) %>% 
  add_recipe(upset_recipe)

upset_fit = wf_up %>% fit(train)

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
