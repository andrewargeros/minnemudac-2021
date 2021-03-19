library(tidyverse)
library(tidymodels)
library(magrittr)
library(glue)

games = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\brackets_2021.csv") %>% 
  filter(str_detect(game_slot, 'R'))
stats = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\team_stats.csv")
names = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MDataFiles_Stage2\\MTeams.csv") %>% 
  janitor::clean_names('snake')

data = games %>% 
  filter(!is.na(team_1_id) & !is.na(team_2_id)) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_1_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              select(-season),
            by = c('team_1_id' = 'team_id')) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_2_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              top_n(1, row_number()) %>% 
              select(-season), 
            by = c('team_2_id' = 'team_id')) %>% 
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
  mutate(up = NA, winner = NA) %>%
  mutate_if(is.character, factor)  

## Load Models ------------------------------------------------------------------------------------

stage_1 = read_rds("C:\\RScripts\\minnemudac-2021\\Models\\model_step_1.rds")
stage_2 = read_rds("C:\\RScripts\\minnemudac-2021\\Models\\model_step_2.rds")

## Bootstrap Function -----------------------------------------------------------------------------

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

## Pipeline ---------------------------------------------------------------------------------------

predictions = stage_1 %>% 
  random_predict(data, 15) %>% 
  bind_cols(stage_1 %>% predict(data)) %>% 
  bind_cols(stage_1 %>% predict(data, type = 'prob')) %>% 
  rename('boot' = 1, 
         'full' = 2, 
         'rf_prob_team_1' = 3,
         'rf_prob_team_2' = 4) %>% 
  mutate(certainty = ifelse(full == 'team_1', rf_prob_team_1, rf_prob_team_2)) %>% 
  select(-c(rf_prob_team_1, rf_prob_team_2)) %>% 
  bind_cols(stage_2 %>% 
              predict(data, type = 'prob') %>%
              bind_cols(stage_2 %>% predict(data)) %>% 
              bind_cols(data %>% select(winner, up, team_1_seed_num, team_2_seed_num)) %>% 
              mutate(knn_pred = case_when(`.pred_class` == 'UPSET' & team_1_seed_num > team_2_seed_num ~ 'team_1',
                                          `.pred_class` == 'UPSET' & team_2_seed_num >= team_1_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_1_seed_num > team_2_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_2_seed_num >= team_1_seed_num ~ 'team_1'),
                     knn_certainty = ifelse(`.pred_class` == 'NORMAL', `.pred_NORMAL`, `.pred_UPSET`)) %>% 
              select(knn_pred, knn_certainty)) %>% 
  mutate_if(is.character, factor) %>% 
  bind_cols(data %>% select(team_1_id, team_1_seed_num, team_2_id, team_2_seed_num)) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_1_name' = 2), 
             by = c('team_1_id' = 'team_id')) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_2_name' = 2), 
             by = c('team_2_id' = 'team_id'))

## Round 2 ----------------------------------------------------------------------------------------

second_round = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\bracket_round2.csv") 

data = second_round %>% 
  mutate(team_1_id = as.character(team_1_id)) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_1_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>%
              select(-season) %>% 
              mutate(team_id = as.character(team_id)),
            by = c('team_1_id' = 'team_id')) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_2_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>% 
              select(-season), 
            by = c('team_2_id' = 'team_id')) %>% 
  mutate(off_eff_diff = team_1_off_rate - team_2_off_rate,
         def_eff_diff = team_1_def_rate - team_2_def_rate,
         rate_diff_diff = team_1_rate_diff - team_2_rate_diff, 
         seed_diff = team_1_seed_num - team_2_seed_num) %>% 
  mutate(across(11:last_col(), .fns = list(log = ~ifelse(.x > 0, log(.x), .x),
                                           sqrt = ~ifelse(.x > 0, sqrt(.x), .x)))) %>%
  mutate(across(ends_with('_log'), ~replace_na(.x, 0)),
         across(ends_with('_sqrt'), ~replace_na(.x, 0))) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  transform(winner = as.factor(winner),
            team_1_id = as.numeric(team_1_id)) %>% 
  mutate(winner = NA, up = NA) %>% 
  mutate_if(is.character, factor) 

predictions = stage_1 %>% 
  random_predict(data, 15) %>% 
  bind_cols(stage_1 %>% predict(data)) %>% 
  bind_cols(stage_1 %>% predict(data, type = 'prob')) %>% 
  rename('boot' = 1, 
         'full' = 2, 
         'rf_prob_team_1' = 3,
         'rf_prob_team_2' = 4) %>% 
  mutate(certainty = ifelse(full == 'team_1', rf_prob_team_1, rf_prob_team_2)) %>% 
  select(-c(rf_prob_team_1, rf_prob_team_2)) %>% 
  bind_cols(stage_2 %>% 
              predict(data, type = 'prob') %>%
              bind_cols(stage_2 %>% predict(data)) %>% 
              bind_cols(data %>% select(winner, up, team_1_seed_num, team_2_seed_num)) %>% 
              mutate(knn_pred = case_when(`.pred_class` == 'UPSET' & team_1_seed_num > team_2_seed_num ~ 'team_1',
                                          `.pred_class` == 'UPSET' & team_2_seed_num >= team_1_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_1_seed_num > team_2_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_2_seed_num >= team_1_seed_num ~ 'team_1'),
                     knn_certainty = ifelse(`.pred_class` == 'NORMAL', `.pred_NORMAL`, `.pred_UPSET`)) %>% 
              select(knn_pred, knn_certainty)) %>% 
  mutate_if(is.character, factor) %>% 
  bind_cols(data %>% select(team_1_id, team_1_seed_num, team_2_id, team_2_seed_num)) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_1_name' = 2), 
             by = c('team_1_id' = 'team_id')) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_2_name' = 2), 
             by = c('team_2_id' = 'team_id'))

## Sweet Sixteen ----------------------------------------------------------------------------------

data = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\bracket_round3.csv") %>% 
  mutate(team_1_id = as.character(team_1_id)) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_1_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>%
              select(-season) %>% 
              mutate(team_id = as.character(team_id)),
            by = c('team_1_id' = 'team_id')) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_2_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>% 
              select(-season), 
            by = c('team_2_id' = 'team_id')) %>% 
  mutate(off_eff_diff = team_1_off_rate - team_2_off_rate,
         def_eff_diff = team_1_def_rate - team_2_def_rate,
         rate_diff_diff = team_1_rate_diff - team_2_rate_diff, 
         seed_diff = team_1_seed_num - team_2_seed_num) %>% 
  mutate(across(11:last_col(), .fns = list(log = ~ifelse(.x > 0, log(.x), .x),
                                           sqrt = ~ifelse(.x > 0, sqrt(.x), .x)))) %>%
  mutate(across(ends_with('_log'), ~replace_na(.x, 0)),
         across(ends_with('_sqrt'), ~replace_na(.x, 0))) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  transform(winner = as.factor(winner),
            team_1_id = as.numeric(team_1_id)) %>% 
  mutate(winner = NA, up = NA) %>% 
  mutate_if(is.character, factor) 

predictions = stage_1 %>% 
  random_predict(data, 15) %>% 
  bind_cols(stage_1 %>% predict(data)) %>% 
  bind_cols(stage_1 %>% predict(data, type = 'prob')) %>% 
  rename('boot' = 1, 
         'full' = 2, 
         'rf_prob_team_1' = 3,
         'rf_prob_team_2' = 4) %>% 
  mutate(certainty = ifelse(full == 'team_1', rf_prob_team_1, rf_prob_team_2)) %>% 
  select(-c(rf_prob_team_1, rf_prob_team_2)) %>% 
  bind_cols(stage_2 %>% 
              predict(data, type = 'prob') %>%
              bind_cols(stage_2 %>% predict(data)) %>% 
              bind_cols(data %>% select(winner, up, team_1_seed_num, team_2_seed_num)) %>% 
              mutate(knn_pred = case_when(`.pred_class` == 'UPSET' & team_1_seed_num > team_2_seed_num ~ 'team_1',
                                          `.pred_class` == 'UPSET' & team_2_seed_num >= team_1_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_1_seed_num > team_2_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_2_seed_num >= team_1_seed_num ~ 'team_1'),
                     knn_certainty = ifelse(`.pred_class` == 'NORMAL', `.pred_NORMAL`, `.pred_UPSET`)) %>% 
              select(knn_pred, knn_certainty)) %>% 
  mutate_if(is.character, factor) %>% 
  bind_cols(data %>% select(team_1_id, team_1_seed_num, team_2_id, team_2_seed_num)) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_1_name' = 2), 
             by = c('team_1_id' = 'team_id')) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_2_name' = 2), 
             by = c('team_2_id' = 'team_id'))

## Elite Eight ------------------------------------------------------------------------------------

data = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\bracket_round4.csv") %>% 
  mutate(team_1_id = as.character(team_1_id)) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_1_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>%
              select(-season) %>% 
              mutate(team_id = as.character(team_id)),
            by = c('team_1_id' = 'team_id')) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_2_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>% 
              select(-season), 
            by = c('team_2_id' = 'team_id')) %>% 
  mutate(off_eff_diff = team_1_off_rate - team_2_off_rate,
         def_eff_diff = team_1_def_rate - team_2_def_rate,
         rate_diff_diff = team_1_rate_diff - team_2_rate_diff, 
         seed_diff = team_1_seed_num - team_2_seed_num) %>% 
  mutate(across(11:last_col(), .fns = list(log = ~ifelse(.x > 0, log(.x), .x),
                                           sqrt = ~ifelse(.x > 0, sqrt(.x), .x)))) %>%
  mutate(across(ends_with('_log'), ~replace_na(.x, 0)),
         across(ends_with('_sqrt'), ~replace_na(.x, 0))) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  transform(winner = as.factor(winner),
            team_1_id = as.numeric(team_1_id)) %>% 
  mutate(winner = NA, up = NA) %>% 
  mutate_if(is.character, factor) 

predictions = stage_1 %>% 
  random_predict(data, 15) %>% 
  bind_cols(stage_1 %>% predict(data)) %>% 
  bind_cols(stage_1 %>% predict(data, type = 'prob')) %>% 
  rename('boot' = 1, 
         'full' = 2, 
         'rf_prob_team_1' = 3,
         'rf_prob_team_2' = 4) %>% 
  mutate(certainty = ifelse(full == 'team_1', rf_prob_team_1, rf_prob_team_2)) %>% 
  select(-c(rf_prob_team_1, rf_prob_team_2)) %>% 
  bind_cols(stage_2 %>% 
              predict(data, type = 'prob') %>%
              bind_cols(stage_2 %>% predict(data)) %>% 
              bind_cols(data %>% select(winner, up, team_1_seed_num, team_2_seed_num)) %>% 
              mutate(knn_pred = case_when(`.pred_class` == 'UPSET' & team_1_seed_num > team_2_seed_num ~ 'team_1',
                                          `.pred_class` == 'UPSET' & team_2_seed_num >= team_1_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_1_seed_num > team_2_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_2_seed_num >= team_1_seed_num ~ 'team_1'),
                     knn_certainty = ifelse(`.pred_class` == 'NORMAL', `.pred_NORMAL`, `.pred_UPSET`)) %>% 
              select(knn_pred, knn_certainty)) %>% 
  mutate_if(is.character, factor) %>% 
  bind_cols(data %>% select(team_1_id, team_1_seed_num, team_2_id, team_2_seed_num)) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_1_name' = 2), 
             by = c('team_1_id' = 'team_id')) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_2_name' = 2), 
             by = c('team_2_id' = 'team_id'))

## Final Four -------------------------------------------------------------------------------------

data = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\bracket_round5.csv") %>% 
  mutate(team_1_id = as.character(team_1_id)) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_1_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>%
              select(-season) %>% 
              mutate(team_id = as.character(team_id)),
            by = c('team_1_id' = 'team_id')) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_2_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>% 
              select(-season), 
            by = c('team_2_id' = 'team_id')) %>% 
  mutate(off_eff_diff = team_1_off_rate - team_2_off_rate,
         def_eff_diff = team_1_def_rate - team_2_def_rate,
         rate_diff_diff = team_1_rate_diff - team_2_rate_diff, 
         seed_diff = team_1_seed_num - team_2_seed_num) %>% 
  mutate(across(11:last_col(), .fns = list(log = ~ifelse(.x > 0, log(.x), .x),
                                           sqrt = ~ifelse(.x > 0, sqrt(.x), .x)))) %>%
  mutate(across(ends_with('_log'), ~replace_na(.x, 0)),
         across(ends_with('_sqrt'), ~replace_na(.x, 0))) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  transform(winner = as.factor(winner),
            team_1_id = as.numeric(team_1_id)) %>% 
  mutate(winner = NA, up = NA) %>% 
  mutate_if(is.character, factor) 

predictions = stage_1 %>% 
  random_predict(data, 15) %>% 
  bind_cols(stage_1 %>% predict(data)) %>% 
  bind_cols(stage_1 %>% predict(data, type = 'prob')) %>% 
  rename('boot' = 1, 
         'full' = 2, 
         'rf_prob_team_1' = 3,
         'rf_prob_team_2' = 4) %>% 
  mutate(certainty = ifelse(full == 'team_1', rf_prob_team_1, rf_prob_team_2)) %>% 
  select(-c(rf_prob_team_1, rf_prob_team_2)) %>% 
  bind_cols(stage_2 %>% 
              predict(data, type = 'prob') %>%
              bind_cols(stage_2 %>% predict(data)) %>% 
              bind_cols(data %>% select(winner, up, team_1_seed_num, team_2_seed_num)) %>% 
              mutate(knn_pred = case_when(`.pred_class` == 'UPSET' & team_1_seed_num > team_2_seed_num ~ 'team_1',
                                          `.pred_class` == 'UPSET' & team_2_seed_num >= team_1_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_1_seed_num > team_2_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_2_seed_num >= team_1_seed_num ~ 'team_1'),
                     knn_certainty = ifelse(`.pred_class` == 'NORMAL', `.pred_NORMAL`, `.pred_UPSET`)) %>% 
              select(knn_pred, knn_certainty)) %>% 
  mutate_if(is.character, factor) %>% 
  bind_cols(data %>% select(team_1_id, team_1_seed_num, team_2_id, team_2_seed_num)) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_1_name' = 2), 
             by = c('team_1_id' = 'team_id')) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_2_name' = 2), 
             by = c('team_2_id' = 'team_id'))

## Championship -----------------------------------------------------------------------------------

data = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\bracket_round6.csv") %>% 
  mutate(team_1_id = as.character(team_1_id)) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_1_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>%
              select(-season) %>% 
              mutate(team_id = as.character(team_id)),
            by = c('team_1_id' = 'team_id')) %>% 
  left_join(., stats %>% 
              filter(season == 2020) %>% 
              select(-day_num) %>% 
              rename_with(~glue('team_2_{.x}')) %>% 
              rename('team_id' = 1,
                     'season' = 2) %>% 
              # top_n(1, row_number()) %>% 
              select(-season), 
            by = c('team_2_id' = 'team_id')) %>% 
  mutate(off_eff_diff = team_1_off_rate - team_2_off_rate,
         def_eff_diff = team_1_def_rate - team_2_def_rate,
         rate_diff_diff = team_1_rate_diff - team_2_rate_diff, 
         seed_diff = team_1_seed_num - team_2_seed_num) %>% 
  mutate(across(11:last_col(), .fns = list(log = ~ifelse(.x > 0, log(.x), .x),
                                           sqrt = ~ifelse(.x > 0, sqrt(.x), .x)))) %>%
  mutate(across(ends_with('_log'), ~replace_na(.x, 0)),
         across(ends_with('_sqrt'), ~replace_na(.x, 0))) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  transform(winner = as.factor(winner),
            team_1_id = as.numeric(team_1_id)) %>% 
  mutate(winner = NA, up = NA) %>% 
  mutate_if(is.character, factor) 

predictions = stage_1 %>% 
  random_predict(data, 15) %>% 
  bind_cols(stage_1 %>% predict(data)) %>% 
  bind_cols(stage_1 %>% predict(data, type = 'prob')) %>% 
  rename('boot' = 1, 
         'full' = 2, 
         'rf_prob_team_1' = 3,
         'rf_prob_team_2' = 4) %>% 
  mutate(certainty = ifelse(full == 'team_1', rf_prob_team_1, rf_prob_team_2)) %>% 
  select(-c(rf_prob_team_1, rf_prob_team_2)) %>% 
  bind_cols(stage_2 %>% 
              predict(data, type = 'prob') %>%
              bind_cols(stage_2 %>% predict(data)) %>% 
              bind_cols(data %>% select(winner, up, team_1_seed_num, team_2_seed_num)) %>% 
              mutate(knn_pred = case_when(`.pred_class` == 'UPSET' & team_1_seed_num > team_2_seed_num ~ 'team_1',
                                          `.pred_class` == 'UPSET' & team_2_seed_num >= team_1_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_1_seed_num > team_2_seed_num ~ 'team_2',
                                          `.pred_class` == 'NORMAL' & team_2_seed_num >= team_1_seed_num ~ 'team_1'),
                     knn_certainty = ifelse(`.pred_class` == 'NORMAL', `.pred_NORMAL`, `.pred_UPSET`)) %>% 
              select(knn_pred, knn_certainty)) %>% 
  mutate_if(is.character, factor) %>% 
  bind_cols(data %>% select(team_1_id, team_1_seed_num, team_2_id, team_2_seed_num)) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_1_name' = 2), 
             by = c('team_1_id' = 'team_id')) %>% 
  inner_join(., names %>% 
               select(team_id, team_name) %>% 
               rename('team_2_name' = 2), 
             by = c('team_2_id' = 'team_id'))
