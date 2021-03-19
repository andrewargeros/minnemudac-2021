library(tidyverse)
library(tidymodels)

games = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\brackets_2021.csv") %>% 
  filter(str_detect(game_slot, 'R'))
stats = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\team_stats.csv")

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

stage_1 = read_rds()
