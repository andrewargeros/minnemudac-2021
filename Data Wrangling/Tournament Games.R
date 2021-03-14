library(tidyverse)
library(glue)


ncaa_results = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MNCAATourneyDetailedResults.csv") %>% 
  janitor::clean_names('snake')

ncaa_seeds = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MNCAATourneySeeds.csv") %>% 
  janitor::clean_names('snake') %>% 
  filter(season >= 2003) %>% 
  mutate(region = str_extract(seed, '^[A-Z]'),
         seed_num = str_extract(seed, '\\d{2}') %>% as.numeric())

ncaa_slots = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MNCAATourneySlots.csv") %>% 
  janitor::clean_names('snake') %>% 
  filter(season >= 2003)

ncaa_round_slots = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MNCAATourneySeedRoundSlots.csv") %>% 
  janitor::clean_names('snake') %>% 
  filter(season >= 2003)

brackets = ncaa_results %>% 
  select(season, day_num, w_team_id, l_team_id) %>% 
  mutate(team_1_id = ifelse(w_team_id %% 2 == 0, w_team_id, l_team_id),
         team_2_id = ifelse(w_team_id %% 2 == 0, l_team_id, w_team_id),
         winner = ifelse(team_1_id == w_team_id, 'team_1', 'team_2') %>% as.factor()) %>% 
  select(-c(w_team_id, l_team_id)) %>% 
  left_join(., ncaa_seeds, by = c('season', 'team_1_id' = 'team_id')) %>%
  rename('team_1_seed' = seed,
         'team_1_seed_num' = seed_num) %>%
  left_join(., ncaa_seeds %>% select(-region), by = c('season', 'team_2_id' = 'team_id')) %>%
  rename('team_2_seed' = seed,
         'team_2_seed_num' = seed_num) %>% 
  left_join(., ncaa_round_slots, by = c('team_1_seed' = 'seed')) %>% 
  filter(day_num >= early_day_num, 
         day_num <= late_day_num) %>% 
  select(season, day_num, region, game_round, game_slot, winner, team_1_id, team_2_id, team_1_seed_num, team_2_seed_num)

brackets %>% write_csv('C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\brackets.csv')
