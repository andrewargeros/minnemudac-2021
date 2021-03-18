library(tidyverse)
library(glue)


ncaa_results = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MDataFiles_Stage2\\MNCAATourneyDetailedResults.csv") %>% 
  janitor::clean_names('snake')

ncaa_seeds = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MDataFiles_Stage2\\MNCAATourneySeeds.csv") %>% 
  janitor::clean_names('snake') %>% 
  filter(season >= 2003) %>% 
  mutate(region = str_extract(seed, '^[A-Z]'),
         seed_num = str_extract(seed, '\\d{2}') %>% as.numeric())

ncaa_slots = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MDataFiles_Stage2\\MNCAATourneySlots.csv") %>% 
  janitor::clean_names('snake')

ncaa_round_slots = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MDataFiles_Stage2\\MNCAATourneySeedRoundSlots.csv") %>% 
  janitor::clean_names('snake')

brackets = ncaa_slots %>% 
  filter(season == 2021) %>% 
  inner_join(., ncaa_seeds %>% 
                 filter(season == 2021) %>% 
                 rename('high_team_id' = team_id,
                        'high_seed_num' = seed_num), 
             by = c('season', 'strong_seed' = 'seed')) %>% 
  left_join(., ncaa_seeds %>% 
                filter(season == 2021) %>% 
                select(-region) %>% 
                rename('low_team_id' = team_id,
                       'low_seed_num' = seed_num), 
            by = c('season', 'weak_seed' = 'seed')) %>% 
  mutate(low_seed_num = case_when(is.na(low_seed_num) & high_seed_num == 1 ~ 16,
                                  is.na(low_seed_num) & high_seed_num == 6 ~ 11,
                                  T ~ low_seed_num)) %>% 
  inner_join(., ncaa_round_slots %>% select(game_slot, early_day_num), by = c('slot' = 'game_slot')) %>% 
  mutate(team_1_id = ifelse(high_team_id %% 2 == 0, high_team_id, low_team_id),
         team_2_id = ifelse(high_team_id %% 2 == 0, low_team_id, high_team_id),
         team_1_seed_num = ifelse(high_team_id %% 2 == 0, high_seed_num, low_seed_num),
         team_2_seed_num = ifelse(high_team_id %% 2 == 0, low_seed_num, high_seed_num),
         day_num = early_day_num,
         game_slot = slot,
         game_round = 1,
         winner = NA) %>% 
  select(season, day_num, region, game_round, game_slot, winner, team_1_id, team_2_id, team_1_seed_num, team_2_seed_num) %>% 
  distinct()

brackets %>% write_csv('C:\\RScripts\\minnemudac-2021\\Data\\Final Output Data\\brackets_2021.csv')
