library(tidyverse)
library(glue)


ncaa_results = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MNCAATourneyDetailedResults.csv") %>% 
  janitor::clean_names('snake')

ncaa_seeds = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MNCAATourneySeeds.csv") %>% 
  janitor::clean_names('snake') %>% 
  filter(season >= 2003) %>% 
  mutate(region = str_extract(seed, '^[A-Z]'),
         seed = str_extract(seed, '\\d{2}') %>% as.numeric())

ncaa_slots = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MNCAATourneySlots.csv") %>% 
  janitor::clean_names('snake') %>% 
  filter(season >= 2003)

ncaa_round_slots = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\Kaggle Data\\MNCAATourneySeedRoundSlots.csv") %>% 
  janitor::clean_names('snake') %>% 
  filter(season >= 2003)

ncaa_results %>% 
  select(season, day_num, w_team_id, l_team_id) %>% 
  inner_join(., ncaa_seeds, by = c('season', 'w_team_id' = 'team_id')) %>% 
  rename('w_seed' = seed) %>% 
  inner_join(., ncaa_seeds, by = c('season', 'l_team_id' = 'team_id', 'region'))
