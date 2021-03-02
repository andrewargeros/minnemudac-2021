library(tidyverse)
library(magrittr)

ncaa_results = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\MNCAATourneyDetailedResults.csv") %>% 
  janitor::clean_names('snake')

reg_seas = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\MRegularSeasonDetailedResults.csv") %>% 
  janitor::clean_names('snake') %>% 
  mutate(w_fg_pct = wfgm/wfga,
         w_fg3_pct = wfgm3/wfga3,
         w_ft_pct = wftm/wfta,
         w_ordr_pct = wor/wdr,
         l_fg_pct = lfgm/lfga,
         l_fg3_pct = lfgm3/lfga3,
         l_ft_pct = lftm/lfta,
         l_wordr_pct = lor/ldr,
         fgm_diff = wfgm-lfgm,
         fga_diff = wfga-lfga,
         fg3m_diff = wfgm3 - lfga3,
         fg3a_diff = wfga3 - lfga3,
         to_diff = wto - lto,
         stl_diff = w_stl - l_stl,
         ast_diff = w_ast - l_ast,
         blk_diff = w_blk - l_blk,
         or_diff = wor - lor,
         dr_diff = wdr - ldr,
         pf_diff = wpf - lpf,
         score_diff = w_score - l_score) %>% 
  mutate(across(ends_with('_diff'), ~case_when(.x > 0 ~ "W", .x < 0 ~ "L", T ~ "Tie"), .names = "greater_{.col}")) %>% 
  mutate(id = uuid::UUIDgenerate())


w_team_avg = reg_seas %>% 
  select(!starts_with('l')) %>% 
  select(!c(season, day_num)) %>% 
  rename_with(~str_remove(.x, 'w_'), starts_with("w_")) %>% 
  rename_with(~str_remove(.x, "^w"), starts_with('w')) %>% 
  group_by(team_id, loc) %>%
  mutate(n_games = n_distinct(id)) %>% 
  summarise(across(everything(), mean))
  

         