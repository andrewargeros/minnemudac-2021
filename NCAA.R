library(tidyverse)
library(magrittr)
library(glue)

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
  mutate(id = map(row_number(), uuid::UUIDgenerate) %>% as.character())

locations = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\MGameCities.csv") %>% 
  inner_join(., read_csv('C:\\RScripts\\minnemudac-2021\\Data\\Cities.csv'), by = "CityID") %>% 
  janitor::clean_names('snake') %>% 
  mutate(city_st = glue("{city}, {state}") %>% as.character())

w_team_avg = reg_seas %>% 
  select(!starts_with('l') & !starts_with('greater')) %>% 
  rename_with(~str_remove(.x, 'w_'), starts_with("w_")) %>% 
  rename_with(~str_remove(.x, "^w"), starts_with('w')) %>% 
  mutate(w_l = 'W') %>% 
  bind_rows(reg_seas %>% 
              select((!starts_with('w') & !starts_with('greater'))) %>%  
              rename_with(~str_remove(.x, 'l_'), starts_with("l_")) %>% 
              rename_with(~str_remove(.x, "^l"), starts_with('l')) %>%
              inner_join(., reg_seas %>% select(id, w_loc), by = "id") %>% 
              rename("loc" = w_loc,
                     "ordr_pct" = wordr_pct) %>% 
              mutate(loc = case_when(loc == "H" ~ "A",
                                     loc == "A" ~ "H",
                                     loc == "N" ~ "N")) %>% 
              mutate(w_l = "L")) %>% 
  relocate(id, .before = season) %>% 
  relocate(w_l, .before = team_id) %>% 
  arrange(id) %>% 
  inner_join(., reg_seas %>% 
                 select(id, w_team_id, l_team_id, season, day_num) %>% 
                 inner_join(., locations) %>% 
                 select(!c(w_team_id, l_team_id, season, day_num)), by = "id")

team_loc_wl_stats = w_team_avg %>% 
  group_by(team_id, season, loc, w_l) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  arrange(season, team_id, loc, w_l)


w_team_avg %>% # home team advantage doesn't seem to be a huge deal
  group_by(loc, w_l) %>% 
  summarise(s = mean(score)) %>% 
  ggplot() +
  aes(x = loc, y = s, fill = w_l) +
  geom_bar(stat = "identity") +
  facet_wrap(~w_l) +
  theme_minimal()

w_team_avg %>% 
  ggplot() +
  aes(x = to, y = score, color = w_l) +
  geom_jitter()
