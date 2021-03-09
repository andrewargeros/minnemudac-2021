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
                 select(!c(w_team_id, l_team_id, season, day_num)), by = "id") %>% 
  ungroup() %>% 
  group_by(team_id, season) %>% 
  mutate(all_games = n_distinct(id)) %>% 
  ungroup() %>% 
  group_by(team_id, season, w_l) %>% 
  mutate(games = n_distinct(id))

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

conferences = read_csv("C:\\RScripts\\minnemudac-2021\\Data\\MTeamConferences.csv") %>% 
  inner_join(., read_csv('C:\\RScripts\\minnemudac-2021\\Data\\Conferences.csv')) %>% 
  inner_join(., read_csv('C:\\RScripts\\minnemudac-2021\\Data\\MTeams.csv')) %>% 
  janitor::clean_names('snake') %>% 
  mutate(desc_clean = str_remove(description, 'Conference')) %>% 
  mutate(team_name2 = str_replace(team_name, 'Univ$', 'University') %>% 
                        str_replace(' St', ' State') %>% 
                        str_replace('Stateate', 'State')) %>% 
  filter(last_d1season >= 2003) %>% 
  arrange(description) %>% 
  left_join(., read_csv("C:\\RScripts\\minnemudac-2021\\Back End Data\\sports_reference_teams.csv") %>% 
                mutate(team = str_replace_all(team, '&amp;', '&')), by = c('team_name2' = 'team')) %>% 
  left_join(., read_csv('C:\\RScripts\\minnemudac-2021\\Back End Data\\matched_names.csv'), 
            by = c('team_name2' = 'TeamName')) %>% 
  mutate(sref_name = ifelse(is.na(link), team, team_name2))

conferences %>% 
  filter(is.na(sref_name)) %>% 
  select(team_name) %>% 
  unique() # check missing data

player_stats = read_csv('C:\\RScripts\\minnemudac-2021\\Back End Data\\player_stats_detail_0321.csv') %>% 
  left_join(., read_csv('C:\\RScripts\\minnemudac-2021\\Back End Data\\player_stats_0321.csv'),
            by = c('season' = 'year', 'team' = 'team', 'Player' = 'Player')) %>% 
  janitor::clean_names('snake') %>% 
  select(-rk) %>% 
  mutate(height = 12*(str_extract(height, '^\\d') %>% as.numeric()) + (str_extract(height, '\\d$') %>% as.numeric())) %>% 
  # mutate(across)
  mutate(x3pt_pt_pct = x3p/(x3p + x2p)) %>% 
  mutate(across(2:26, ~g*.x, .names = "ev_{.col}")) %>% 
  mutate(s_eff = (ev_pts + ev_ast + ev_trb + ev_blk - (ev_x2pa - ev_x2p) - (ev_x3pa - ev_x3p) - (ev_fta - ev_ft) - ev_tov),
         pg_eff = (pts + ast + trb + blk - (x2pa - x2p) - (x3pa - x3p) - (fta - ft) - tov)) %>% 
  group_by(season) %>% 
  mutate(across(where(is.numeric), ~ntile(.x, n = 100), .names = "season_{.col}_pctile")) %>% 
  relocate(team, .before = player) %>% 
  relocate(season, .before = team) %>% 
  mutate(rsci_top_100 = str_remove_all(rsci_top_100, " \\(.*?\\)$") %>% as.numeric(),
         rsci_top_100 = replace_na(rsci_top_100, 999))

team_level_player_stats = player_stats %>% 
  group_by(season, team) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = T))) %>% 
  # inner_join(., player_stats %>% 
  #                 group_by(season, team) %>% 
  #                 summarise(across(where(is.numeric), ~mean(.x, na.rm = T))) %>% 
  #                 ungroup() %>% 
  #                 group_by(team) %>% 
  #                 summarise(across(where(is.numeric), cummean, .names = 'cum_{.col}')) %>% 
  #                 filter(cum_season %% 1 == 0.5) %>% 
  #                 mutate(cum_season = as.integer(cum_season + 0.5)) %>% 
  #                 rename("season" = cum_season),
  #            by = c('team', 'season')) %>%  # this is causing either multiple NAs or data to be cutoff at 2016
  left_join(., player_stats %>% 
                  select(season, team, player, class, gs) %>% 
                  group_by(season, team) %>% 
                  mutate(rank = row_number(-gs)) %>% 
                  filter(rank <= 5) %>% 
                  ungroup() %>% 
                  group_by(season, team, class) %>% 
                  summarise(n = n_distinct(player)) %>% 
                  mutate(class = case_when(class == "FR" ~ 'freshman',
                                           class == "SO" ~ 'sophomore',
                                           class == "JR" ~ 'junior',
                                           class == 'SR' ~ 'senior',
                                           T ~ 'unknown_class')) %>% 
                  pivot_wider(names_from = class, values_from = n) %>% 
                  mutate(across(everything(), ~replace_na(.x, 0))),
            by = c('team', 'season')) # bind this to `w_team_avg` once both are wrangled

# Season Average Data For Missing Team/Season from Above

na_season_data = team_level_player_stats %>% 
  group_by(season) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = T)))

# Ken Pom Ratings Data 

kenpom = read_csv('C:\\RScripts\\minnemudac-2021\\Back End Data\\kenpom_ratings0321.csv') %>%
  janitor::clean_names('snake') %>% 
  filter(!str_detect(rk, 'R')) %>% 
  mutate(team_clean = str_remove_all(team, '\\d') %>% 
                      str_remove_all('\\.') %>% 
                      str_remove_all('\\*') %>% 
                      str_replace_all(' St$| St ', ' State ') %>% 
                      str_trim()) %>% 
  mutate(across(everything(), ~str_remove_all(.x, '\\+'))) %>% 
  mutate(across(5:22, as.numeric)) %>% 
  select(!ends_with('_sub'))

test = conferences %>% 
  select(sref_name) %>%
  distinct() %>% 
  left_join(., kenpom %>% 
                select(team_clean) %>% 
                distinct(),
            by = c('sref_name' = 'team_clean'),
            keep = T)
kp_names = kenpom %>% select(team_clean) %>% distinct()
