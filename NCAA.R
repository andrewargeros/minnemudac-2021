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
         w_true_shoot = w_score/(2*((wfga+wfga3) + 0.44*wfta)),
         w_efg = (wfgm + wfgm3 + (0.5*wfgm3))/(wfga + wfga3),
         w_poss = 0.96*(wfga + wfga3 - wor + wto + (0.44*wfta)),
         w_o_eff = (w_score + w_ast + wor + wdr + w_blk - 
                   (wfga - wfgm) - (wfga3 - wfgm3) - (wfta - wftm) - wto),
         w_off_rate = 100*(w_score / w_poss),
         w_def_rate = 100*(l_score / w_poss),
         w_rate_diff = w_off_rate - w_def_rate,
         l_fg_pct = lfgm/lfga,
         l_fg3_pct = lfgm3/lfga3,
         l_ft_pct = lftm/lfta,
         l_wordr_pct = lor/ldr,
         l_true_shoot = l_score/(2*((lfga+lfga3) + 0.44*lfta)),
         l_efg = (lfgm + lfgm3 + (0.5*lfgm3))/(lfga + lfga3),
         l_poss = 0.96*(lfga + lfga3 - lor + lto + (0.44*lfta)),
         l_o_eff = (l_score + l_ast + lor + ldr + l_blk - 
                   (lfga - lfgm) - (lfga3 - lfgm3) - (lfta - lftm) - lto),
         l_off_rate = 100*(l_score / l_poss),
         l_def_rate = 100*(w_score / l_poss),
         l_rate_diff = l_off_rate - l_def_rate,
         off_rate_diff = w_off_rate - l_off_rate,
         def_rate_diff = w_def_rate - l_def_rate,
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

## Plots and Viz ----------------------------------------------------------------------------------

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

## Ken Pom Ratings Data ---------------------------------------------------------------------------

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

## Team Name, Conference, and Location ------------------------------------------------------------

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
  mutate(sref_name = ifelse(is.na(link), team, team_name2)) %>% 
  left_join(., name_key, by = c('sref_name' = 'scores'))

# This file joins KenPom Names to Kaggle and Sports Ref
name_link = 'https://raw.githubusercontent.com/pjmartinkus/College_Basketball/master/Data/School_Names/schools.csv'

name_key = conferences %>% 
  select(team_id, team_name, sref_name) %>% 
  distinct() %>% 
  left_join(., read_csv(name_link) %>% 
                  janitor::clean_names('snake'), 
            by = c('sref_name' = 'scores')) %>%
  mutate(kenpom_t_rank = str_replace_all(kenpom_t_rank, '&amp;', '&')) %>% 
  mutate(name = ifelse(is.na(kenpom_t_rank), sref_name, kenpom_t_rank)) %>% 
  mutate(name = case_when(name == 'Tennessee-Martin' ~ 'Tennessee Martin',
                          name == 'Dixie State' ~ 'Dixie St.',
                          name == 'Tarleton State' ~ 'Tarleton St.',
                          name == 'North Carolina St.' ~ 'N.C. State',
                          name == 'UC-San Diego' ~ 'UC San Diego',
                          T ~ as.character(name))) %>% 
  left_join(., kenpom %>% 
                select(team, team_clean) %>% 
                mutate(team = str_remove_all(team, '\\d') %>% 
                         str_remove_all('\\*') %>% 
                         str_trim()) %>% 
                distinct(),
            by = c('name' = 'team'),
            keep = T) %>% 
  arrange(!is.na(team)) %>% 
  rename('kaggle_name' = 2,
         'kenpom_name' = 7,
         'kenpom_name_clean' = 8) %>% 
  select(team_id, kaggle_name, sref_name, kenpom_name, kenpom_name_clean)

conferences %>% 
  filter(is.na(sref_name)) %>% 
  select(team_name) %>% 
  unique() # check missing data

## Sports Reference Player Statistics -------------------------------------------------------------

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
         rsci_top_100 = replace_na(rsci_top_100, 999)) %>% 
  mutate(l_name = str_replace_all(player, '-', ' ') %>% 
           str_remove_all('[^A-Za-z ]') %>% 
           str_remove_all('Jr$') %>% 
           str_remove_all('[A-Z]+$') %>% 
           str_trim() %>% 
           str_extract('\\b(\\w+)$')) %>% 
  mutate(join = glue('{team}/{l_name}')) %>% 
  left_join(., read_csv('C:\\RScripts\\minnemudac-2021\\Back End Data\\all_americans0320.csv') %>% 
                janitor::clean_names('snake') %>% 
                mutate(no = replace_na(no, 0)) %>% 
                filter(no != 'No.') %>% 
                mutate(college_of_choice = ifelse(str_detect(college_of_choice, 'not attend|Not Attend|Undecided'), 
                                                  'No College', college_of_choice)) %>% 
                mutate(college_of_choice = str_remove_all(college_of_choice, '\\[.*?\\]') %>% 
                         str_remove_all('[^A-Za-z ]')) %>% 
                mutate(college_of_choice = case_when(college_of_choice == 'Connecticut' ~ 'UConn',
                                                     college_of_choice == 'North Carolina' ~ 'UNC',
                                                     college_of_choice == 'North Carolina State' ~ 'NC State',
                                                     college_of_choice == 'Pittsburgh' ~ 'Pitt',
                                                     college_of_choice == "TexasEl Paso" ~ 'UTEP',
                                                     str_detect(college_of_choice, 'Miami') ~ 'Miami (FL)',
                                                     T ~ as.character(college_of_choice))) %>% 
                mutate(l_name = str_replace_all(name, '-', ' ') %>% 
                         str_remove_all('[^A-Za-z ]') %>% 
                         str_remove_all('Jr$') %>% 
                         str_remove_all('[A-Z]+$') %>% 
                         str_trim() %>% 
                         str_extract('\\b(\\w+)$')) %>% 
                mutate(join = glue('{college_of_choice}/{l_name}')) %>% 
                select(join) %>% 
                mutate(all_american = 1), 
            by = 'join') %>% 
  mutate(all_american = replace_na(all_american, 0)) %>% 
  distinct()

team_level_player_stats = player_stats %>% 
  mutate(team = str_replace_all(team, '&amp;', '&')) %>% 
  mutate(team = case_when(team == 'Ole Miss' ~ 'Mississippi',
                          T ~ as.character(team))) %>% 
  group_by(season, team) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = T))) %>% 
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
            by = c('team', 'season')) %>% 
  left_join(., player_stats %>% 
                group_by(team, season) %>% 
                summarise(all_americans = sum(all_american)),
            by = c('team', 'season')) %>% 
  left_join(., name_key %>% 
                select(sref_name, team_id), by = c('team' = 'sref_name')) %>% 
  relocate(team_id, .before = team) %>% 
  mutate(team_id = ifelse(team == 'UConn', 1163, team_id)) %>% 
  relocate(all_americans, .after = team)   # bind this to `w_team_avg` once both are wrangled

team_level_player_stats %>% 
  ungroup() %>% 
  filter(is.na(team_id)) %>% 
  select(team) %>% 
  distinct()

team_level_player_stats %>% write_csv('C:\\RScripts\\minnemudac-2021\\Back End Data\\team_level_player_stats.csv')

# McDonalds All Americans -------------------------------------------------------------------------

mcdon = read_csv('C:\\RScripts\\minnemudac-2021\\Back End Data\\all_americans0320.csv') %>% 
  janitor::clean_names('snake') %>% 
  mutate(no = replace_na(no, 0)) %>% 
  filter(no != 'No.') %>% 
  mutate(college_of_choice = ifelse(str_detect(college_of_choice, 'not attend|Not Attend|Undecided'), 
                                    'No College', college_of_choice)) %>% 
  mutate(college_of_choice = str_remove_all(college_of_choice, '\\[.*?\\]') %>% 
                             str_remove_all('[^A-Za-z ]')) %>% 
  mutate(college_of_choice = case_when(college_of_choice == 'Connecticut' ~ 'UConn',
                                       college_of_choice == 'North Carolina' ~ 'UNC',
                                       college_of_choice == 'North Carolina State' ~ 'NC State',
                                       college_of_choice == 'Pittsburgh' ~ 'Pitt',
                                       college_of_choice == "TexasEl Paso" ~ 'UTEP',
                                       str_detect(college_of_choice, 'Miami') ~ 'Miami (FL)',
                                       T ~ as.character(college_of_choice))) %>% 
  mutate(l_name = str_replace_all(name, '-', ' ') %>% 
                  str_remove_all('[^A-Za-z ]') %>% 
                  str_remove_all('Jr$') %>% 
                  str_remove_all('[A-Z]+$') %>% 
                  str_trim() %>% 
                  str_extract('\\b(\\w+)$')) %>% 
  mutate(join = glue('{college_of_choice}/{l_name}'))

# Season Average Data For Missing Team/Season from Above

na_season_data = team_level_player_stats %>% 
  group_by(season) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = T)))


mcdon %>% 
  select(college_of_choice) %>% 
  distinct() %>% 
  filter(!college_of_choice %in% name_key$sref_name)
