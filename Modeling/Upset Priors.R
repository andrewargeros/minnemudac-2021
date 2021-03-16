library(tidyverse)
library(glue)

ncaa = read_csv("https://query.data.world/s/rmwfowv6g4afpkrrzosypgypoqh7p5") %>% 
  janitor::clean_names('snake') %>% 
  mutate(winner = ifelse(score > score_1, team, team_1),
         win_seed = ifelse(score>score_1, seed, seed_1),
         lose_seed = ifelse(score>score_1, seed_1, seed),
         seed_diff = lose_seed - win_seed,
         upset = ifelse(lose_seed<win_seed, 1, 0))

upsets_1 = ncaa %>%  
  filter(round == 1) %>% 
  group_by(round, win_seed) %>% 
  summarise(upsets = n()) %>% 
  mutate(upset_prob = upsets/140) %>% 
  mutate(game_no = 16:1) %>% 
  mutate(ev = ifelse(win_seed > 8, upset_prob*(1+win_seed-game_no), upset_prob))

upsets_2 = upsets_1 %>% 
  filter(win_seed <= 8) %>% 
  inner_join(., upsets_1 %>% filter(win_seed > 8), by = 'game_no')

ncaa %>% 
  filter(round == 2) %>% 
  filter(win_seed == 12 | lose_seed == 12) %>% 
  group_by(win_seed) %>% 
  summarise(n())

high_seed = 8

test = upsets_1 %>% 
  filter(win_seed.x == high_seed) %>% 
  select(win_seed.x, win_seed.y) %>% 
  t() %>% 
  as_tibble()

test_p = upsets_1 %>% 
  filter(win_seed.x == high_seed) %>% 
  select(upset_prob.x, upset_prob.y) %>% 
  t() %>% 
  as_tibble() 

sim = sample(test$V1, 
             prob = test_p$V1, 
             size = 1001, 
             replace = T) %>% 
  enframe() %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(n_pct = n/sum(n))


upsets = ncaa %>% 
  group_by(round) %>% 
  mutate(both_seeds = glue('{seed}//{seed_1}'), 
         high_seed_win = ifelse(seed == win_seed, 1, 0),
         low_seed_win = ifelse(seed != win_seed, 1, 0)) %>% 
  select(round, both_seeds, high_seed_win, low_seed_win) %>% 
  ungroup() %>% 
  group_by(round, both_seeds) %>% 
  summarise(high = sum(high_seed_win),
            low = sum(low_seed_win)) %>% 
  ungroup() %>% 
  mutate(high_prob = high/(high+low),
         low_prob = low/(high+low))


ncaa %>% 
  filter(round == 1) %>% 
  filter(win_seed == 8 | win_seed == 9) %>% 
  group_by(win_seed) %>% 
  summarise(n())

test = seq(1:16) %>% 
  bind_cols(seq(from = 16, to = 1)) %>% 
  rename('w' = 1, 'l' = 2) %>% 
  mutate(r1_50_ev = ifelse(w-l<0, 0.5, 0.5*(1+w-l)))
