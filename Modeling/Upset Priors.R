library(tidyverse)
library(glue)

ncaa = read_csv("https://query.data.world/s/rmwfowv6g4afpkrrzosypgypoqh7p5") %>% 
  janitor::clean_names('snake') %>% 
  mutate(winner = ifelse(score > score_1, team, team_1),
         win_seed = ifelse(score>score_1, seed, seed_1),
         lose_seed = ifelse(score>score_1, seed_1, seed),
         seed_diff = lose_seed - win_seed,
         upset = ifelse(lose_seed<win_seed, 1, 0))

upsets = ncaa %>% 
  group_by(round, win_seed) %>% 
  summarise(upsets = n(),
            upset_prob = upsets/140)

ncaa %>% 
  filter(round == 1) %>% 
  filter(win_seed == 8 | win_seed == 9) %>% 
  group_by(win_seed) %>% 
  summarise(n())

test = seq(1:16) %>% 
  bind_cols(seq(from = 16, to = 1)) %>% 
  rename('w' = 1, 'l' = 2) %>% 
  mutate(r1_50_ev = ifelse(w-l<0, 0.5, 0.5*(1+w-l)))
