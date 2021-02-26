library(tidyverse)
library(ncaahoopR)
library(RCurl)

ncaa = read_csv("https://query.data.world/s/rmwfowv6g4afpkrrzosypgypoqh7p5") %>% 
  janitor::clean_names(case = "snake")
ncaa$region_name = as.factor(ncaa$region_name)
summary(ncaa$round)

ncaa$winner = ifelse(ncaa$score>ncaa$score_1, ncaa$team, ncaa$team_1)
ncaa$win_seed = ifelse(ncaa$score>ncaa$score_1, ncaa$seed, ncaa$seed_1)
ncaa$lose_seed = ifelse(ncaa$score>ncaa$score_1, ncaa$seed_1, ncaa$seed)
ncaa$upset = ifelse(ncaa$win_seed>ncaa$lose_seed, 1,0)

upsetprob = ncaa %>% 
  group_by(round, win_seed) %>% 
  summarise(s = sum(upset)/35,
            n = n()/(35*4))
ggplot(upsetprob, aes(x = as.factor(win_seed), y = s, group = round, fill = s))+
  geom_col()+
  facet_wrap(~round)+
  geom_text(aes(label = round(s,2)), vjust = -0.25)+
  ggtitle("Average Upsets Per Year per Round")

ggplot(upsetprob, aes(x = as.factor(win_seed), y = n, group = round, fill = s))+
  geom_col()+
  facet_wrap(~round)+
  geom_label_repel(aes(label = round(n,2)), vjust = -0.25, color = "Navy", fill = "White")+
  ggtitle("Win Percentage Per Year per Round")+
  labs(x = "Seed",
       y = "Win Percentage %",
       fill = "Upset Pct")


a =  ncaa[,c("year", "round", "team", "score")]
b =  ncaa[,c("year", "round", "team_1", "score_1")] %>% rename("team" = team_1, "score" = score_1)
yrt = bind_rows(a,b)

everyr1 = list()
for(i in 1985:2019){
  one = ncaa %>% 
    filter(year==i&
             round==1)
  
  everyr1[[as.character(i)]] = one
}
df.allr1 = tibble()
for (i in everyr1){
 df.allr1 = bind_rows(df.allr1, i)
}
df.allr1$winner = ifelse(df.allr1$score>df.allr1$score_1, df.allr1$team, df.allr1$team_1)
df.allr1$win_seed = ifelse(df.allr1$score>df.allr1$score_1, df.allr1$seed, df.allr1$seed_1)
df.allr1$upset = ifelse(df.allr1$win_seed>=9, 1, 0)

seedup = df.allr1 %>% 
          group_by(win_seed) %>% 
          summarise(s = sum(upset)/(4*34), 
                    nu = n()/(4*34)) 
ggplot(seedup, aes(x = as.factor(win_seed), y = s, fill = win_seed))+
  geom_col(aes(x = as.factor(win_seed), y = nu, fill = nu)) +
  geom_col() +
  scale_x_discrete()+
  geom_text(aes(label = round(s, 3)), vjust = -0.5)+
  ggtitle("Win Percentage by Seed in Round 1")+
  labs(fill = "Seed",
       x = "Seed # (1:16)",
       y = "Win Percentage")+
  theme(text = element_text(family = "Calibri Light", size = 16),
        title = element_text(hjust = 5))

team_r1_prob = yrt %>%
  filter(round==1) %>% 
  group_by(team) %>% 
  summarise(n = n()/(max(ncaa$year)-min(ncaa$year)),
            score = round(mean(score)))

r1 = sample(team_r1_prob$team, prob = team_r1_prob$n,
       size = 64, replace = F ) %>% as_tibble() %>% rename("team" = value)

r1 = inner_join(r1, team_r1_prob, by = "team") %>% select(-score)
r1$rank = rank(-r1$n, ties.method = "random")
r1$invrank = rank(-r1$rank, ties.method = "random")
r1 = r1 %>% mutate(seed = ntile(rank, 16))

library(rvest)
tbls = list()
for (i in r1$team){
  j = j %>% str_replace_all(" ", "-")
  j = ifelse(i=="Ole Miss", "Mississippi", 
             ifelse(i=="St Johns", "st-johns-ny", 
                    ifelse()))
  j = str_to_lower(i)
  link = paste0("https://www.sports-reference.com/cbb/schools/", j, "/2020.html#all_totals")
  web = read_html(link)
  tbls[[i]] = web %>% html_nodes("table") %>% 
    .[2] %>% html_table(fill = T)
}
topr1 = r1[which(r1$rank<=32), ]
botr1 = r1[which(r1$rank>32), ] %>% select(-invrank) %>%  rename("invrank" = rank)
r1games = inner_join(topr1, botr1, by = "invrank")

team_champ_prob = yrt %>%
  filter(round==6) %>% 
  group_by(team) %>% 
  summarise(n = n()/(max(ncaa$year)-min(ncaa$year)),
            score = round(mean(score)))

team_champ_prob_1 = team_champ_prob %>% rename("team_1" = team, "score_1" = score)

winners = ncaa %>% 
  filter(region_name=="Championship")
winners$champ = ifelse(winners$score>winners$score_1, winners$team, winners$team_1)
champ.prob = table(winners$champ) %>% as.data.frame() %>% rename("team" = Var1)
champ.prob$team_1 = champ.prob$team

champs = sample(team_champ_prob$team, prob = team_champ_prob$n,
                     size = 20, replace = T )
champs1 = sample(team_champ_prob$team, prob = team_champ_prob$n,
                size = 20, replace = T )
champ_games = tibble(champs, champs1)

champ_games = champ_games %>% 
  filter(champs1 != champs) %>% 
  rename("team" = champs,
         "team_1" = champs1)

champ_games = inner_join(champ_games,team_champ_prob, by = "team") %>% select(-n)
champ_games = inner_join(champ_games,team_champ_prob_1, by = "team_1") %>% select(-n)
champ_games = inner_join(champ_games, champ.prob, by = "team") %>% select(-team_1.y) %>% 
  rename("team_1" = team_1.x)
champ_games = inner_join(champ_games, champ.prob, by = "team_1") %>% select(-team.y) %>% 
  rename("team" = team.x)

champ_games$winner = ifelse((champ_games$score+champ_games$Freq.x)>(champ_games$score_1+champ_games$Freq.y),
                            champ_games$team, champ_games$team_1)

