import pandas as pd
import numpy as np
import re
from transformers import pipeline

nlp = pipeline("zero-shot-classification")

sr = pd.read_csv('/Back End Code/sports_reference_teams.csv')
df = pd.read_csv('/Data/MTeams.csv')

df.TeamName = [re.sub(r'Univ$', "University", team.replace(' St', ' State')) for team in df.TeamName]
sr['team'] = [team.replace('&amp;', '&') for team in sr.team]

straight_join = pd.merge(df, sr, how='left', left_on='TeamName', right_on='team')
need_team = straight_join[straight_join.team.isna()]

def nlp_flow(input, options, l):
  if len(options) == 1:
    ret = {'TeamName': team, "team": options[0]}
    l.append(ret)
      
  elif len(options) > 1:
    output = nlp(team, options)
    ret = {'TeamName': team, "team": output['labels'][0]}
    l.append(ret)
  else: 
    output = nlp(team, sr.team.to_list())
    ret = {'TeamName': team, "team": output['labels'][0]}
    l.append(ret)

  print(ret)

fixed_list = []
for team in need_team.TeamName:
  team2 = team.replace('-', " ")
  if len(re.findall(" ", team2)) > 0:
    s = team.split()
    bad_words = ['St', "St.", "Saint", "University"]
    regex = max([word for word in s if word not in bad_words])
    options = sr[sr.team.str.lower().str.contains(regex.lower())].team.tolist()
    nlp_flow(team, options, fixed_list)
  else:
    options = sr[sr.team.str.lower().str.contains(team.lower())].team.tolist()
    nlp_flow(team, options, fixed_list)

pd.DataFrame(fixed_list).to_csv('matched_names.csv', index = False)