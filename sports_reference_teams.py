from bs4 import BeautifulSoup
import re
import urllib3
import pandas as pd

http = urllib3.PoolManager()

url = 'https://www.sports-reference.com/cbb/'
response = http.request('GET', url)
soup = BeautifulSoup(response.data)

links = soup.find('div', id = "schools").find('select', id = 'selector_1').find_all("option")

all_teams = []
for link in links:
  name = re.findall(r">(.*?)<", str(link))[0]
  ext = re.findall(r'value="(.*?)"', str(link))[0]

  if ext is not '':
    out = {"team": name,
           "link": ext}

    all_teams.append(out)

# pd.DataFrame(all_teams).to_csv('sports_reference_teams.csv', index = False)

ranking_df_list = []
player_df_list = []
player_detail_list = []
for team in all_teams:
  url = f'https://www.sports-reference.com{team["link"]}'
  print(url)
  response = http.request('GET', url)
  soup = BeautifulSoup(response.data)

  ptags = soup.find('div', id = 'info').find_all('p')

  for tag in ptags:

    if str(tag).find("Location") != -1:
      place = re.findall(r'\n(.*)', str(tag))[1].strip()
      team['location'] = place
  
  try:
    data = pd.read_html(f'{url}/polls.html')[0]
    data = data[~data.Rk.str.contains("Season|Rk")].drop("Rk", axis = 1)
    data['team'] = team['team']
    data['team_location'] = team['location']

    ranking_df_list.append(data)
  except:
    print(f"Could Not Find Ranking Data For {team['team']}")
    continue

  for year in range(2003, 2022):
    try:
      player_data_a = pd.read_html(f'{url}/{year}.html')
      player_data = player_data_a[0]
      player_data['year'] = year
      player_data['team'] = team['team']
      player_df_list.append(player_data)

      for table in player_data_a[1:]:
        if table.shape[0] > 10: 
          table['season'] = year
          table['team'] = team['team']
          player_detail_list.append(table)
    except:
      print(f"Could Not Find Player Level Data For {team['team']} in Year {year}")

pd.concat(player_df_list, ignore_index=True).to_csv("player_stats_0321.csv", index=False)
pd.concat(player_detail_list, ignore_index=True).to_csv("player_stats_detail_0321.csv", index=False)
pd.concat(ranking_df_list, ignore_index=True).to_csv("team_ap_historical_rank.csv", index=False)