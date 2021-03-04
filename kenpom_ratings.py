import pandas as pd

set_cols = ["rk", 'team', 'conf', 'w_l', "adjem", 'adjo', 'adjo_sub', 'adjd', 'adjd_sub',
            'adjt','adjt_sub', 'luck','luck_sub', 'adjem','adjem_sub', 'oppo', 'oppo_sub',
            'oppd', 'oppd_sub', 'ncsos', 'ncsos_sub']

df_list = []
for year in range(2003, 2021):
  t = pd.read_html(f'https://kenpom.com/index.php?y={year}')[0]
  t.columns = set_cols
  t = t[~t.ncsos.str.contains("NCSOS")]
  t['year']= year
  df_list.append(t)

year = 2021
t = pd.read_html(f'https://kenpom.com/index.php')[0]
t.columns = set_cols
t = t[~t.ncsos.str.contains("NCSOS")]
t['year'] = year
df_list.append(t)

pd.concat(df_list, ignore_index=True).to_csv('kenpom_ratings0321.csv', index = False)