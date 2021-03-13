import pandas as pd

def all_americans(start_year, end_year):
  headers = ['No.',	'Name','Height','Weight','Position','Hometown','High school','College of Choice']
  valid_df = []
  for year in range(start_year, end_year+1):
    link = f'https://en.wikipedia.org/wiki/{year}_McDonald%27s_All-American_Boys_Game'
    dfs = pd.read_html(link)

    for df in dfs:
      if df.shape[1] == 8:
        df.columns = headers
        df['year'] = year
        valid_df.append(df)

  return pd.concat(valid_df, ignore_index=True)