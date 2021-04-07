# MinneMUDAC 2021: March Madness [<img align="middle" alt="MM" width="150px" src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fupload.wikimedia.org%2Fwikipedia%2Fen%2Fthumb%2F2%2F28%2FMarch_Madness_logo.svg%2F1200px-March_Madness_logo.svg.png&f=1&nofb=1" />][MM]

## Contributors: [Andrew Argeros](https://www.linkedin.com/in/andrewargeros/) and [Max Bolger](https://www.linkedin.com/in/max-bolger/) [<img align="right" alt="HU" width="200px" src="https://www.collegeconsensus.com/wp-content/uploads/2020/05/Hamline-University-logo.png" />][HU]

This repo contains all of the code, data, and output from our entry in the 2021 [MinneMUDAC Data Science Competition](https://minneanalytics.org/minnemudac/) sponsored by MinneAnalytics.

## The Problem

This year's competition focused on predicting the outcome of the [2021 NCAA Men's Basketball Tournament](https://en.wikipedia.org/wiki/2021_NCAA_Division_I_Men%27s_Basketball_Tournament), or March Madness.

## The Data

The majority of the data used stems from the [2021 Kaggle March Mania Competition](https://www.kaggle.com/c/ncaam-march-mania-2021/data). The data are assembled into 20 `.csv` files, stored in the `/Data/Kaggle Data` folder.

These files can be downloaded using the Kaggle API:

```
kaggle competitions download -c ncaam-march-mania-2021 -p {PATH TO YOUR DIRECTORY}
```

### McDonalds All Americans

Yearly, since 1977, McDonalds has hosted a sort of all-star game for the top high school players in the country. These players eventually go on to play Division 1 basketball, and are some of the top players in their matriculating class. By scraping Wikipedia's pages on the all star game rosters, we are able to account for the relative "Star Power" of a given team in a given season. For example, the 2018-19 Duke Blue Devils had a team of four MCD All Americans from the year previous. This is often regarded as one of the top recruiting classes in college basketball. The code to scrape this data is located in `all_americans.py` and the resulting data is stored in `/Back End Data/all_americans0320.csv`.

### SportsReference.com

Additionally, we pulled in location, ranking, and player data for each team listed on [SportsReference](www.sportsreference.com/cbb). These files are additionally stored in `/Data/Back End Data`

## Modeling

To predict the outcomes of games, we used two models. The first was a random forests based model, trained to predict `team_1` or `team_2`. The ordering of the teams was changed this way for its data quality. The team listed in the data as `team_1`, was determined by the odd/evenness of the `w_team_id`.

To predict, we used the underlying `predict.all = TRUE`, argument to bootstrap individual tree predictions, so as to create a more stochastic process, better simulating the randomness of the tournament. We additionally calculated the "probability", based on the voting breakdown of the model's trees.

## Final Bracket

Our bracket finished 19th in the overall competition, and in the 89th percentile of ESPN's Tournament challenge, giving Andrew a 2nd place finish in his family pool. One thing to note is the MinneMUDAC scoring system, which gave an incentive to picking upsets.

##### By Round:
- **Sweet Sixteen**: 6/16
- **Elight Eight**: 4/8
- **Final Four**: 3/4

![](https://github.com/andrewargeros/minnemudac-2021/blob/main/Bracket/final_bracket.png?raw=true)















[HU]: hamline.edu
[MM]: https://www.ncaa.com/news/basketball-men/article/2021-march-madness-schedule
