# Opening libraries


library(tidyverse)
library(dplyr)
library(broom)


# Reading in and tidying Bulls data files


sal <- read_csv("data/raw/2018-19_nba_player-salaries.csv")
stat_pl <- read_csv("data/raw/2018-19_nba_player_statistics.csv")
stat_tm1 <- read_csv("data/raw/2018-19_nba_team_statistics_1.csv")
stat_tm2 <- read_csv("data/raw/2018-19_nba_team_statistics_2.csv")
pay <- read_csv("data/raw/2019-20_nba_team-payroll.csv")


## Check struture of the two team files


str(stat_tm1)

str(stat_tm2)



# Tidying data

## Binding data from two data frames
###Binding variables from team 1 data and team 2 data


df_team <- left_join(x = stat_tm2, y = stat_tm1 [-c(1,3, 23:25)],
                     by = "Team")


## renaming variables: 
#Making sure variables do not include a %, replace with 'p'. And added a 'x' in front of number variables. Change '/' to _per_ .

df_team <- df_team %>%
  rename( x3P = '3P', x3PA = '3PA', x3Pp = '3P%', x2P = '2P', x2PA = '2PA', x2Pp = '2P%', FGp = 'FG%', FTp = 'FT%', x3PAr = "3PAr", TSp = "TS%", eFGp = "eFG%", TOVp = "TOV%", ORBp = "ORB%", FT_per_FGA = "FT/FGA", DRBp = "DRB%")


## Normalising key metrics

###We want normalise the key metrics to per game

df_team <- df_team %>%
  mutate (PTS_per_game = PTS / G,
          WINp = W/G,
          AST_per_game = AST / G,
          FG_per_game = FGA / G, 
          x3P_per_game = x3P / G,
          x2P_per_game = x2PA / G,
          FT_per_game = FTA / G,
          STL_per_game = STL / G,
          BLK_per_game = BLK / G,
          TOV_per_game = TOV / G,
          ORB_per_game = ORB / G,
          DRB_per_game = DRB / G,
          TRB_per_game = TRB / G,
          PF_per_game = PF / G)


## Normalising variables per minuted played minutes played
###There are 5 players playing roughly 48mins per game. Therefore there are ~240mins per game. WE want normalise these varaibles for the minutes played. 


df_team <- df_team %>%
  mutate (PTS_pmp = PTS / MP , 
          AST_pmp = AST / MP,
          FG_pmp = FGA / MP, 
          x3P_pmp = x3P / MP ,
          x2P_pmp = x2P / MP ,
          FT_pmp = FTA / MP,
          STL_pmp = STL / MP ,
          BLK_pmp = BLK / MP,
          TOV_pmp = TOV / MP,
          ORB_pmp = ORB / MP,
          DRB_pmp = DRB / MP,
          TRB_pmp = TRB / MP,
          PF_pmp = PF / MP)


##Check structure of new data set

str(df_team)








