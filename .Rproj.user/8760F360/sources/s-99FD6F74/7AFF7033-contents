#Tidying Player data

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


# Tidying data

## Binding data from two data frames
#Binding variables from team 1 data and team 2 data


df_team <- left_join(x = stat_tm2, y = stat_tm1 [-c(1,3, 23:25)],


## renaming variables: 
#Making sure variables do not include a %, replace with 'p'. And added a 'x' in front of number variables. Change '/' to _per_ .


df_team <- df_team %>%
  rename( x3P = '3P', x3PA = '3PA', x3Pp = '3P%', x2P = '2P', x2PA = '2PA', x2Pp = '2P%', FGp = 'FG%', FTp = 'FT%', x3PAr = "3PAr", TSp = "TS%", eFGp = "eFG%", TOVp = "TOV%", ORBp = "ORB%", FT_per_FGA = "FT/FGA", DRBp = "DRB%")



## Normalising key metrics

#We want normalise the key metrics to per game

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
There are 5 players playing roughly 48mins per game. Therefore there are ~240mins per game. WE want normalise these varaibles for the minutes played. 

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

### Tidying player data


df_player <- stat_pl %>%
  rename( x3P = '3P', x3PA = '3PA', x3Pp = '3P%', x2P = '2P', x2PA = '2PA', x2Pp = '2P%', FGp = 'FG%', FTp = 'FT%', eFGp = "eFG%")

#Averaging out minutes played per game, more player (mp_per_game) to create one number from the team data for the player data

mp_per_game <- df_team %>%
  group_by(Team) %>%
  summarise(mp_per_game = MP/G/5) %>%
  pull(mp_per_game) %>%
  mean

mp_per_game



## Creating new variables: Points per minute per player for each of the model metrics

df_player <- df_player %>%
  group_by(player_name) %>%
  mutate(PTS_pmp = PTS / MP,
         x3P_pmp = x3P / MP,
         x2P_pmp = x2P / MP,
         FT_pmp = FTA / MP)




## normalising using the minutes played per game
player <- df_player %>%
  group_by(player_name) %>%
  mutate(PTS_n = PTS_pmp * mp_per_game,
         x3P_n = x3P_pmp * mp_per_game,
         x2P_n = x2P_pmp * mp_per_game,
         FT_n = FT_pmp * mp_per_game) %>%
  filter(PTS_n > 10) %>%
  select(c(player_name, Pos, PTS_n, x3P_n, x2P_n, FT_n, x3P_pmp, x2P_pmp, FT_pmp, PTS_pmp))
```


#Identifying how all the below points were achieved, with the normalised per minutes played

player <- df_player %>%
  summarise(MPn = sum(MP)/mp_per_game,
            x3P_pmp = sum(x3P_pmp)/ MPn,
            x2P_pmp = sum(x2P_pmp)/MPn,
            FT_pmp = sum(FT_pmp) / MPn,
            G = sum(G)) %>%
  filter(G > 20) %>%
  select(-c(MPn))

head(player)


## Expected points per minuted played metric

player <- mutate(player, exp_PTS_ppl = predict(fit_mp, newdata = player))


#Visualising new player metric

player %>%
  ggplot(aes(x = exp_PTS_ppl)) +
  geom_histogram(binwidth = 0.01, colour = "black", fill = "blue")

## add in salary


player <- sal %>%
  select(player_name, salary) %>%
  right_join(player, by = "player_name")


## Creating position data frame object

pos <- df_player %>%
  filter(G > 0) %>%
  group_by(player_name, Pos) %>%
  summarise(G = sum(G)) %>%
  top_n(1,abs(G))

## add in player positions

player <- pos %>%
  select(player_name, Pos) %>%
  right_join(player, by = "player_name")

#Filtering out NAs in Pos and Salary variables

player <- player %>%
  filter(!is.na(Pos)) %>%
  filter(!is.na(salary))

#Creating Player rating data frame with ideal players at the top

player_choices <- player %>%
  select(player_name, Pos, salary, exp_PTS_ppl) %>%
  arrange(desc(exp_PTS_ppl), salary)

###Choose players from this final 'player_choices' data frame





