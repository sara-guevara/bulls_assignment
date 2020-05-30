##Check structure of new data set

```{r Str_df_team}
str(df_team)
```




## Multiple Regression
###Checking confidence intervals, R squared stat and intercept to see which variables have a relationship and which don't.

### Wasn't sure whether to use per game played or per minute played as this is a more common stat in Basketball. So looked at both.

# 1. Per Game metric 

lm_teams <- lm(PTS_per_game ~ AST_per_game + FG_per_game + x3P_per_game + x2P_per_game + FT_per_game + STL_per_game + BLK_per_game + TOV_per_game + DRB_per_game + ORB_per_game, data = df_team)

tidy(lm_teams, conf.int = TRUE)

summary(lm_teams)

# 2. Per minute played metric 

lm_mp <- lm(PTS_pmp ~ AST_pmp + FG_pmp + x3P_pmp + x2P_pmp + FT_pmp + STL_pmp + BLK_pmp + TOV_pmp + DRB_pmp + ORB_pmp, data = df_team)
tidy(lm_mp, conf.int = TRUE)

summary(lm_mp)


## Checking assumptions for MLR
### Checking assumptions for both linear models (lm_teams_pg) (lm_mp)

#1, Response variables should be continuous. Yes this is met
#2. There are wo or more explanatory variables that are continuous or categorical Yes all of variables are continous
#3. Independence

car::durbinWatsonTest(lm_teams)

## D-W statistic is Close to '2' so meets this assumption

#4. Linearity

car::avPlots(lm_teams)

car::durbinWatsonTest(lm_mp)


## based on the linear regression confidence intervals that are crossing over zero (therefore stating that there isn't a relationship). And the sign that a few variables do not show a linear relationship:
## The linear regression model has been Updated to only include variables with a relationship.

# *per game*

fit_pg <- lm(PTS_per_game ~ AST_per_game + x3P_per_game + x2P_per_game + FT_per_game, data = df_team)

tidy(fit_pg, conf.int = TRUE)

summary(fit_pg)

# *per minute played*

fit_mp <- lm(PTS_pmp ~   x3P_pmp + x2P_pmp + FT_pmp, data = df_team)
tidy(fit_mp, conf.int = TRUE)

summary(fit_mp)
