############################################################################## #
### Regressions
############################################################################## #  

# NBA, without IV
feols(attendance_per ~ cbsa_w_n_cases + adj_home_odds + policy | # controls
        home + away + weekday, # fixed effects
      data = filter(dat.final, league == "NBA") %>% 
        filter(season == "2021-22") %>% 
        filter(home %notin% c("Boston Celtics", "Golden State Warriors", "Utah Jazz", "Miami Heat")),
      vcov = ~home
) %>% 
  etable(tex = T)
# NBA, with IV
feols(attendance_per ~ adj_home_odds + policy | # controls
        home + away + weekday | # fixed effects
        cbsa_w_n_cases ~ l_neigh_w_n_cases, # instrument
      data = filter(dat.final, league == "NBA") %>% 
        filter(season == "2021-22") %>% 
        filter(home %notin% c("Boston Celtics", "Golden State Warriors", "Utah Jazz", "Miami Heat")),
      vcov = ~home
) %>% 
  etable(tex = T)
# NHL, without IV
feols(attendance_per ~ cbsa_w_n_cases + adj_home_odds + policy | # controls
        home + away + weekday, # fixed effects
      data = filter(dat.final, league == "NHL") %>% 
        filter(season == "2021-22") %>% 
        filter(home %notin% c("Boston Bruins", "Tampa Bay Lightning", "Washington Capitals")),
      vcov = ~home
) %>% 
  etable(tex = T)
# NHL, with IV
feols(attendance_per ~ adj_home_odds + policy | # controls
        home + away + weekday | # fixed effects
        cbsa_w_n_cases ~ l_neigh_w_n_cases, # instrument
      data = filter(dat.final, league == "NHL") %>% 
        filter(season == "2021-22") %>% 
        filter(home %notin% c("Boston Bruins", "Tampa Bay Lightning", "Washington Capitals")),
      vcov = ~home
) %>% 
  etable(tex = T)

feols(attendance_per ~ adj_home_odds + season_wins_scaled + policy | # controls
        home + away + month + weekday | # fixed effects
      cbsa_w_n_cases ~ l_neigh_w_n_cases, # instrument
      data = filter(dat.final, league == "NHL"),
      vcov = ~home
) %>% 
  etable(tex = T)

# impute the data
# try OLS with varying degrees of controls,
# clustering the standord errors at the error

# NBA teams that have basically 0 variance
c("Boston Celtics", "Golden State Warriors", "Utah Jazz", "Miami Heat")
# NBA teams with the highest variance (Top 5)
c("Detroit Pistons", "Washington Wizards", "Orlando Magic", "San Antonio Spurs", "Denver Nuggets")
# NHL teams with basically 0 variance
c("Boston Bruins", "Tampa Bay Lightning", "Washington Capitals")
# NHL teams with the highest variance
c("Buffalo Sabres", "Arizona Coyotes", "New Jersey Devils", "San Jose Sharks", "Los Angeles Kings")

