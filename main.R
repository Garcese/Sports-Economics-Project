############################################################################## #
### Main File for sports econ paper
############################################################################## #  

# packages
library(car) # vif function
library(expp) # neighbors data frame function. this messes with view() function, and probably lots of others
library(fastDummies)
library(fixest) # regression stuff
library(htmltools) # for leaflet
library(httr) # data-scraping
library(leaflet)
library(lubridate)
library(magrittr)
library(openxlsx)
library(patchwork) 
library(performance)
library(renv)
library(RCurl) # needed for something ...
library(rsconnect)
library(rvest) # read_html function
library(scales)
library(shiny)
library(shinycssloaders)
library(spdep) # spatial data, I think
library(stargazer)
library(tidycensus) # census data
library(tidyselect)
library(tidyverse)
library(tigris) # census data
library(totalcensus) # cbsa data
# import helper functions. Keeps them out of global namespace. Also sets some global options
source("helper.R", local = helper <- new.env())
helper$my_attach(helper) 

# Raw Data Collection ----------------------------------------------------------

# cbsa data (from totalcensus package)
data("dict_cbsa")
# write_csv(dict_cbsa, "assets/cbsa_data/raw.cbsa.csv")

# covid-19 data (NewYorkTimes)
nytRaw2020 <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2020.csv")
nytRaw2021 <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2021.csv")
nytRaw2022 <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv")
# write_csv(read_csv(nytRaw2020), "assets/covid_data/raw.covid2020.csv")
# write_csv(read_csv(nytRaw2021), "assets/covid_data/raw.covid2021.csv")
# write_csv(read_csv(nytRaw2022), "assets/covid_data/raw.covid2022.csv")

# import data scraping objects
attach_source("scrape.R", "data_scraping")

# 2015-16 season. 1231 observations -> 1230 games, 1 allstar game
raw.nba.2015 <- scrape_season(nbaSeasons[1] %>% reduce(c), "nba")
# 2016-17 season. 1232 observations -> 1230 games, 1 allstar game, 1/7/2017 blazers postponed
raw.nba.2016 <- scrape_season(nbaSeasons[2] %>% reduce(c), "nba")
# 2017-18 season. 1232 observations -> 1230 games, 1 allstar game, USA game
raw.nba.2017 <- scrape_season(nbaSeasons[3] %>% reduce(c), "nba")
# 2018-19 season. 1232 observations -> 1230 games, 1 allstar game, USA game
raw.nba.2018 <- scrape_season(nbaSeasons[4] %>% reduce(c), "nba")
# 2019-2020 season, up to covid. 975 observations -> 971, 1 allstar game, USA game, Thunder game and Kings game cancelled on 3/11/2020
raw.nba.2019 <- scrape_season(nbaSeasons[6] %>% reduce(c), "nba", new = T)
# NBA 2021-22 season. 1231 observations -> 1230 games, 1 allstar game
raw.nba.2021 <- scrape_season(nbaSeasons[5] %>% reduce(c), "nba")

# 2015-16 season. 1235 observations -> 1230 games, 3 allstar games, 1/23/2016 islanders  ...
# ... postponed, 1/24/2016 capitals postponed. Missing 7 games from 10/8/2015, persistent 503
raw.nhl.2015 <- scrape_season(nhlSeasons[1] %>% reduce(c), "nhl")
# 2016-17 season. 1234 observations -> 1230 games, 3 allstar games, 3/14/2017 devils postponed
raw.nhl.2016 <- scrape_season(nhlSeasons[2] %>% reduce(c), "nhl")
# 2017-18 season. 1274 observations -> 1271 games, 3 allstar games. VGK joined this season
raw.nhl.2017 <- scrape_season(nhlSeasons[3] %>% reduce(c), "nhl")
# 2018-19 season. 1274 observations -> 1271 games, 3 allstar games
raw.nhl.2018 <- scrape_season(nhlSeasons[4] %>% reduce(c), "nhl")
# 2019-2020 season, up to covid. 1085 observations -> 1082 games, 3 allstar games
raw.nhl.2019 <- scrape_season(nhlSeasons[6] %>% reduce(c), "nhl", new = T)
# NHL 2021-22 season. 1413 observations -> 1312 games, 3 allstar games, 98 postponed. Kraken joined
raw.nhl.2021 <- scrape_season(nhlSeasons[5] %>% reduce(c), "nhl")

# write the data
# mass_write("assets/game_data/nba/", game_data_names %includes% "nba")
# mass_write("assets/game_data/nhl/", game_data_names %includes% "nhl")
write_csv(raw.nhl.2019, "assets/game_data/nhl/raw.nhl.2019.csv")


# betting data 
attach_source("bet.R", "betting_data")
# write_bet("assets/betting_data/")

# collect Raw data, tigris package function
counties(cb = TRUE, resolution = "500k") %>% # 500k is best resolution
  # saveRDS(file = "assets/leaflet/2020_fips_shapes.rds") # geometry doesn't save nicely as CSV

# tidycensus package. Newest is 2019 apparently
get_estimates(geography = "county", product = "population", year = 2019) %>% 
  # write_csv("assets/cbsa_data/dat.population.csv")

# FIP/CBSA Data ----------------------------------------------------------------

# get neighboring counties
dat.neighboring <- readRDS("assets/leaflet/2020_fips_shapes.rds") %>% # gets county geo-spatial data
  select(GEOID, geometry) %>% 
  set_colnames(c("fips", "geometry"))
dat.neighboring <- dat.neighboring %>% # run geo-spatial list into a function that automatically detects adjacencies
  pull(geometry) %>% 
  poly2nb(row.names = dat.neighboring %>% pull(fips)) %>% 
  neighborsDataFrame() %>% # expp library
  group_by(id) %>% 
  summarize(id_neigh = list(unique(id_neigh))) %>% 
  ungroup() %>% 
  set_colnames(c("fips", "fips_neighbors")) # 3214 obs, 2 vars - 4/27/2023

# clean population data for merge
dat.population <- read_csv("assets/cbsa_data/dat.population.csv") %>% 
  filter(variable == "POP") %>% 
  select(2, 4) %>% 
  set_colnames(c("fips", "pop")) # 3220 obs, 2 vars - 4/27/2023

# clean CBSA data
dat.cbsa <- read_csv("assets/cbsa_data/raw.cbsa.csv", col_types = cols(.default = "c")) %>% 
  as_tibble() %>% 
  select(CBSA, CBSA_title, STATE, COUNTY, central_outlying) %>% 
  rename_with(~tolower(.x), everything()) %>%
  filter(cbsa %in% (c(12060, 14460, 15380, 16740, 16980, 17460, 18140, 19100, 19740,
                     19820, 26420, 26900, 29820, 31080, 32820, 33100, 33340, 33460,
                     34980, 35380, 35620, 36420, 36740, 37980, 38060, 38300, 38900, 
                     39580, 40900, 41180, 41620, 41700, 41860, 41940, 45300, 47900) %>% 
           as.character())) %>% # all 36 unique cbsas. 27 unique NBA, 21 unique NHL. 313 FIPS
  unite("fips", state:county, sep = "") %>% 
  separate(cbsa_title, into = c("cbsa_title", "cbsa_states"), sep = ", ") %>% 
  mutate(cbsa_states = map(cbsa_states, ~str_split(.x, "-")[[1]] %>% c())) %>% 
  mutate(home = assign_home(cbsa), .after = cbsa_title) %>% 
  mutate(home = map(home, ~str_split(.x, ",")[[1]] %>% c())) %>% 
  mutate(time_zone = cbsa_to_time(cbsa), .after = cbsa_states) %>% 
  group_by(cbsa) %>% 
  mutate(cbsa_fips = list(unique(fips)), .after = time_zone) %>% # all FIPs in a cbsa, in a list
  ungroup() %>% 
  left_join(dat.neighboring, by = "fips") %>% 
  mutate(cbsa_neighbors = map2(fips_neighbors, cbsa_fips, ~setdiff(.x, .y))) %>% # all fips neighbors, including those already in the CBSA
  left_join(unnest(., cbsa_neighbors) %>%
              distinct(cbsa, cbsa_neighbors) %>% 
              group_by(cbsa) %>% 
              summarize(cbsa_neighbors = list(unique(cbsa_neighbors))),
            by = "cbsa") %>% 
  select(-cbsa_neighbors.x) %>% 
  rename(cbsa_neighbors = cbsa_neighbors.y) %>% # now, cbsa_neighbors is at the CBSA level, and identifies all bordering to that CBSA
  relocate(cbsa_neighbors, .after = cbsa_fips) %>% 
  bind_rows(distinct(., cbsa, cbsa_neighbors) %>% 
              unnest(cbsa_neighbors) %>% 
              rename(fips = cbsa_neighbors) %>% 
              mutate(central_outlying = "Neighboring")
              ) %>% # adds (416 now, 729 total) 430 fips, 313 + 430 = 743, all the neighboring fips. Assign them as "neighboring". 
  # it seems that an update fixed neighbors being assigned over water, I found 8 of the 14 removed. Likely some ...
  # ... duplicates were being reported erroneously.
  relocate(fips_neighbors, .after = central_outlying) %>% 
  left_join(x = select(., -(cbsa_title:cbsa_neighbors)), # for some reason, specifying x and y was necessary
            y = select(., cbsa:cbsa_neighbors) %>% distinct(cbsa, .keep_all = T),
             by = "cbsa") %>% 
  relocate(fips:fips_neighbors, .after = last_col()) %>% 
  # merge population data
  left_join(dat.population, by = "fips") %>% 
  mutate(temp = case_when(central_outlying == "Neighboring" ~ T, T ~ F)) %>% 
  group_by(cbsa, temp) %>% 
  mutate(cbsa_pop = sum(pop)) %>% 
  ungroup(temp) %>% 
  select(-temp) # 729 obs, 12 vars - 4/27/2023
# 313 unique CBSA-only fips, 416 neighbor-only fips, 403 unique neighbor-only fips. ...
# ... 23 duplicates. 13 neighbor-neighbor duplicates, 10 neighbor-cbsa duplicates. ...
# ... together, there are 313 + 403 - 10 = 706 unique fips

# Covid-19 Data ----------------------------------------------------------------

# Clean Covid-19 data
dat.covid <- read_csv("assets/covid_data/raw.covid2021.csv") %>% 
  bind_rows(read_csv("assets/covid_data/raw.covid2022.csv")) %>% 
  as_tibble() %>% 
  relocate(fips, state, .before = county) %>% 
  # The 5 boroughs are listed as "New York City" in county var, and NA in fips. ...
  # ... I made it so NYC all under Bronx fip. 309 unique cbsa fips now. So 313 + 403 - 4 - 10 = 702 unique fips
  mutate(fips = case_when(county == "New York City" ~ "36005", T ~ fips)) %>% 
  filter(fips %in% (dat.cbsa %>% pull(fips))) %>% 
  # 2021-09-13 is the Monday 4 weeks before the first NHL games, 2022-05-29 is the ...
  # ... Sunday 4 weeks after the last games. The NBA starts later and finishes earlier than the NHL
  filter("2021-09-12" <= date & date <= "2022-05-29") %>% # keep a day before 2021-09-13 for lag calculation
  left_join(dat.cbsa %>% select(cbsa, cbsa_title, home, fips, central_outlying, cbsa_pop), by = "fips", relationship = "many-to-many") %>% 
  relocate(cbsa:home, .after = date) %>% 
  relocate(central_outlying:cbsa_pop, .after = county) %>% 
  mutate(floor_monday = floor_date(date, "week", 1), .after = date) %>%  # week identifier
  group_by(fips, cbsa) %>% # need central_outlying to adequately identify counties, since we have duplicates.
  mutate(across(cases:deaths, ~case_when( # create new cases and deaths
    row_number() == 1 ~ NA_real_, 
    T ~ .x - lag(.x)
  ), 
  .names = "n_{col}"
  )) %>%  
  ungroup() %>% 
  filter(date > "2021-09-12") %>% # we just needed this to calculate new cases for 2021-09-13
  # we now have the same fips as dat.cbsa, but 4 less from NYC so 725 FIPs each with 259 observations. ...
  # 187775 total, 23 duplicate FIPs (still, the removed counties were not duplicates)
  mutate(across(n_cases:n_deaths, ~case_when( # impute negatives to NAs
    .x <= 0 ~ NA,
    T ~ .x
  ),
  .names = "i_{col}"
  )) %>% 
  group_by(cbsa, fips) %>% 
  mutate(across(i_n_cases:i_n_deaths, ~round(my_impute(.x), digits = 0))) %>% 
  mutate(across(i_n_cases:i_n_deaths, ~case_when(is.na(.x) ~ 0, T ~ .x))) %>% 
  # Any leftover NAs just mean there was something like c(..., 100, 0, -3, 0, 0) ...
  # ... at the end of the data set, so we are fine setting them to 0.
  # gets the rolling weekly cases figure, and the lag of that.
  mutate(across(n_cases:i_n_deaths, ~ .x + lag(.x) + lag(.x, 2) + lag(.x, 3) + lag(.x, 4) + lag(.x, 5) + lag(.x, 6),
                .names = "w_{col}")) %>% 
  mutate(across(w_n_cases:w_i_n_deaths, ~ lag(get(cur_column()), 7),
                .names = "l_{col}")) %>% 
  ungroup() %>% # aggregate to cbsa levels
  left_join( 
    filter(., central_outlying != "Neighboring") %>% 
      distinct(date, cbsa, fips, .keep_all = T) %>%
      group_by(date, cbsa) %>% 
      mutate(across(w_n_cases:w_i_n_deaths, ~sum(.x), .names = "cbsa_{col}")) %>% 
      ungroup() %>% 
      distinct(date, cbsa, .keep_all = T) %>% 
      group_by(cbsa) %>% # add one week lag
      mutate(across(cbsa_w_n_cases:cbsa_w_i_n_deaths, ~lag(.x, 7), .names = "l_{col}")) %>% 
      mutate(across(matches("(?=.*cbsa)(?=.*_n)", perl = T), ~ .x*100/cbsa_pop)) %>% 
      select(date, cbsa, cbsa_w_n_cases:l_cbsa_w_i_n_deaths),
    by = c("date", "cbsa")) %>% 
  left_join( # aggregate to cbsa levels
    filter(., central_outlying == "Neighboring") %>% 
      distinct(date, cbsa, fips, .keep_all = T) %>%
      group_by(date, cbsa) %>% 
      mutate(across(w_n_cases:w_i_n_deaths, ~sum(.x), .names = "neigh_{col}")) %>% 
      ungroup() %>% 
      distinct(date, cbsa, .keep_all = T) %>% 
      group_by(cbsa) %>% # add one week lag
      mutate(across(neigh_w_n_cases:neigh_w_i_n_deaths, ~lag(.x, 7), .names = "l_{col}")) %>% 
      mutate(across(matches("(?=.*neigh)(?=.*_n)", perl = T), ~ .x*100/cbsa_pop)) %>% 
      select(date, cbsa, neigh_w_n_cases:l_neigh_w_i_n_deaths),
    by = c("date", "cbsa")) # 187775 obs, 40 vars  - 4/27/2023. you get errors from my_impute function

dat.covid.2020 <- read_csv("assets/covid_data/raw.covid2020.csv") %>% 
  as_tibble() %>% 
  relocate(fips, state, .before = county) %>% 
  # The 5 boroughs are listed as "New York City" in county var, and NA in fips. ...
  # ... I made it so NYC all under Bronx fip. 309 unique cbsa fips now. So 313 + 403 - 4 - 10 = 702 unique fips
  mutate(fips = case_when(county == "New York City" ~ "36005", T ~ fips)) %>% 
  filter(fips %in% (dat.cbsa %>% pull(fips))) %>% 
  # Jan 6th 2020 is the first monday of the year, and april 5th is a sunday
  filter("2020-01-05" <= date & date <= "2020-04-05") %>% # keep a day before 2020-01-05 for lag calculation
  left_join(dat.cbsa %>% select(cbsa, cbsa_title, home, fips, central_outlying, cbsa_pop), by = "fips", relationship = "many-to-many") %>% 
  relocate(cbsa:home, .after = date) %>% 
  relocate(central_outlying:cbsa_pop, .after = county) %>% 
  mutate(floor_monday = floor_date(date, "week", 1), .after = date) %>%  # week identifier
  group_by(fips, cbsa) %>% # need central_outlying to adequately identify counties, since we have duplicates.
  mutate(across(cases:deaths, ~case_when( # create new cases and deaths
    row_number() == 1 ~ NA_real_, 
    T ~ .x - lag(.x)
  ), 
  .names = "n_{col}"
  )) %>%  
  ungroup() %>% 
  filter(date > "2020-01-05") %>% # we just needed this to calculate new cases for 2020-01-06
  # we now have the same fips as dat.cbsa, but 4 less from NYC so 725 FIPs each with 259 observations. ...
  # 187775 total, 23 duplicate FIPs (still, the removed counties were not duplicates)
  mutate(across(n_cases:n_deaths, ~case_when( # impute negatives to NAs
    .x <= 0 ~ NA,
    T ~ .x
  ),
  .names = "i_{col}"
  )) %>% 
  group_by(cbsa, fips) %>% 
  mutate(across(i_n_cases:i_n_deaths, ~round(my_impute(.x), digits = 0))) %>% 
  mutate(across(i_n_cases:i_n_deaths, ~case_when(is.na(.x) ~ 0, T ~ .x))) %>% 
  # Any leftover NAs just mean there was something like c(..., 100, 0, -3, 0, 0) ...
  # ... at the end of the data set, so we are fine setting them to 0.
  # gets the rolling weekly cases figure, and the lag of that.
  mutate(across(n_cases:i_n_deaths, ~ .x + lag(.x) + lag(.x, 2) + lag(.x, 3) + lag(.x, 4) + lag(.x, 5) + lag(.x, 6),
                .names = "w_{col}")) %>% 
  mutate(across(w_n_cases:w_i_n_deaths, ~ lag(get(cur_column()), 7),
                .names = "l_{col}")) %>% 
  ungroup() %>% # aggregate to cbsa levels
  left_join( 
    filter(., central_outlying != "Neighboring") %>% 
      distinct(date, cbsa, fips, .keep_all = T) %>%
      group_by(date, cbsa) %>% 
      mutate(across(w_n_cases:w_i_n_deaths, ~sum(.x), .names = "cbsa_{col}")) %>% 
      ungroup() %>% 
      distinct(date, cbsa, .keep_all = T) %>% 
      group_by(cbsa) %>% # add one week lag
      mutate(across(cbsa_w_n_cases:cbsa_w_i_n_deaths, ~lag(.x, 7), .names = "l_{col}")) %>% 
      mutate(across(matches("(?=.*cbsa)(?=.*_n)", perl = T), ~ .x*100/cbsa_pop)) %>% 
      select(date, cbsa, cbsa_w_n_cases:l_cbsa_w_i_n_deaths),
    by = c("date", "cbsa")) %>% 
  left_join( # aggregate to cbsa levels
    filter(., central_outlying == "Neighboring") %>% 
      distinct(date, cbsa, fips, .keep_all = T) %>%
      group_by(date, cbsa) %>% 
      mutate(across(w_n_cases:w_i_n_deaths, ~sum(.x), .names = "neigh_{col}")) %>% 
      ungroup() %>% 
      distinct(date, cbsa, .keep_all = T) %>% 
      group_by(cbsa) %>% # add one week lag
      mutate(across(neigh_w_n_cases:neigh_w_i_n_deaths, ~lag(.x, 7), .names = "l_{col}")) %>% 
      mutate(across(matches("(?=.*neigh)(?=.*_n)", perl = T), ~ .x*100/cbsa_pop)) %>% 
      select(date, cbsa, neigh_w_n_cases:l_neigh_w_i_n_deaths),
    by = c("date", "cbsa")) # 13019 obs, 40 vars  - 4/27/2023. you get errors from my_impute function

# Betting Data -----------------------------------------------------------------

# load already cleaned betting data
attach_source("bet.R", "betting_data")
dat.bet <- load_clean_bet() # 13320 obs, 7 vars - 4/27/2023. Now 15373 - 5/31/2023

# Game Data Cleaning -----------------------------------------------------------

# clean NBA data 
dat.nba <- mass_load("assets/game_data/nba/", 1, .bind = T) %>% # 7133 observations
  as_tibble() %>% 
  mutate(across(everything(), ~str_replace(.x, "\\?", ""))) %>% 
  filter(home_record != "") %>% # -9 -> remove allstar, USA games
  filter(home_score != "NA") %>% # -3 -> removes postponed game (1) and 2 cancelled 3/11/2020 covid games
  filter(home != "Toronto Raptors") %>% # -237, 41*5 + 32 games from 2019-20 season
  filter(stadium %notin% c("Arena Ciudad de Mexico", "Mexico City Arena", "O2 Arena (ENG)", 
  "UW-Milwaukee Panther Arena", "Accor Arena")) %>% # -15 -> this last *2* stadium(s) was (were) only played in one time
  mutate(across(home:away, ~case_when( 
    .x == "LA Clippers" ~ "Los Angeles Clippers",
    T ~ .x
  ))) %>% 
  # filling in missing attendance figures below. Missing from the actual ESPN site
  mutate(attendance = case_when(
    date == "2021-11-03" & home == "Sacramento Kings" ~ "Attendance: 12,480", # https://www.nba.com/game/nop-vs-sac-0022100118?watch
    date == "2021-10-26" & home == "Oklahoma City Thunder" ~ "Attendance: 15,717", # https://www.nba.com/game/gsw-vs-okc-0022100051    
    date == "2019-04-09" & home == "Utah Jazz" ~ "Attendance: 18,306", # https://www.nba.com/game/den-vs-uta-0021801217?watch
    date == "2021-10-27" & home == "Boston Celtics" ~ "Attendance: 19,156", # https://www.nba.com/game/was-vs-bos-0022100056?watch
    date == "2017-11-08" & home == "Orlando Magic" ~ "Attendance: 18,803", # https://www.nba.com/game/nyk-vs-orl-0021700160
    T ~ attendance
  )) %>% 
  mutate(league = "NBA", .after = date) # 5869 obs 12 vars - 5/31/2023

# clean NHL data 
dat.nhl <- mass_load("assets/game_data/nhl/", 1, .bind = T) %>% # 7508 observations
  as_tibble() %>% 
  mutate(across(everything(), ~str_replace(.x, "\\?", ""))) %>% 
  filter(home_record != "") %>% # -18 -> remove allstar games
  filter(home_score != "NA") %>% # -101 -> removes postponed game, none in 2019-20 season
  filter(home %notin% c("Calgary Flames", "Edmonton Oilers", "Montreal Canadiens", 
                        "Ottawa Senators", "Toronto Maple Leafs", "Vancouver Canucks", 
                        "Winnipeg Jets", "Seattle Kraken")) %>% # -1723, 41*7*5 + 41 (Kraken) + 247 (from 2019 season)
  filter(stadium %notin% c("Avicii Arena", "Citi Field", "Ericsson Globe", "Hartwall Areena", 
                           "Lincoln Financial Field", "Navy-Marine Corps Memorial Stadium",
                           "Nissan Stadium", "Notre Dame Stadium", "Scandinavium",
                           "Target Field", "Tim Hortons Field", "Scotiabank Arena", 
                           "NA", "Rogers Place", "O2 Arena")) %>% # -28. NA just seems to be winter classics. Diffiuclt to tell how many teach season, as some were played by canadian teams and already filtered out above
  # Add in missing games from -> https://www.espn.com/nhl/boxscore/_/gameId/400814774
  # https://www.hockey-reference.com/boxscores/201510080BUF.html https://www.espn.com/nhl/game/_/gameId/400814776
  # https://www.hockey-reference.com/boxscores/201510080DAL.html https://www.espn.com/nhl/game/_/gameId/400814778
  # https://www.hockey-reference.com/boxscores/201510080STL.html https://www.espn.com/nhl/game/_/gameId/400814780
  bind_rows(tibble(date = rep("2015-10-08", 7),
                   home = c("Boston Bruins", "Buffalo Sabres", "Colorado Avalanche",
                            "Dallas Stars","Nashville Predators", "St. Louis Blues", 
                            "Tampa Bay Lightning"),
                   away = c("Winnipeg Jets", "Ottawa Senators", "Minnesota Wild", 
                            "Pittsburgh Penguins", "Carolina Hurricanes", "Edmonton Oilers",
                            "Philadelphia Flyers"),
                   home_record = c("0-1-0", "0-1-0", "0-1-0", "1-0-0", "1-0-0", "1-0-0",
                                   "1-0-0"),
                   away_record = c("1-0-0", "1-0-0", "1-0-0", "0-1-0", "0-1-0", "0-1-0", 
                                   "0-0-1"),
                   home_score = c(2, 1, 4, 3, 2, 3, 3) %>% as.character(),
                   away_score = c(6, 3, 5, 0, 1, 1, 2) %>% as.character(),
                   attendance = c("Attendance: 17,565", "Attendance: 19,070", "Attendance: 18,007",
                                  "Attendance: 18,532", "Attendance: 17,204", "Attendance: 19,327",
                                  "Attendance: 19,092"),
                   capacity = c("Capacity: 17,850", "Capacity: 19,070", "Capacity: 18,007",
                                "Capacity: 18,532", "Capacity: 17,113", "Capacity: 18,096", 
                                "Capacity: 19,092"),
                   stadium = c("TD Garden", "KeyBank Center", "Ball Arena", "American Airlines Center",
                               "Bridgestone Arena", "Enterprise Center", "Amalie Arena"),
                   game_time = c("7:00 PM, October 8, 2015", "7:00 PM, October 8, 2015", 
                                 "9:00 PM, October 8, 2015", "8:30 PM, October 8, 2015",
                                 "8:00 PM, October 8, 2015", "8:00 PM, October 8, 2015",
                                 "7:30 PM, October 8, 2015"))) %>% 
  arrange(date) %>% 
  mutate(league = "NHL", .after = date) # 5645 obs, 12 vars - 5/31/2023

# Final Merge ------------------------------------------------------------------
  
# merge everything together, prep for regression
dat.final <- dat.nba %>% 
  bind_rows(dat.nhl) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(across(home_score:away_score, ~as.numeric(.x))) %>% 
  mutate(across(attendance:capacity, ~str_extract(.x, "(?<=: )[\\s\\S]*") %>% 
                  str_replace(",", "") %>% 
                  as.numeric())) %>% 
  mutate(attendance_per = attendance*100/capacity) %>% 
  relocate(attendance_per, .after = capacity) %>% # up to here all just string cleaning
  mutate(across(c(home_record, away_record, game_time), ~str_extract(.x, "^[^,]*"))) %>% 
  left_join(dat.bet %>% select(-home_odds, -away_odds), by = c("date", "home")) %>% 
  relocate(adj_home_odds:adj_away_odds, .after = away_score) %>% 
  left_join(dat.cbsa %>% # get cbsa-level vars, 4 vars
              select(cbsa:time_zone) %>% 
              unnest(home) %>% 
              distinct(home, .keep_all = T),
            by = "home") %>% 
  relocate(cbsa:time_zone, .after = date) %>% 
  left_join(dat.covid %>% # merging covid-19 data, 16 vars
              distinct(date, cbsa, .keep_all = T) %>% 
              select(date, cbsa, cbsa_w_n_cases:ncol(.)),
            by = c("date", "cbsa")) %>% 
  mutate(floor_monday = floor_date(date, "week", 1), .after = date) %>% 
  relocate(season, .after = floor_monday) %>% 
  mutate(game_time_num = case_when( # make 24 hour time. All games in PM
    time_zone == "EST" ~ str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric(),
    time_zone == "CST" ~ str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric() - 1,
    time_zone == "MST" ~ str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric() - 2,
    T ~ str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric() - 3 
  ) + # adjust for time weirdness
    case_when(str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric() >= 12 ~ 0, T ~ 12), 
  .after = game_time) %>% 
  mutate(game_time = case_when( # adjust time
    game_time_num < 12 ~ format(round(game_time_num, 2), nsmall = 2) %>% paste("AM") %>% str_replace("\\.", ":"),
    T ~ format(round(game_time_num, 2) - 12, nsmall = 2) %>% paste("PM") %>% str_replace("\\.", ":"),
  )) %>% 
  mutate(game_time_approx = case_when( # forward inclusive, like an 8:59pm game is still "Evening Game"
    game_time_num < 18 ~ "Early Game", # includes, for example, 5:15pm
    18 <= game_time_num & game_time_num <= 20 ~ "Evening Game",
    game_time_num > 20 ~ "Night Game" 
  ), .after = game_time_num) %>% 
  mutate(game_time_approx = factor(game_time_approx, levels = c("Evening Game", "Early Game", "Night Game"))) %>% 
  mutate(policy = policy_func(home, date), .after = stadium) %>% 
  mutate(weekday = weekdays(date), .after = date) %>% 
  mutate(policy = case_when(is.na(policy) ~ "none", T ~ policy)) %>% 
  mutate(month = month(date, label = T, abbr = F) %>% as.character(), .before = weekday) %>% 
  mutate(season_wins = str_sub(home_record, 1L, 2L), .before = home_record) %>% 
  group_by(season, home) %>% 
  mutate(season_wins = as.numeric(last(season_wins))) %>% 
  ungroup() %>% # need to use distinct because not an equal number of entries per team per season (cause of mexico games and such)
  left_join(distinct(., season, league, home, .keep_all = T) %>% 
              group_by(season, league) %>% 
              mutate(season_wins_scaled = as.numeric(scale(season_wins))) %>% 
              ungroup() %>% 
              select(season, home, season_wins_scaled),
            by = c("season", "home")) %>% 
  relocate(season_wins_scaled, .after = season_wins) %>% 
  mutate(policy = factor(policy, levels = c("none", "mask", "vaccine", "both"))) %>% # 10748 obs, 44 vars - 4/27/2023
  mutate(across(contains("_w_"), ~case_when(
    is.na(.x) ~ 0,
    T ~ .x
  )))
  
# Leaflet Data -----------------------------------------------------------------

# 200 by 710 observations
dat.leaflet <- readRDS("assets/leaflet/2020_fips_shapes.rds") %>%
  select(GEOID, geometry) %>% 
  mutate(geometry = st_transform(geometry, '+proj=longlat +datum=WGS84')) %>% # prevents leaflet warnings
  rename(fips = GEOID) %>% 
  bind_rows(filter(., fips %in% c("36005", "36047", "36061", "36081", "36085")) %>% summarize(fips = "36005", geometry = st_union(geometry))) %>% 
  filter(row_number() != 471) %>% 
  right_join(dat.covid %>% 
               select(date, floor_monday, cbsa, cbsa_title, county, fips, central_outlying, w_n_cases, w_n_deaths, cbsa_w_n_cases, cbsa_w_n_deaths, neigh_cbsa_w_n_cases, neigh_cbsa_w_n_deaths), 
             by = "fips") %>% 
  filter("2021-10-12" <= date & date <= "2022-04-29") %>% # just the 2021-22 season
  left_join(dat.final %>% select(date, cbsa, season, home:game_time_approx), by = c("date", "cbsa")) %>% 
  relocate(season, home:game_time_approx, geometry, .after = central_outlying) %>% 
  group_by(fips) %>% 
  mutate(temp = (any(central_outlying != "Neighboring") & any(central_outlying == "Neighboring")), .after = fips) %>% 
  ungroup() %>% 
  filter(!(temp == T & central_outlying == "Neighboring")) %>% # removes cbsa-neighbor duplicates, keeps CBSA ones
  group_by(fips) %>% 
  mutate(temp = (first(cbsa) == cbsa), .after = fips) %>% 
  ungroup() %>% 
  filter(temp == T) %>% # removes neighbor-neighbor overlap.
  select(-temp) %>% 
  left_join(filter(., date == "2021-11-16") %>% # could be any day
              mutate(neighboring = case_when(central_outlying == "Neighboring" ~ T, T ~ F)) %>% 
              group_by(cbsa, neighboring) %>% 
              mutate(cbsa_geometry = st_union(geometry)) %>% 
              ungroup() %>% 
              distinct(fips, cbsa, .keep_all = T) %>% 
              select(fips, cbsa, cbsa_geometry) %>% 
              st_drop_geometry(geometry),
            by = c("fips", "cbsa")
  ) %>% 
  relocate(cbsa_geometry, .after = cbsa_title) %>% 
  mutate(across(c(home_score:away_score, attendance), ~as.integer(.x))) %>% 
  mutate(policy = case_when(
    policy == "vaccine" ~ "Vaccine Mandate",
    policy == "mask" ~ "Mask Mandate",
    policy == "both" ~ "Mask & Vaccine Mandate",
    T ~ "No Policy"
  )) %>% 
  left_join(st_drop_geometry(., geometry, cbsa_geometry) %>% # group all game data into a single data-frame column
              select(floor_monday, fips, date, home:away_score, stadium, policy, attendance) %>% 
              filter(!is.na(home)) %>% 
              mutate(date = as.character(date)) %>% 
              set_colnames(c("floor_monday", "fips", "Date", "Home Team", "Away Team", "Home Record", "Away Record", "Home Score", "Away Score", "Stadium", "Covid-19 Policy", "Attendance")) %>% 
              nest(game_data_nest = Date:Attendance),
            by = c("floor_monday", "fips")
  ) %>% 
  select(-(home:game_time)) %>% # all in data frame column
  group_by(fips, floor_monday) %>% 
  mutate(temp = row_number()) %>% # remove duplicates caused by multiples games in same cbsa on same day, just keep first.
  ungroup() %>% 
  filter(temp == 1) %>% 
  select(-temp) %>% # 710 unique fips times 29 weeks
  mutate(fip_label = paste0(
    "<dt style=font-weight:bold;font-size:12pt;>City Stats</dt>
        <li style=margin-left:15px;>CBSA Title: ", cbsa_title, "</li>
        <li style=margin-left:15px;>Weekly New Cases: ", cbsa_w_n_cases, "</li>
        <li style=margin-left:15px;>Weekly New Deaths: ", cbsa_w_n_deaths, "</li>
    <dt style=font-weight:bold;font-size:12pt;>Neighboring Counties Stats</dt>
        <li style=margin-left:15px;>Weekly New Cases: ", neigh_cbsa_w_n_cases, "</li>
        <li style=margin-left:15px;>Weekly New Deaths: ", neigh_cbsa_w_n_deaths, "</li>
    <dt style=font-weight:bold;font-size:12pt;>County Stats</dt>
        <li style=margin-left:15px;>County Title: ", county, "</li>
        <li style=margin-left:15px;>Weekly New Cases: ", w_n_cases, "</li>
        <li style=margin-left:15px;>Weekly New Deaths: ", w_n_deaths, "</li>"
  )) %>% 
  select(-game_time_num, -season, -game_time_approx) 

# fips data
dat.leaflet.fips <- dat.leaflet %>%
  select(floor_monday, cbsa, cbsa_title, game_data_nest, fips, county, fip_label, central_outlying, geometry, w_n_cases:neigh_cbsa_w_n_deaths) %>% 
  saveRDS("dat.leaflet.fips.rds")

# cbsa data
dat.leaflet.cbsa <- dat.leaflet %>% 
  st_drop_geometry() %>% 
  select(cbsa, cbsa_geometry) %>% 
  distinct(cbsa_geometry) %>% 
  saveRDS("dat.leaflet.cbsa.rds")








