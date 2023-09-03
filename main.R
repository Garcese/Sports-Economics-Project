############################################################################## #
### Sports Econ Project
############################################################################## #  

library(fastDummies) # used in policy graph
library(fixest) # regression stuff
# library(htmltools) # for leaflet
library(httr) # data-scraping
library(ivDiag)
# library(leaflet)
library(lubridate) # dates
library(magrittr) # set_colnames function
library(maps) # US map tiles
# library(openxlsx) # note sure if I actually need this anymore - 7/14/2023
library(patchwork) 
# library(performance) # not sure if I actually use this ... - 6/25/2023
# library(renv) # package manager
library(RCurl) # needed for something ...
# library(rsconnect)
library(rvest) # read_html function
library(scales)
# library(shiny)
# library(shinycssloaders)
# library(spdep) # spatial data, I think
# library(stargazer)
# library(tidycensus) # census data
library(tidyselect)
library(tidyverse)
# library(tigris) # census shape data
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
raw.nba.2015 <- scrape_season(nbaSeasons[1] %>% reduce(c), "nba") %>% 
    mutate(season = "2015-16")
# 2016-17 season. 1232 observations -> 1230 games, 1 allstar game, 1/7/2017 blazers postponed
raw.nba.2016 <- scrape_season(nbaSeasons[2] %>% reduce(c), "nba") %>% 
  mutate(season = "2016-17")
# 2017-18 season. 1232 observations -> 1230 games, 1 allstar game, USA game
raw.nba.2017 <- scrape_season(nbaSeasons[3] %>% reduce(c), "nba") %>% 
  mutate(season = "2017-18")
# 2018-19 season. 1232 observations -> 1230 games, 1 allstar game, USA game
raw.nba.2018 <- scrape_season(nbaSeasons[4] %>% reduce(c), "nba") %>% 
  mutate(season = "2018-19")
# 2019-2020 season, up to covid. 975 observations -> 971 games, 1 allstar game, USA game, Thunder game and Kings game cancelled on 3/11/2020
raw.nba.2019 <- scrape_season(nbaSeasons[5] %>% reduce(c), "nba") %>% 
  mutate(season = "2019-20")
# 2020-21 season. 1081 observations -> 1080 games, 1 allstar game. 72 instead of 82 games.
raw.nba.2020 <- scrape_season(nbaSeasons[6] %>% reduce(c), "nba", tries = 20) %>% 
  mutate(season = "2020-21")
# 2021-22 season. 1231 observations -> 1230 games, 1 allstar game
raw.nba.2021 <- scrape_season(nbaSeasons[7] %>% reduce(c), "nba") %>% 
  mutate(season = "2021-22")
# 2022-23 season. 1231 observations -> 1230 games, 1 allstar game
raw.nba.2022 <- scrape_season(nbaSeasons[8] %>% reduce(c), "nba", tries = 20) %>% 

# 2015-16 season. 1235 observations -> 1230 games, 3 allstar games, 1/23/2016 islanders  ...
# ... postponed, 1/24/2016 capitals postponed.
raw.nhl.2015 <- scrape_season(nhlSeasons[1] %>% reduce(c), "nhl") %>% 
  mutate(season = "2015-16")
# 2016-17 season. 1234 observations -> 1230 games, 3 allstar games, 3/14/2017 devils postponed
raw.nhl.2016 <- scrape_season(nhlSeasons[2] %>% reduce(c), "nhl") %>% 
  mutate(season = "2016-17")
# 2017-18 season. 1274 observations -> 1271 games, 3 allstar games. VGK joined this season
raw.nhl.2017 <- scrape_season(nhlSeasons[3] %>% reduce(c), "nhl") %>% 
  mutate(season = "2017-18")
# 2018-19 season. 1274 observations -> 1271 games, 3 allstar games
raw.nhl.2018 <- scrape_season(nhlSeasons[4] %>% reduce(c), "nhl") %>% 
  mutate(season = "2018-19")
# 2019-2020 season, up to covid. 1085 observations -> 1082 games, 3 allstar games
raw.nhl.2019 <- scrape_season(nhlSeasons[5] %>% reduce(c), "nhl") %>% 
  mutate(season = "2019-20")
# 2020-21 season. 56 instead of 82 games. 938 observations -> 868 games, the playoffs began on "2021-05-15", 
# but there were still a couple of regular season games going on. Code below removes the 14 playoff games. Additionally, there were 56 postponed games
raw.nhl.2020 <- scrape_season(nhlSeasons[6] %>% reduce(c), "nhl", tries = 20) %>%
  filter(!(home %notin% c("Calgary Flames?", "Edmonton Oilers?", "Vancouver Canucks?") & date > "2021-05-14")) %>% 
  filter(!(home == "Edmonton Oilers?" & date == "2021-05-19")) %>% 
  mutate(season = "2020-21")
# 2021-22 season. 1413 observations -> 1312 games, 3 allstar games, 98 postponed. Kraken joined
raw.nhl.2021 <- scrape_season(nhlSeasons[7] %>% reduce(c), "nhl") %>% 
  mutate(season = "2021-22")
# 2022-23 season. 1330 observations -> 1312 games, 3 allstar games, regular season started abroad 
# on 2022-10-07, and picked up again on 2022-10-11. There were 15 games inbetween that that were practice
raw.nhl.2022 <- scrape_season(nhlSeasons[8] %>% reduce(c), "nhl", tries = 20) %>% 
  filter(!(home %notin% c("San Jose Sharks?", "Nashville Predators?") & date < "2022-10-11")) %>% 
  mutate(season = "2022-23")

# write the data - it's probably just smarter to save the dataframes one at a time
# mass_write("assets/game_data/nba/", game_data_names %includes% "nba")
# mass_write("assets/game_data/nhl/", game_data_names %includes% "nhl")
# write_csv(test, "assets/game_data/nhl/raw.nhl.2022.csv")

# betting data 
attach_source("bet.R", "betting_data")
# write_bet("assets/betting_data/")

# collect Raw data, tigris package function
counties(cb = TRUE, resolution = "500k") %>% # 500k is best resolution
  # saveRDS(file = "assets/leaflet/2020_fips_shapes.rds") # geometry doesn't save nicely as CSV

# tidycensus package. Newest is 2019 apparently
# get_estimates(geography = "county", product = "population", year = 2019) %>% # this is old 7/10/2023
#   # write_csv("assets/cbsa_data/dat.population.csv")
get_estimates(geography = "county", variables = "POPESTIMATE", year = 2021) %>% 
  # write_csv("assets/cbsa_data/dat.population.2021.csv")
get_estimates(geography = "county", variables = "POPESTIMATE", year = 2022) %>% 
  # write_csv("assets/cbsa_data/dat.population.2022.csv")

# FIP/CBSA Data ----------------------------------------------------------------

# get neighboring counties
# dat.neighboring <- readRDS("assets/leaflet/2020_fips_shapes.rds") %>% # gets county geo-spatial data
#   select(GEOID, geometry) %>% 
#   set_colnames(c("fips", "geometry"))
# dat.neighboring <- dat.neighboring %>% # run geo-spatial list into a function that automatically detects adjacencies
#   pull(geometry) %>% 
#   poly2nb(row.names = dat.neighboring %>% pull(fips)) %>% 
#   neighborsDataFrame() %>% # expp library
#   group_by(id) %>% 
#   summarize(id_neigh = list(unique(id_neigh))) %>% 
#   ungroup() %>% 
#   set_colnames(c("fips", "fips_neighbors")) # 3214 obs, 2 vars - 8/7/2023

# because I can't fix expp
# saveRDS(dat.neighboring, "assets/dat.neighboring.rds")
dat.neighboring <- readRDS("assets/dat.neighboring.rds")

# clean population data for merge
# I manually had to put in Fairfield and Litchfield county into the data, b/c of bullshit. Sourced from worldpopulationreview.com
# In fact that didn't even have 2021 estimates, so I just summed the 2020 and 2022 estimates and divided by 2.
dat.population <- read_csv("assets/cbsa_data/dat.population.2021.csv", col_types = "ccccd") %>% 
  select(GEOID, year, value) %>% 
  set_colnames(c("fips", "year", "pop")) %>% 
  mutate(temp = case_when( # the code up to ! collapses the 5 boroughs, summing the pops tho
    fips %in% c("36061", "36005", "36047", "36081", "36085") ~ "36005",
    T ~ fips
  )) %>% 
  group_by(temp) %>% 
  mutate(fips = fips, year = year, pop = sum(pop)) %>% 
  ungroup() %>% # !
  select(-temp) %>% 
  bind_rows( # add 2022 data
    read_csv("assets/cbsa_data/dat.population.2022.csv", col_types = "ccccd") %>% 
      select(GEOID, year, value) %>% 
      set_colnames(c("fips", "year", "pop")) %>% 
      mutate(temp = case_when( # the code up to ! collapses the 5 boroughs, summing the pops tho
        fips %in% c("36061", "36005", "36047", "36081", "36085") ~ "36005",
        T ~ fips
      )) %>% 
      group_by(temp) %>% 
      mutate(fips = fips, year = year, pop = sum(pop)) %>% 
      ungroup() %>% # !
      select(-temp)
  ) %>% 
  pivot_wider(names_from = year, values_from = pop) %>% 
  set_colnames(c("fips", "pop_2021", "pop_2022")) # 3146 obs, 3 vars - 8/7/2023

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
  mutate(cbsa_states = cbsa_states %>% str_split("-")) %>% 
  mutate(home = assign_home(cbsa), .after = cbsa_title) %>% 
  mutate(home = home %>% str_split(",")) %>% 
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
  mutate(across(contains("pop_"), ~sum(.x), .names = "cbsa_{col}")) %>% 
  ungroup(temp) %>% 
  select(-temp) %>% 
  ungroup()
# 729 obs, 14 vars - 8/7/2023
# 313 unique CBSA-only fips, 416 neighbor-only fips, 403 unique neighbor-only fips. ...
# ... 23 duplicates. 13 neighbor-neighbor duplicates, 10 neighbor-cbsa duplicates. ...
# ... together, there are 313 + 403 - 10 = 706 unique fips. This includes all 5 boroughs, so really 702. The pop for the 5 boroughs is summed.

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
  left_join(dat.cbsa %>% select(cbsa, cbsa_title, home, fips, central_outlying, cbsa_pop_2021, cbsa_pop_2022), by = "fips", relationship = "many-to-many") %>% 
  relocate(cbsa:home, .after = date) %>% 
  relocate(central_outlying:cbsa_pop_2022, .after = county) %>% 
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
      mutate(across(matches("(?=.*(?:cbsa|neigh))(?=.*_n_cases)", perl = T), ~case_when(
        date < "2022-01-01" ~ .x*100/cbsa_pop_2021,
        T ~ .x*100/cbsa_pop_2022,
        )
        )) %>% 
      mutate(across(matches("(?=.*(?:cbsa|neigh))(?=.*_n_deaths)", perl = T), ~case_when(
        date < "2022-01-01" ~ .x*100000/cbsa_pop_2021,
        T ~ .x*100000/cbsa_pop_2022,
      )
      )) %>% 
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
      mutate(across(matches("(?=.*(?:cbsa|neigh))(?=.*_n_cases)", perl = T), ~case_when(
        date < "2022-01-01" ~ .x*100/cbsa_pop_2021,
        T ~ .x*100/cbsa_pop_2022,
      )
      )) %>% 
      mutate(across(matches("(?=.*(?:cbsa|neigh))(?=.*_n_deaths)", perl = T), ~case_when(
        date < "2022-01-01" ~ .x*100000/cbsa_pop_2021,
        T ~ .x*100000/cbsa_pop_2022,
      )
      )) %>% 
      select(date, cbsa, neigh_w_n_cases:l_neigh_w_i_n_deaths),
    by = c("date", "cbsa")) # 187775 obs, 41 vars  - 8/7/2023. you get errors from my_impute function

# THIS IS CURRENTLY NOT BEING USED. WE AREN'T USING COVID-19 DATA IN THE MULTI-SEASON REGRESSION
# This also isn't updated to the newest code, specicially, population data will be messed up, and I forgot to divide neigh by population.
# dat.covid.2020 <- read_csv("assets/covid_data/raw.covid2020.csv") %>% 
#   as_tibble() %>% 
#   relocate(fips, state, .before = county) %>% 
#   # The 5 boroughs are listed as "New York City" in county var, and NA in fips. ...
#   # ... I made it so NYC all under Bronx fip. 309 unique cbsa fips now. So 313 + 403 - 4 - 10 = 702 unique fips
#   mutate(fips = case_when(county == "New York City" ~ "36005", T ~ fips)) %>% 
#   filter(fips %in% (dat.cbsa %>% pull(fips))) %>% 
#   # Jan 6th 2020 is the first monday of the year, and april 5th is a sunday
#   filter("2020-01-05" <= date & date <= "2020-04-05") %>% # keep a day before 2020-01-05 for lag calculation
#   left_join(dat.cbsa %>% select(cbsa, cbsa_title, home, fips, central_outlying, cbsa_pop), by = "fips", relationship = "many-to-many") %>% 
#   relocate(cbsa:home, .after = date) %>% 
#   relocate(central_outlying:cbsa_pop, .after = county) %>% 
#   mutate(floor_monday = floor_date(date, "week", 1), .after = date) %>%  # week identifier
#   group_by(fips, cbsa) %>% # need central_outlying to adequately identify counties, since we have duplicates.
#   mutate(across(cases:deaths, ~case_when( # create new cases and deaths
#     row_number() == 1 ~ NA_real_, 
#     T ~ .x - lag(.x)
#   ), 
#   .names = "n_{col}"
#   )) %>%  
#   ungroup() %>% 
#   filter(date > "2020-01-05") %>% # we just needed this to calculate new cases for 2020-01-06
#   # we now have the same fips as dat.cbsa, but 4 less from NYC so 725 FIPs each with 259 observations. ...
#   # 187775 total, 23 duplicate FIPs (still, the removed counties were not duplicates)
#   mutate(across(n_cases:n_deaths, ~case_when( # impute negatives to NAs
#     .x <= 0 ~ NA,
#     T ~ .x
#   ),
#   .names = "i_{col}"
#   )) %>% 
#   group_by(cbsa, fips) %>% 
#   mutate(across(i_n_cases:i_n_deaths, ~round(my_impute(.x), digits = 0))) %>% 
#   mutate(across(i_n_cases:i_n_deaths, ~case_when(is.na(.x) ~ 0, T ~ .x))) %>% 
#   # Any leftover NAs just mean there was something like c(..., 100, 0, -3, 0, 0) ...
#   # ... at the end of the data set, so we are fine setting them to 0.
#   # gets the rolling weekly cases figure, and the lag of that.
#   mutate(across(n_cases:i_n_deaths, ~ .x + lag(.x) + lag(.x, 2) + lag(.x, 3) + lag(.x, 4) + lag(.x, 5) + lag(.x, 6),
#                 .names = "w_{col}")) %>% 
#   mutate(across(w_n_cases:w_i_n_deaths, ~ lag(get(cur_column()), 7),
#                 .names = "l_{col}")) %>% 
#   ungroup() %>% # aggregate to cbsa levels
#   left_join( 
#     filter(., central_outlying != "Neighboring") %>% 
#       distinct(date, cbsa, fips, .keep_all = T) %>%
#       group_by(date, cbsa) %>% 
#       mutate(across(w_n_cases:w_i_n_deaths, ~sum(.x), .names = "cbsa_{col}")) %>% 
#       ungroup() %>% 
#       distinct(date, cbsa, .keep_all = T) %>% 
#       group_by(cbsa) %>% # add one week lag
#       mutate(across(cbsa_w_n_cases:cbsa_w_i_n_deaths, ~lag(.x, 7), .names = "l_{col}")) %>% 
#       mutate(across(matches("(?=.*cbsa)(?=.*_n)", perl = T), ~ .x*100000/cbsa_pop)) %>% 
#       select(date, cbsa, cbsa_w_n_cases:l_cbsa_w_i_n_deaths),
#     by = c("date", "cbsa")) %>% 
#   left_join( # aggregate to cbsa levels
#     filter(., central_outlying == "Neighboring") %>% 
#       distinct(date, cbsa, fips, .keep_all = T) %>%
#       group_by(date, cbsa) %>% 
#       mutate(across(w_n_cases:w_i_n_deaths, ~sum(.x), .names = "neigh_{col}")) %>% 
#       ungroup() %>% 
#       distinct(date, cbsa, .keep_all = T) %>% 
#       group_by(cbsa) %>% # add one week lag
#       mutate(across(neigh_w_n_cases:neigh_w_i_n_deaths, ~lag(.x, 7), .names = "l_{col}")) %>% 
#       mutate(across(matches("(?=.*neigh)(?=.*_n)", perl = T), ~ .x*100000/cbsa_pop)) %>% 
#       select(date, cbsa, neigh_w_n_cases:l_neigh_w_i_n_deaths),
#     by = c("date", "cbsa")) # 13019 obs, 40 vars  - 6/25/2023. you get errors from my_impute function

# Betting Data -----------------------------------------------------------------

# load already cleaned betting data
attach_source("bet.R", "betting_data")
dat.bet <- load_clean_bet() # 6 vars, 18499 obs - 8/7/2023

# Game Data Cleaning -----------------------------------------------------------

# clean NBA data 
dat.nba <- mass_load("assets/game_data/nba/", 1, .bind = T) %>% # 9445 observations - 7/14/2023
  as_tibble() %>% 
  mutate(across(everything(), ~str_replace(.x, "\\?", ""))) %>% 
  filter(home_record != "") %>% # -11 -> remove allstar, USA games
  filter(home_score != "NA") %>% # -3 -> removes postponed game (1) and 2 cancelled 3/11/2020 covid games
  mutate(across(home:away, ~case_when( 
    .x == "LA Clippers" ~ "Los Angeles Clippers",
    T ~ .x
  ))) %>% 
  # filling in missing attendance figures below. Missing from the actual ESPN site. Listed as NA in the data
  # Any game listed as NA in the 2020-21 season, I'm marking as 0, without checking. Any other instance of NA I'm looking into.
  # Same with any raptors game in the 2021-22 season, automatically 0 if NA.
  # The cavs played a game on 2022-10-26 and I can see fans in the stadium but both ESPN and NBA.com lists 0 attendance.
  # Same with a 2023-03-22 jazz game - both say 0 attendance. Didn't check video.
  mutate(attendance = case_when(
    date == "2021-11-03" & home == "Sacramento Kings" ~ "Attendance: 12,480", # https://www.nba.com/game/nop-vs-sac-0022100118?watch
    date == "2021-10-26" & home == "Oklahoma City Thunder" ~ "Attendance: 15,717", # https://www.nba.com/game/gsw-vs-okc-0022100051    
    date == "2019-04-09" & home == "Utah Jazz" ~ "Attendance: 18,306", # https://www.nba.com/game/den-vs-uta-0021801217?watch
    date == "2021-10-27" & home == "Boston Celtics" ~ "Attendance: 19,156", # https://www.nba.com/game/was-vs-bos-0022100056?watch
    date == "2017-11-08" & home == "Orlando Magic" ~ "Attendance: 18,803", # https://www.nba.com/game/nyk-vs-orl-0021700160
    date == "2022-10-25" & home == "Phoenix Suns" ~ "Attendance: 17,071", # https://www.nba.com/game/gsw-vs-phx-0022200055
    date == "2022-11-14" & home == "Boston Celtics" ~ "Attendance: 19,156", # https://www.nba.com/game/0022200201
    T ~ attendance
  )) %>% 
  mutate(attendance = case_when(
    attendance == "NA" ~ "Attendance: 0",
    T ~ attendance
  )) %>% 
  mutate(league = "NBA", .after = date) # 9431 obs, 13 vars - 8/7/2023.

# clean NHL data 
dat.nhl <- mass_load("assets/game_data/nhl/", 1, .bind = T) %>% # 9754 observations - 7/14/2023
  as_tibble() %>% 
  mutate(across(everything(), ~str_replace(.x, "\\?", ""))) %>% 
  filter(home_record != "") %>% # -21 -> remove allstar games
  filter(home_score != "NA") %>% # -101 + (- 56) -> removes postponed game, none in 2019-20 season. Lots (56) in 2020-21
  # Seems like some 2022-23 games accidentally were marked NA, those 7 games fixed below. Otherwise, 2021-22 or 2020-21, marked 0. Those seem to be correctly NA.
  mutate(attendance = case_when(
    date == "2023-03-25" & home == "New York Islanders" ~ "Attendance: 17,255", # https://www.hockey-reference.com/boxscores/202303250NYI.html
    date == "2023-02-12" & home == "Washington Capitals" ~ "Attendance: 18,573", # https://www.hockey-reference.com/boxscores/202302120WSH.html
    date == "2022-12-31" & home == "Vegas Golden Knights" ~ "Attendance: 18,333", # https://www.hockey-reference.com/boxscores/202212310VEG.html
    date == "2023-04-02" & home == "Calgary Flames" ~ "Attendance: 17,439", # https://www.hockey-reference.com/boxscores/202304020CGY.html
    date == "2023-01-18" & home == "Calgary Flames" ~ "Attendance: 17,768", # https://www.hockey-reference.com/boxscores/202301180CGY.html
    date == "2022-11-29" & home == "Winnipeg Jets" ~ "Attendance: 13,510", # https://www.hockey-reference.com/boxscores/202211290WPG.html
    date == "2022-11-21" & home == "Winnipeg Jets" ~ "Attendance: 13,346", # https://www.hockey-reference.com/boxscores/202211210WPG.html
    date == "" & home == "" ~ "Attendance: ", # 	
    T ~ attendance
  )) %>% 
  mutate(attendance = case_when(
    attendance == "NA" ~ "Attendance: 0",
    T ~ attendance
  )) %>% 
  mutate(league = "NHL", .after = date) # 9576 obs, 13 vars - 8/7/2023

# Final Merge ------------------------------------------------------------------

# merge everything together, prep for regression
dat.final <- dat.nba %>% # 19007 obs, 13 vars - 7/13/2023.
  bind_rows(dat.nhl) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(across(home_score:away_score, ~as.numeric(.x))) %>% 
  mutate(across(attendance:capacity, ~str_extract(.x, "(?<=: )[\\s\\S]*") %>% 
                  str_replace(",", "") %>% 
                  as.numeric())) %>% 
  mutate(capacity = case_when( # for some reason, ESPN didn't have capacity data on Amalie arena in 2020-21 NBA season.
    stadium == "Amalie Arena" ~ 19092,
    T ~ capacity
  )) %>% 
  mutate(attendance_per = attendance*100/capacity) %>% 
  group_by(season, home) %>% 
  mutate(game_number = row_number(), .after = away_score) %>% 
  ungroup() %>% 
  relocate(attendance_per, .after = capacity) %>% # up to here all just string cleaning
  mutate(across(c(home_record, away_record, game_time), ~str_extract(.x, "^[^,]*"))) %>% 
  semi_join(group_by(., league, season, home) %>% # this beautiful code courtesy of chatGPT removes all observations ...
              count(stadium, sort = T) %>% # ... that contained a weird stadium, per league, per season, per home team
              filter(n >= 10) %>% # it keeps the stadium if it was played in more than 10 times that season
              ungroup(), # it removes 64 observations, which is expected
            by = c("league", "season", "home", "stadium")) %>% 
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
  # mutate(policy = case_when(is.na(policy) ~ "none", T ~ policy)) %>% 
  mutate(month = month(date, label = T, abbr = F) %>% as.character(), .before = weekday) %>% 
  mutate(season_wins = str_sub(home_record, 1L, 2L), .before = home_record) %>% 
  mutate(season_draws = case_when(
    league == "NHL" ~ str_extract(home_record, "(?<=-).*?(?=-)"), # match characters between dashes
    T ~ "0"
    ), .after = season_wins) %>% 
  group_by(season, home) %>% 
  mutate(across(season_wins:season_draws, ~as.numeric(last(.x)))) %>% 
  ungroup() %>% 
  mutate(win_percent = season_wins + season_draws/2, .after = season_draws) %>% 
  mutate(win_percent = case_when(
    season %notin% c("2019-20", "2020-21") ~ win_percent/82,
    league == "NBA" & season == "2020-21" ~ win_percent/72,
    league == "NHL" & season == "2020-21" ~ win_percent/56,
    T ~ win_percent
  )) %>% 
  left_join(
    distinct(., home, season, .keep_all = T) %>%
      group_by(home) %>% 
      mutate(lag_win_percent = lag(win_percent), .after = win_percent) %>% 
      ungroup() %>% 
      select(home, season, lag_win_percent),
      by = c("home", "season")
      ) %>% 
  relocate(lag_win_percent, .after = win_percent) %>% 
  mutate(lag_win_percent = case_when( # number of games in 2019-20 was 72 and 56 for NBA and NHL. Remember, NHL is in points ...
    season == "2015-16" & league == "NBA" ~ winrate_helper(home, season)/82, # ... so multiply denominator by 2
    season == "2015-16" & league == "NHL" ~ winrate_helper(home, season)/164,
    season == "2020-21" & league == "NBA" ~ winrate_helper(home, season)/72,
    season == "2020-21" & league == "NHL" ~ winrate_helper(home, season)/112,
    T ~ lag_win_percent
  )) %>% 
  mutate(lag_win_percent = case_when( # VGK and SK first seasons, set them to 0.5 win percent
    is.na(lag_win_percent) ~ 0.5,
    T ~ lag_win_percent
  )) %>% 
  mutate(policy = factor(policy, levels = c("none", "mask", "vaccine", "both"))) # 18943 obs, 47 vars - 8/7/2023
  




