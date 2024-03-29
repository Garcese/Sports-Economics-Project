############################################################################## #
### Plots and Tables and Regressions for Sports Econ Project
############################################################################## #  

# Tables ------------------------------------------------------------

# general stats for numeric var
dat.final %>% 
  filter(season == "2021-22" & !is.na(cbsa)) %>%
  group_by(league) %>% 
  summary_stats(neigh_cbsa_w_n_cases_per) %>% # change to desired variable
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3))))

# League-level Home-level standard deviation
dat.final %>% 
  filter(season == "2021-22" & !is.na(cbsa)) %>%
  group_by(league, home) %>% 
  summarize(home_sd = psd(attendance_per))%>% 
  ungroup() %>% 
  group_by(league) %>% 
  summary_stats("home_sd") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 4))))

# for categorical variables
dat.final %>% 
  filter(season == "2021-22" & !is.na(cbsa)) %>%
  group_by(league, game_time_approx) %>% 
  summarize(
    n = n()
  ) %>% 
  ungroup() %>% 
  group_by(league) %>% 
  mutate(nprop = n/sum(n))

# Just the start of the season
dat.final %>% 
  filter(!is.na(cbsa) & season == "2021-22") %>% 
  group_by(home) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  group_by(league, policy) %>% 
  summarize(
    n = n()
  ) %>% 
  ungroup() %>% 
  group_by(league) %>% 
  mutate(nprop = n/sum(n))

# general stats for numeric var, non-covid years
dat.final %>% 
  filter(season %notin% c("2019-20", "2020-21", "2022-23") & !is.na(cbsa)) %>%
  group_by(season, league) %>% 
  summary_stats(attendance_per) %>% # change to desired variable
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3)))) %>% 
  filter(str_detect(variable, "mean"))

# Figure 1 Plots ---------------------------------------------------------------

plot_distribution <- function(.league, .var = "cbsa_w_n_cases_per") {
  if (str_detect(.var, "cases")) {
    x_var <- "Cases"
  }
  else {
    x_var <- .var
  }
  # 
  dat.final %>% 
    filter(season == "2021-22" & league == .league & !is.na(cbsa)) %>% 
    ggplot(aes(x = !!sym(.var), fill = "Quartiles")) + 
    geom_density(fill = "#CCCCCC") + 
    geom_hline(yintercept = 0) + 
    labs(
      title = paste("Game-level Distribution of COVID-19", x_var),
      x = x_var,
      y = "Density",
    ) +
    scale_x_continuous(
      label = percent_format(scale = 1),
    ) 
    # scale_y_continuous( ?????? should this be? # ASK ANDERSON
    #   label = percent_format(scale = 1),
    # ) + 
}

plot_cases <- function(.league) {
  if (.league == "NBA") {
    date.start <- "2021-10-19"
    date.end <- "2022-04-10"
  }
  else {
    date.start <- "2021-10-12"
    date.end <- "2022-05-01"
  }
  #
  dat.final %>% 
    filter(season == "2021-22" & league == .league & !is.na(cbsa)) %>% 
    #
    ggplot(aes(x = date)) +
    geom_point(aes(y = cbsa_w_n_cases_per), size = 0.5) +
    scale_x_date(
      date_labels = "%b", 
      date_breaks = "1 month" 
    ) +
    scale_y_continuous(
      name = "Cases",
      label = percent_format(scale = 1),
    ) +
    geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(as.Date(date.start), as.Date(date.end))) +
    labs(title = paste("COVID-19 Cases Throughout Season"), x = "Month")
}

plot_policy <- function(.league, .legend = T) {
  if (.league == "NBA") {
    date.start <- "2021-10-19"
    date.end <- "2022-04-10"
  }
  else {
    date.start <- "2021-10-12"
    date.end <- "2022-05-01"
  }
  #
  plot <- dat.final %>% 
    filter(season == "2021-22" & league == .league & !is.na(cbsa)) %>% 
    select(-policy) %>% 
    left_join(
      tibble(
        date = rep(seq(as.Date("2021-10-12"), as.Date("2022-04-29"), by = "days"), 53),
        league_home = dat.final %>%
          filter(!is.na(cbsa)) %>% 
          mutate(league_home = paste(league, home, sep = ",")) %>% 
          pull(league_home) %>%
          unique() %>% 
          map(~rep(.x, 200)) %>% 
          unlist()
      ) %>% 
        separate(league_home, into = c("league", "home"), extra = "merge") %>% 
        mutate(policy = policy_func(home, date)) %>% 
        filter(!(league == "NBA" & date < "2021-10-19")) %>% # nba season started 19th, nhl 12th
        dummy_cols("policy", remove_selected_columns = T) %>% 
        rename_with(~str_sub(.x, 8L, -1L), 4:7) %>% 
        group_by(league, date) %>% 
        summarize(
          "none" = sum(none)/n(),
          "mask" = sum(mask)/n(),
          "vaccine" = sum(vaccine)/n(),
          "both" = sum(both)/n(),
        ) %>% 
        ungroup() %>% 
        pivot_longer(none:both, names_to = "policy", values_to = "values") %>%
        mutate(policy = factor(policy, levels = c("none", "mask", "vaccine", "both"))), 
      by = c("league", "date")
    ) %>% 
    relocate(policy:values, .after = date) %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = values, linetype = policy, color = policy)) +
    scale_x_date(
      date_labels = "%b", 
      date_breaks = "1 month" 
    ) +
    scale_y_continuous(
      name = "% Teams With Policy",
      label = percent_format(scale = 100),
    ) +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(0, 0.5), xlim = c(as.Date(date.start), as.Date(date.end))) +
    scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"), # the name and labels have to match up
                          name = expression(underline("Policy Type:")),
                          labels = c("No Policy", "Mask Mandate", "Vaccine Mandate", "Mask & Vaccine\n      Mandate")
    ) +
    scale_color_manual(values = c("black", "black", "gray60", "gray60"),
                       name = expression(underline("Policy Type:")),
                       labels = c("No Policy", "Mask Mandate", "Vaccine Mandate", "Mask & Vaccine\n      Mandate")
    ) +
    labs(title = paste("COVID-19 Policies Throughout Season"), x = "Month")  
    #
    if (.legend) {
      plot
    }
  else {
    plot + theme(legend.position = "none")
  }
}

ggsave("plots/plot.distribution.png", plot.distribution, width = 9, height = 3.33, units = "in", dpi = 600)

ggsave("plots/plot.cases.png", plot.cases, width = 9, height = 3.33, units = "in", dpi = 600)

ggsave("plots/plot.policies.png", plot.policies, width = 9, height = 4, units = "in", dpi = 600)

# Instrument Plot --------------------------------------------------------------

plot_instrument <- function(.league = "NBA") {
  
dat.instrument <- readRDS("assets/leaflet/2020_fips_shapes.rds") %>%
  select(GEOID, geometry) %>% 
  mutate(geometry = st_transform(geometry, '+proj=longlat +datum=WGS84')) %>% # prevents leaflet warnings
  rename(fips = GEOID) %>% 
  bind_rows(filter(., fips %in% c("36005", "36047", "36061", "36081", "36085")) %>% summarize(fips = "36005", geometry = st_union(geometry))) %>% 
  filter(row_number() != 471) %>% # removes the original bronx fips, with the aggregated one above
  left_join(dat.covid %>% 
              distinct(fips, .keep_all = TRUE) %>% 
              select(cbsa, cbsa_title, county, fips, central_outlying), 
            by = "fips") %>% 
  filter(!is.na(cbsa)) %>%  # removes fips we aren't studying. 702 total, as expected.
  mutate(adjacent = case_when(
    central_outlying == "Neighboring" ~ "Central",
    T ~ "Adjacent"
  )) %>% # code below figures out if the fip is in an NBA only, NHL only, or Both city
  mutate(league = case_when(
    cbsa_title %in% (dat.final %>% filter(league == "NBA") %>% pull(cbsa_title)) &
      cbsa_title %in% (dat.final %>% filter(league == "NHL") %>% pull(cbsa_title)) ~ "Both",
    cbsa_title %in% (dat.final %>% filter(league == "NBA") %>% pull(cbsa_title)) ~ "NBA",
    T ~ "NHL"
  ))
  
dat.stadiums <- data.frame(
  stadium = c("Amalie Arena", "American Airlines Center", "Amway Center", "AT&T Center", "Ball Arena",
              "Barclays Center", "Bridgestone Arena", "Capital One Arena", "Chase Center",
              "crypto.com Arena", "Enterprise Center", "FedExForum", "Fiserv Forum", "FLA Live Arena",
              "Footprint Center", "FTX Arena", "Gainbridge Fieldhouse", "Gila River Arena", "Golden 1 Center", "Honda Center",
              "KeyBank Center", "Little Caesars Arena", "Madison Square Garden", "Moda Center",
              "Nationwide Arena", "Paycom Center", "PNC Arena", "PPG Paints Arena", "Prudential Center",
              "Rocket Mortgage FieldHouse", "SAP Center at San Jose", "Smoothie King Center",
              "Spectrum Center", "State Farm Arena", "Target Center", "TD Garden", "T-Mobile Arena",
              "Toyota Center (Houston)", "UBS Arena", "United Center", "Vivint Arena", "Wells Fargo Center",
              "Xcel Energy Center"),
  latitude = c(27.9429, 32.7904, 28.5397, 29.4268, 39.7488, 40.6823, 36.1588, 38.8982, 37.7679, 
               34.0434, 38.6267, 35.1381, 43.0450, 26.1585, 33.4458, 25.7814, 39.7637, 33.5317, 
               38.5802, 33.8078, 42.8749, 42.3409, 40.7503, 45.5314, 39.9692, 35.4633, 35.8033, 
               40.4396, 40.7335, 41.4965, 37.3328, 29.9492, 35.2250, 33.7571, 44.9794, 42.3662, 
               36.1025, 29.7506, 40.7117, 41.8805, 40.7681, 39.9010, 44.9447),
  longitude <- c(-82.4518, -96.8106, -81.3839, -98.4369, -105.0081, -73.9749, -86.7786, -77.0213, 
                 -122.3875, -118.2669, -90.2028, -90.0509, -87.9178, -80.3257, -112.0714, -80.1878, 
                 -86.1552, -112.2611, -121.4998, -117.8768, -78.8768, -83.0552, -73.9935, -122.6668, 
                 -83.0064, -97.5152, -78.7221, -79.9893, -74.1709, -81.6883, -121.9012, -90.0823, 
                 -80.8395, -84.3963, -93.2762, -71.0619, -115.1785, -95.3621, -73.7259, -87.6744, 
                 -111.9011, -75.1720, -93.1012)
  ) %>% 
  set_colnames(c("stadium", "latitude", "longitude")) %>% # why I have to do this I have no idea
  mutate(league = case_when( # code below figures out if the stadium is in an NBA only, NHL only, or Both city
    stadium %in% (dat.final %>% filter(league == "NBA" & season == "2021-22") %>% pull(stadium)) &
      stadium %in% (dat.final %>% filter(league == "NHL" & season == "2021-22") %>% pull(stadium)) ~ "Both",
    stadium %in% (dat.final %>% filter(league == "NBA" & season == "2021-22") %>% pull(stadium)) ~ "NBA",
    T ~ "NHL"
  ))
# assemble the graph
ggplot() +
  borders("state", col = "gray") + # using maps package
  geom_sf(data = dat.instrument %>% filter(league == "Both" | league == !!.league), aes(fill = adjacent)) +
  geom_point(data = dat.stadiums %>% filter(league == "Both" | league == !!.league), aes(x = longitude, y = latitude), color = "red", size = 1) +
  # geom_sf_text(data = st_as_sf(data.frame(lon = -98.5, lat = 22.5), coords = c("lon", "lat"), crs = 4326),
  #              aes(label = "Red dots ( ) mark US-based NBA and NHL Stadium locations for the 2021-22 season"),
  #              color = "black", size = 4) +
  # geom_sf_text(data = st_as_sf(data.frame(lon = -118, lat = 22.5), coords = c("lon", "lat"), crs = 4326),
  #              aes(label = "\u25cf"),
  #              color = "red", size = 4) +
  scale_fill_manual(values = c("#777777", "#CCCCCC"), labels = c("Central", "Adjacent")) +
  labs(fill = "County Type") +
  theme_void() +
  theme(legend.position = c(0.9, 0.3),
        plot.title = element_text(hjust = 0.1, margin = margin(b = -5))
  )
}
  
plot.instrument.nba <- plot_instrument()
plot.instrument.nhl <- plot_instrument(.league = "NHL")

ggsave("plots/plot.instrument.nba.png", plot.instrument.nba, width = 6, height = 4, units = "in", dpi = 600, bg = "white")
ggsave("plots/plot.instrument.nhl.png", plot.instrument.nhl, width = 6, height = 4, units = "in", dpi = 600, bg = "white")

# Regressions ------------------------------------------------------------------

# Table 1/2 - game level controls, away team fixed effects, month fixed effects
feols(attendance_per ~ cbsa_w_n_deaths + scale(adj_home_odds) + game_number + game_number^2 | 
        game_time_approx + weekday + home + away,
      data = filter(dat.final, league == "NHL" & !is.na(cbsa) & season == "2021-22"),
      vcov = ~home
)
# Table 3/4 - IV, with and without policy
feols(attendance_per ~ policy + scale(adj_home_odds) |
        game_time_approx + weekday + home + away | 
        cbsa_w_n_cases ~ l_neigh_w_n_cases,
      data = filter(dat.final, league == "NHL" & !is.na(cbsa) & season == "2021-22"),
      vcov = ~home
) %>% 
  summary(stage = 1)

ivDiag(data = filter(dat.final, league == "NHL" & !is.na(cbsa) & season == "2021-22") %>% 
         mutate(adj_home_odds = scale(adj_home_odds)) %>% 
         mutate(game_number_sq = game_number^2),
       Y = "attendance_per", D = "cbsa_w_i_n_cases", Z = "l_neigh_w_i_n_cases", 
       controls = c("adj_home_odds", "policy"),
       FE = c("game_time_approx", "weekday", "home", "away"),
       cl = "home"
       )

# Table 5 - Policy, IV, Interactions
feols(attendance_per ~ policy + scale(adj_home_odds) |
        game_time_approx + weekday + home + away | 
        cbsa_w_i_n_cases + i(policy, cbsa_w_i_n_cases, "none") ~ l_neigh_w_i_n_cases + i(policy, l_neigh_w_i_n_cases, "none"),
      data = filter(dat.final, league == "NHL" & !is.na(cbsa) & season == "2021-22"),
      vcov = ~home
) %>% 
  summary(stage = 1)
# Table 6 - All Seasons
feols(attendance_per ~ season + scale(lag_win_percent) + scale(adj_home_odds) + game_number + game_number^2 |
        game_time_approx + weekday + home + away,
      # data = filter(dat.final, league == "NHL") %>% 
      data = filter(dat.final, league == "NHL" & (!is.na(cbsa) | home == "Seattle Kraken")) %>% 
        mutate(season = relevel(factor(season), ref = "2018-19")),
      vcov = ~home
)

# Golden state warriors and Tampa bay lightning were at 100% capacity for EVERY SINGLE GAME in the 5 seasons
# NBA teams with 0 variance in 2021-22 (Heat is very low variance)
c("Boston Celtics", "Golden State Warriors", "Utah Jazz")
# NBA teams with the highest variance (Top 5)
c("Detroit Pistons", "Washington Wizards", "Orlando Magic", "San Antonio Spurs", "Denver Nuggets")
# NHL teams with 0 variance in 2021-22
c("Boston Bruins", "Tampa Bay Lightning", "Washington Capitals")
# NHL teams with the highest variance
c("Buffalo Sabres", "Arizona Coyotes", "New Jersey Devils", "San Jose Sharks", "Los Angeles Kings")





