############################################################################## #
### Plots and Tables and Regressions for Sports Econ Project
############################################################################## #  

# Tables ------------------------------------------------------------

# League-level
dat.final %>% 
  filter(!is.na(cbsa)) %>%
  # filter(season == "2021-22") %>%
  # mutate(across(contains("cases"), ~.x*1000)) %>% 
  group_by(league, season) %>% 
  # filter(!is.na(adj_home_odds)) %>% 
  summary_stats(attendance_per) %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3))))
# League-level Home-level standard deviation
dat.final %>% 
  group_by(league, home) %>% 
  summarize(home_sd = psd(attendance_per))%>% 
  ungroup() %>% 
  group_by(league) %>% 
  summary_stats("home_sd") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 4))))
# 
dat.final %>% filter(season == "2020-21") %>% 
  # group_by(league, weekday) %>% summarize(n = n()) %>% 
  # ungroup() %>% 
  group_by(league) %>% 
  summary_stats(attendance_per) %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3)))) %>% 
  # mutate(percent = n/sum(n)) %>% 
  view()
# Covid-19 Statistics
dat.final %>% 
  mutate(across(contains("cases"), ~.x*1000)) %>% 
  filter(season == "2021-22" & !is.na(cbsa)) %>% 
  group_by(league) %>% 
  summary_stats(cbsa_w_n_cases:neigh_w_n_deaths, yes_median = T)

# Attendance Plots -------------------------------------------------------------

# Plot of attendance distributions
plot.attendance <- dat.final %>% 
  filter(season %in% c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2021-22")) %>% 
  mutate("Season(s)" = case_when(
    season == "2021-22" ~ "2021",
    T ~ "2015-19*"
  )) %>% 
  ggplot(aes(x = attendance_per, fill = `Season(s)`)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~league, nrow = 2, scale = "free_y") + 
  scale_x_continuous(
    breaks = seq(30, 120, length =  10),
    label = percent_format(scale = 1)
  ) + 
  geom_hline(yintercept = 0) + 
  labs(
    title = "Distribution of Attendance Rate",
    subtitle = "2015-2019* Seasons VS. 2021 Season",
    x = "Attendance Rate",
    y = "Density",
  )

ggsave("plots/plot.attendance.png", plot.attendance, width = 6, height = 4, units = "in", dpi = 600)

# Covid-19 Plots ---------------------------------------------------------------

# Per CBSA
cbsa_covid_plot <- function(.home, .imputed = T) {
  # get the right season length
  if (.home %in% (dat.final %>% filter(league == "NBA") %>% pull(home))) {
    dat.lower <- "2021-10-18"
    dat.upper <- "2022-04-11"
  }
  else {
    dat.lower <- "2021-10-11"
    dat.upper <- "2022-04-30"
  }
  var1 <- if (.imputed) "cbsa_w_i_n_cases" else "cbsa_w_n_cases"
  var2 <- if (.imputed) "neigh_w_i_n_cases" else "neigh_w_n_cases"
  var3 <- if (.imputed) "cbsa_w_i_n_deaths" else "cbsa_w_n_deaths"
  var4 <- if (.imputed) "neigh_w_i_n_deaths" else "neigh_w_n_deaths"
  # cases plot
  plot.cases <- dat.covid %>%
    unnest(home) %>%
    filter(home == .home) %>%
    filter(date > dat.lower & date < dat.upper) %>%
    select(date, all_of(c(var1, var2))) %>% 
    distinct() %>% 
    set_colnames(c("date", "Weekly New Cases", "Neighboring Weekly New Cases")) %>%
    pivot_longer(`Weekly New Cases`:`Neighboring Weekly New Cases`) %>%
    mutate(name = factor(name, levels = c("Weekly New Cases", "Neighboring Weekly New Cases"))) %>%
    ggplot(aes(x = date, y = value, fill = name)) +
    geom_col(width = 5) +
    facet_wrap(
      ~name,
      nrow = 1,
      scales = "fixed"
    ) +
    geom_hline(yintercept = 0) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_fill_discrete(guide = "none") +
    labs(
      title = paste("New Weekly Covid-19 Cases/Deaths for the", .home),
      y = "Cases/Deaths per Population (%)"
    ) + 
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank()
    )
  # deaths plot
  plot.deaths <- dat.covid %>%
    unnest(home) %>%
    filter(home == .home) %>%
    filter(date > dat.lower & date < dat.upper) %>%
    select(date, all_of(c(var3, var4))) %>% 
    distinct() %>% 
    set_colnames(c("date", "Weekly New Deaths", "Neighboring Weekly New Deaths")) %>%
    pivot_longer(`Weekly New Deaths`:`Neighboring Weekly New Deaths`) %>%
    mutate(name = factor(name, levels = c("Weekly New Deaths", "Neighboring Weekly New Deaths"))) %>%
    ggplot(aes(x = date, y = value, fill = name)) +
    geom_col(width = 5) +
    facet_wrap(
      ~name,
      nrow = 1,
      scales = "fixed"
    ) +
    geom_hline(yintercept = 0) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_fill_discrete(guide = "none") +
    labs(
      title = paste("New Weekly Covid-19 Cases/Deaths for the", .home),
      x = "Date (2021-22)",
      y = "Cases/Deaths per Population (%)"
    ) + 
    theme(
      plot.title = element_blank(),
      axis.title.y = element_blank()
    )
  # share y-axis title (seems to be the best way, src SE)
  plot.y.title <- ggplot() + 
    annotate(geom = "text", x = 1, y = 1, label = "Cases/Deaths per Population (%)", angle = 90) +
    coord_cartesian(clip = "off") +
    theme_void()
  # patchwork together
  (plot.y.title | (plot.cases / plot.deaths)) +
    plot_layout(widths = c(.05, 1))
}

cbsa_covid_plot(.home = "Dallas Stars", .imputed = T)

# Covid-19 Plots Over Season ---------------------------------------------------

plot.nba.policy <- dat.final %>% 
  filter(season == "2021-22" & league == "NBA" & !is.na(cbsa)) %>% 
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
  geom_line(aes(y = (values*8 + 4), linetype = policy, color = policy)) +
  geom_point(aes(y = cbsa_w_i_n_cases)) +
  geom_hline(yintercept = seq(4.8, 8, 0.8), color = "grey", alpha = 0.25) +
  # geom_segment(aes(x = as.Date("2021-09-12"), xend = as.Date("2021-10-13"), y = 4, yend = 4), 
  #              alpha = 0.0098) + 
  # geom_segment(aes(x = as.Date("2022-04-16"), xend = as.Date("2022-05-10"), y = 4, yend = 4), 
  #              alpha = 0.0098) +
  scale_y_continuous(
    name = "New Cases",
    breaks = seq(0, 4, by = 1),
    minor_breaks = seq(0, 3, 1),
    label = percent_format(scale = 1),
    sec.axis = sec_axis(
      ~.*0.5/4 - 0.5,
      name = "Policy Proportion",
      breaks = seq(0.6, 0, by = -0.1),
      label = percent_format(scale = 100),
    )
  ) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(0, 8), xlim = c(as.Date("2021-10-19"), as.Date("2022-04-10"))) +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"), # the name and labels have to match up
                        name = "Policy Type",
                        labels = c("No Policy", "Mask Mandate", "Vaccine Mandate", "Mask & Vaccine\n      Mandate")
  ) +
  scale_color_manual(values = c("black", "black", "gray60", "gray60"),
                     name = "Policy Type",
                     labels = c("No Policy", "Mask Mandate", "Vaccine Mandate", "Mask & Vaccine\n      Mandate")
  ) +
  labs(title = "NBA 2021-22 Regular Season: Cases & Policy Shifts", x = "Month") + 
  theme(axis.title.y.left = element_text(hjust = 0.185),
        axis.title.y.right = element_text(vjust = 2, hjust = 0.12),
  ) +  
  theme(legend.position = c(1.3, 0.8),
        plot.margin = margin(r = 4, l = 0.5, t = 0.5, b = 0.5, unit = "cm"))
#       ###aaaaaaaaaaa
plot.nhl.policy <- dat.final %>% 
  filter(season == "2021-22" & league == "NHL" & !is.na(cbsa)) %>% 
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
      filter(!(league == "NHL" & date < "2021-10-12")) %>% # nba season started 19th, nhl 12th
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
  geom_line(aes(y = (values*8 + 4), linetype = policy, color = policy)) +
  geom_point(aes(y = cbsa_w_i_n_cases)) +
  geom_hline(yintercept = seq(4.8, 8, 0.8), color = "grey", alpha = 0.25) +
  # geom_segment(aes(x = as.Date("2021-09-12"), xend = as.Date("2021-10-05"), y = 4, yend = 4), 
  #              alpha = 0.0098) + 
  # geom_segment(aes(x = as.Date("2022-05-08"), xend = as.Date("2022-05-20"), y = 4, yend = 4), 
  #              alpha = 0.0098) +
  scale_y_continuous(
    name = "New Cases",
    breaks = seq(0, 4, by = 1),
    minor_breaks = seq(0, 3, 1),
    label = percent_format(scale = 1),
    sec.axis = sec_axis(
      ~.*0.5/4 - 0.5,
      name = "Policy Proportion",
      breaks = seq(0.6, 0, by = -0.1),
      label = percent_format(scale = 100),
    )
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(0, 8), xlim = c(as.Date("2021-10-12"), as.Date("2022-05-01"))) +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"), # the name and labels have to match up
                        name = "Policy Type",
                        labels = c("No Policy", "Mask Mandate", "Vaccine Mandate", "Mask & Vaccine\n      Mandate")
  ) +
  scale_color_manual(values = c("black", "black", "gray60", "gray60"),
                     name = "Policy Type",
                     labels = c("No Policy", "Mask Mandate", "Vaccine Mandate", "Mask & Vaccine\n      Mandate")
  ) +
  labs(title = "NHL 2021-22 Regular Season: Cases & Policy Shifts", x = "Month") + 
  theme(axis.title.y.left = element_text(hjust = 0.185),
        axis.title.y.right = element_text(vjust = 2, hjust = 0.12),
  ) +  
  theme(legend.position = c(1.3, 0.8),
        plot.margin = margin(r = 4, l = 0.5, t = 0.5, b = 0.5, unit = "cm"))

league_covid_distribution <- function(.league, .var) {
  if (str_sub(.var, -5L, -1L) == "cases") {
    x_var <- "Cases"
  }
  else {
    x_var <- "Deaths"
  }
  
  dat.final %>% 
    filter(season == "2021-22" & league == .league & !is.na(cbsa)) %>% 
    mutate(cbsa_w_i_n_cases = cbsa_w_i_n_cases) %>% 
    ggplot(aes(x = !!sym(.var), fill = "Quartiles")) + 
    geom_density(fill = "#CCCCCC") + 
    geom_hline(yintercept = 0) + 
    stat_summary( # God this is amazing
      geom = "vline",
      color = "black",
      linetype = "dashed",
      show.legend = T,
      orientation = "y",
      aes(y = 0.000001, xintercept = after_stat(x)),
      fun = function(x) {
        quantile(x, probs = c(0.25, 0.50, 0.75))
      }
    ) +
    scale_fill_manual("Quartiles", values = "Quartiles") + # Will show mean on top of the line
  labs(
      title = paste(.league, "New", x_var, "Distribution"),
      x = paste("New", x_var),
      y = "Density",
    ) +
    scale_x_continuous(
      label = percent_format(scale = 1),
    ) + 
    # scale_y_continuous( ?????? should this be?
    #   label = percent_format(scale = 1),
    # ) + 
    theme(
      legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"), 
          panel.border = element_rect(colour = "black", fill = NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          # strip.text.x = element_blank(),
          # strip.background = element_rect(colour = "black", fill = "white"),
          legend.position = c(0.8, 0.9),
        legend.key.size = unit(1.5, "line")
    )
}

plot.covid <- league_covid_distribution(.league = "NBA", .var = "cbsa_w_i_n_cases") + plot.nba.policy +
  league_covid_distribution(.league = "NHL", .var = "cbsa_w_i_n_cases") + plot.nhl.policy +
  plot_layout(ncol = 2, nrow = 2)

ggsave("plots/plot.covid.png", plot.covid, width = 10.75, height = 8, units = "in", dpi = 600)

# Policy Table -----------------------------------------------------------------

dat.final %>% 
  filter(season == "2021-22") %>% 
  group_by(league, policy) %>% 
  summarize(
    n = n()
  ) %>% 
  ungroup() %>% 
  group_by(league) %>% 
  mutate(nprop = n/sum(n))

# Instrument Graph -------------------------------------------------------------

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

