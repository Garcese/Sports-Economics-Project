############################################################################## #
### Plots and Tables and Regressions for Sports Econ Project
############################################################################## #  

# Attendance Tables ------------------------------------------------------------

# League-level
dat.final %>% 
  group_by(league) %>% 
  summary_stats(attendance_per) %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3))))
# League-level Home-level standard deviation
dat.final %>% 
  group_by(league, home) %>% 
  summarize(home_sd = psd(attendance_per))%>% 
  ungroup() %>% 
  group_by(league) %>% 
  summary_stats("home_sd") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3))))

# League-Season-level Statistics
dat.final %>% 
  group_by(league, season) %>% 
  summary_stats(attendance_per, yes_median = T) %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3)))) 
# League-Season-level Home-team standard deviation
dat.final %>% 
  group_by(league, season, home) %>% 
  summarize(home_sd = psd(attendance_per))%>% 
  ungroup() %>% 
  group_by(league, season) %>% 
  summary_stats("home_sd") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3))))

# Home-level Statistics
dat.final %>% 
  group_by(home) %>% 
  summary_stats("attendance_per") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3)))) 

# Season-Home-level Statistics
dat.final %>% 
  group_by(league, season, home) %>% 
  summary_stats("attendance_per") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3)))) 

# Covid-19 Tables --------------------------------------------------------------

# Covid-19 Statistics
dat.final %>% 
  filter(season == "2021-22") %>% 
  group_by(league) %>% 
  summary_stats(cbsa_w_n_cases, neigh_w_n_deaths, yes_median = T) 

# Single Team
dat.final %>% 
  filter(season == "2021-22") %>% 
  filter(home == "New York Knicks") %>% 
  summary_stats(cbsa_w_n_cases, neigh_w_n_deaths, yes_median = T) 

# Attendance Plots -------------------------------------------------------------

# Plot of attendance distributions
plot.attendance <- dat.final %>% 
  mutate("Season(s)" = case_when(
    season == "2021-22" ~ "2021",
    T ~ "2015-2018"
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
    title = "Distribution of Attendance per Capcity",
    subtitle = "2015-2018 Seasons VS. 2021 Season",
    x = "Percent Attendance",
    y = "Density",
  ) 

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
  # sahre y-axis title (seems to be the best way, src SE)
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

league_covid_plot <- function(.league, .var, .color) {
  # get the right season length
  if (.league %in% c("NBA", "NHL")) {
    dat <- dat.final %>% filter(league == .league)
  }
  else {
    dat <- dat.final
  }
  # graph
  dat %>% filter(season == "2021-22") %>% 
    ggplot(aes(x = date, y = !!sym(.var), color = !!sym(.color))) + 
    geom_point() + 
    geom_hline(yintercept = 0) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    labs(
      title = paste(.league, .var)
    )
}

league_covid_plot(.league = "BOTH", .var = "cbsa_w_n_cases", .color = "policy")

# Policy Graph -----------------------------------------------------------------

plot.policies <- tibble(
  date = rep(seq(as.Date("2021-10-12"), as.Date("2022-04-29"), by = "days"), 53),
  leage_home = dat.final %>%
    mutate(league_home = paste(league, home, sep = ",")) %>% 
    pull(league_home) %>%
    unique() %>% 
    map(~rep(.x, 200)) %>% 
    unlist()
  ) %>% 
  separate(leage_home, into = c("league", "home"), extra = "merge") %>% 
  mutate(policy = policy_func(home, date)) %>% 
  filter(!(league == "NBA" & date < "2021-10-20")) %>% 
  mutate(policy = case_when(
    league == "NBA" & date < "2021-10-20" ~ "none",
    T ~ policy
  )) %>% 
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
  mutate(policy = factor(policy, levels = c("none", "mask", "vaccine", "both"))) %>% 
  ggplot(aes(x = date, y = values, color = policy)) + 
  geom_line() + 
  facet_wrap(~league) + 
  geom_hline(yintercept = 0) + 
  coord_cartesian(ylim = c(0, 0.5)) + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_color_discrete(
    name = "Policy Type",
    labels = c("No Policy", "Mask Mandate", "Vaccine Mandate", "Mask & Vaccine\n      Mandate")
  ) +
  guides(color = guide_legend(keyheight = c(1, 1, 1, 1.5))) + 
  labs(
    title = "Covid-19 Policy Types in the NBA and NHL",
    subtitle = "Over the 2021-22 Season",
    x = "Date (2021-22)",
    y = "Percent Teams\nWith Policy"
  ) 

# Regressions ------------------------------------------------------------------

# NBA, without IV
feols(attendance_per ~ cbsa_w_i_n_cases + adj_home_odds + game_time_approx + policy + i(policy, cbsa_w_i_n_cases, "none") | # controls
        home + away + weekday + season, # fixed effects
      data = filter(dat.final, league == "NBA") %>% 
        filter(season == "2021-22"),
      vcov = ~home
) %>% 
  etable(tex = T)
# NBA, with IV
feols(attendance_per ~ adj_home_odds + policy | # controls
        home + away + weekday | # fixed effects
        cbsa_w_n_cases ~ l_neigh_w_n_cases, # instrument
      data = filter(dat.final, league == "NHL") %>% 
        filter(season == "2021-22"),
      vcov = ~home
) %>% 
  etable(tex = T)

# Golden state warriors and tampa bay lightning were at 100% capacity for EVERY SINGLE GAME in the 5 seasons
# NBA teams with 0 variance in 2021-22 (Heat is very low variance)
c("Boston Celtics", "Golden State Warriors", "Utah Jazz")
# NBA teams with the highest variance (Top 5)
c("Detroit Pistons", "Washington Wizards", "Orlando Magic", "San Antonio Spurs", "Denver Nuggets")
# NHL teams with 0 variance in 2021-22
c("Boston Bruins", "Tampa Bay Lightning", "Washington Capitals")
# NHL teams with the highest variance
c("Buffalo Sabres", "Arizona Coyotes", "New Jersey Devils", "San Jose Sharks", "Los Angeles Kings")




#