############################################################################## #
### Data Scraping ESPN
############################################################################## #

# NBA season time ranges
nbaSeasons <- list(seq(as.Date("2015-10-27"), as.Date("2016-04-13"), by = "days"),
                   seq(as.Date("2016-10-25"), as.Date("2017-04-12"), by = "days"),
                   seq(as.Date("2017-10-17"), as.Date("2018-04-11"), by = "days"),
                   seq(as.Date("2018-10-16"), as.Date("2019-04-10"), by = "days"),
                   seq(as.Date("2019-10-22"), as.Date("2020-03-11"), by = "days"),
                   seq(as.Date("2020-12-22"), as.Date("2021-05-16"), by = "days"),
                   seq(as.Date("2021-10-19"), as.Date("2022-04-10"), by = "days"),
                   seq(as.Date("2022-10-18"), as.Date("2023-04-09"), by = "days")
                   )
# NHL season time ranges
nhlSeasons <- list(seq(as.Date("2015-10-07"), as.Date("2016-04-10"), by = "days"),
                   seq(as.Date("2016-10-12"), as.Date("2017-04-09"), by = "days"),
                   seq(as.Date("2017-10-04"), as.Date("2018-04-08"), by = "days"),
                   seq(as.Date("2018-10-03"), as.Date("2019-04-06"), by = "days"),
                   seq(as.Date("2019-10-02"), as.Date("2020-03-11"), by = "days"),
                   seq(as.Date("2021-01-13"), as.Date("2021-05-19"), by = "days"),
                   seq(as.Date("2021-10-12"), as.Date("2022-05-01"), by = "days"),
                   seq(as.Date("2022-10-07"), as.Date("2023-04-14"), by = "days")
                   )
# What to name the data files
game_data_names <- c("raw.nba.2015", "raw.nba.2016", "raw.nba.2017", "raw.nba.2018", 
                     "raw.nba.2021", "raw.nhl.2015", "raw.nhl.2016", "raw.nhl.2017", 
                     "raw.nhl.2018", "raw.nhl.2021")
# Generate the variables on each espn page
gen_row <- function(date, gamePage, new = T) {
  # create variables
  home <- gamePage %>% 
    html_element(".Gamestrip__Team--home .truncate") %>% 
    html_text() %>% 
    paste0("?")
  away <- gamePage %>% 
    html_element(".Gamestrip__Team--away .truncate") %>% 
    html_text() %>% 
    paste0("?")
  home_record <- gamePage %>% 
    html_element(".Gamestrip__Team--home .clr-gray-03") %>% 
    html_text() %>% 
    paste0("?")
  away_record <- gamePage %>% 
    html_element(".Gamestrip__Team--away .clr-gray-03") %>% 
    html_text() %>% 
    paste0("?")
  home_score <- gamePage %>% 
    html_element(".Gamestrip__Team--home .h2") %>% 
    html_text()
  if (new) { # ESPN added and SVG to the div element this selector matches to. I can't find ...
    home_score <- home_score %>% str_remove_all("76ers") %>% 
      str_remove_all("[^0-9]") # a nice way to select the plain text ...
  } # so I just remove the non-numeric characters picked up by the SVG. I had to add the "new" parameter
    home_score <- home_score %>% paste0("?")
  away_score <- gamePage %>% 
    html_element(".Gamestrip__Team--away .h2") %>% 
    html_text()
  if (new) { # ESPN added and SVG to the div element this selector matches to. I can't find ...
    away_score <- away_score %>% str_remove_all("76ers") %>% 
      str_remove_all("[^0-9]") # a nice way to select the plain text ...
  } # so I just remove the non-numeric characters picked up by the SVG. I had to add the "new" parameter
  away_score <- away_score %>% paste0("?")
    paste0("?")
  attendance <- gamePage %>% 
    html_element(".Attendance__Numbers") %>% 
    html_text() %>% 
    paste0("?")
  capacity <- gamePage %>% 
    html_elements(".h10") %>% 
    html_text() %>% 
    paste0("?") 
  stadium <- gamePage %>% 
    html_element("div.GameInfo__Location > div") %>% 
    html_text() %>% 
    paste0("?")
  game_time = gamePage %>% 
    html_element(".GameInfo__Meta > span") %>% 
    html_text() %>% 
    paste0("?")
  # create dataframe
  data.frame(date = date, home = home, away = away, 
             home_record = home_record, away_record = away_record, 
             home_score = home_score, away_score = away_score,
             attendance = attendance, capacity = capacity, stadium = stadium,
             game_time = game_time)
}
# Scrape a single day from ESPN
scrape_day <- function(date, league, timeout = 10, new = T) {
  baseUrl <- paste0("https://www.espn.com/", league, "/scoreboard/_/date/")
  url <- paste0(baseUrl, year(date), month0(date), day0(date))
  mainPage <- url %>% 
    GET(timeout(timeout)) %>% 
    read_html()
  # get game links
  links <- mainPage %>% 
    html_elements(".mr2:nth-child(2)") %>% 
    html_attr("href")
  
  output.data <- NULL
  for (link in links) {
    gamePage <- paste0("https://www.espn.com", link) %>% 
      GET(timeout(timeout)) %>% 
      read_html()
    # generate row
    row <- gen_row(date, gamePage, new = new)
    # assemble dataframe
    output.data <- bind_rows(output.data, row)
  }
  output.data
}
# Scrape over a time range
scrape_season <- function(timeRange, league, new = T, tries = 3) {
  dat.season <- NULL
  for (i in 1:length(timeRange)) {
    dat.day <- NULL
    attempt <- 1
    while(is.null(dat.day) && attempt <= tries) {
      attempt <- attempt + 1
      Sys.sleep(2)
      tryCatch(
        {
          dat.day <- scrape_day(timeRange[i], league, new = new)
        },
        error = function(e) {
          cat("Attempt", attempt, ". Error occurred on date:", as.character(timeRange[[i]]), "\n")
          # You can choose to print the error message as well if desired
          # cat("Error message:", conditionMessage(e), "\n")
        }
      )
    }
    dat.season <- bind_rows(dat.season, dat.day)
  }
  dat.season
}





