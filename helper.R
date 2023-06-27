############################################################################## #
### The Giancarlo Arcese personal package, trademark pending
############################################################################## #

theme_set(theme_bw())
options(scipen=999)

# General ----------------------------------------------------------------------

# assign to environment and update namespace
assign_to_namespace <- function(.name, .obj, .env) {
  envName = as.character(match.call()$.env)
  assign(.name, .obj, envir = .env)
  if (envName %in% search()) {
    detach(envName, character.only = T)
    attach(.env, name = envName)
  }
  else {
    attach(.env, name = envName)
  }
}
# attaches personal package
my_attach <- function(.env) {
  envName = as.character(match.call()$.env)
  if (envName %in% search()) {
    detach(envName, character.only = T)
    attach(.env, name = envName)
  }
  else {
    attach(.env, name = envName)
  }
  rm(list = envName, envir = .GlobalEnv)
}
# attaches any other .R file in one line instead of two
attach_source <- function(.src, .envName) {
  .env <- new.env()
  source(.src, local = .env)
  if (.envName %in% search()) {
    detach(.envName, character.only = T)
    attach(.env, name = .envName)
  }
  else {
    attach(.env, name = .envName)
  }
}
# returns vector of ordered weights of consecutive entries satisfy a condition
cond_weights <- function(.vec, .fun) {
  ids <- which(.fun(.vec))
  weights <- NULL
  i <- 0
  while (i < length(ids)) {
    if (i == length(ids) - 1) {
      weights <- c(weights, 1)
      i <- i + 1
    }
    else {
      j <- 1
      while (ids[i+j] == ids[i+j+1] - 1 & j < length(ids)) {
        j <- j + 1
      }
      weights <- c(weights, j)
      i <- i + j
    }
  }
  weights
}
# return day from date object as character with leading 0s.
day0 <- function(.date) {
  output <- day(.date) %>% as.character()
  if (str_length(output) == 1) paste0("0", output)
  else output
}
# fill numeric vector NA values with smooth averages
fill_average <- function(.vec) {
  if (all(is.na(.vec))) { # county 06003, is literally all 0s. so if you get c(NA, NA, ...) just return a vec of 0s.
    rep(NA, length = length(.vec))
  }
  else {
    filledVec <- .vec
    minNumId <- which(!is.na(filledVec)) %>% min()
    maxNumId <- which(!is.na(filledVec)) %>% max()
    for (i in 1:length(filledVec)) {
      if (is.na(filledVec[i]) & i > minNumId & i < maxNumId) {
        lagDist <- length(head(filledVec, i-1)) - max(which(!is.na(head(filledVec, i-1)))) + 1
        lagValue <- head(filledVec, i-1)[max(which(!is.na(head(filledVec, i-1))))]
        leadDist <- min(which(!is.na(tail(filledVec, -i))))
        leadValue <- tail(filledVec, -i)[leadDist]
        filledVec[i] <- seq(lagValue, leadValue, length = (lagDist + leadDist + 1))[lagDist + 1]
      }
    }
    replace(filledVec, is.na(filledVec), 0) # replace any existing NAs on the boundaries with 0.
  }
}
# distributes first non-NA value to previous NA values
my_impute <- function(.vec) {
  if (all(is.na(.vec))) { # example: c(10, 0, 0, 40) -> c(10, 13.3, 13.3, 13.3)
    rep(NA, length = length(.vec))
  }
  else {
    minNumId <- which(!is.na(.vec)) %>% min()
    maxNumId <- which(!is.na(.vec)) %>% max()
    filledVec <- .vec
    for (i in 1:length(.vec)) {
      lagDist <- length(head(.vec, i-1)) - max(which(!is.na(head(.vec, i-1)))) + 1
      lagValue <- head(.vec, i-1)[max(which(!is.na(head(.vec, i-1))))]
      leadDist <- min(which(!is.na(tail(.vec, -i))))
      leadValue <- tail(.vec, -i)[leadDist]
      if (i < minNumId) { # special cases for first section of NAs
        filledVec[i] <- leadValue/(minNumId)
      }
      else if (i == minNumId) { # ...
        filledVec[i] <- .vec[i]/minNumId
      }
      else if (is.na(.vec[i]) & i > minNumId & i < maxNumId) { # regular calc
        filledVec[i] <- leadValue/(leadDist + lagDist)
      }
      else if (i > 1 && is.na(.vec[i-1]) & !is.na(.vec[i])) { # for the value that is being distributed
        filledVec[i] <- .vec[i]/lagDist
      }
    }
    replace(filledVec, is.na(filledVec), NA) # replace any existing NAs on the boundaries with NA.
  }
}
# mass load all csvs in a directory
mass_load <- function(.src, .pos, .bind = F) {
  files <- list.files(path = .src, pattern = "*.csv")
  if (.bind) {
    dat.binded <- NULL
    for (file in files) {
      dat.binded <- bind_rows(dat.binded, read_csv(paste0(.src, file)))
    }
    dat.binded
  }
  else {
    for (file in files) {
      assign(str_sub(file, 1L, -5L), read_csv(paste0(.src, file)), pos = .pos)
    }
  }
}
# mass write data as csv
mass_write <- function(.path, .data_names) {
  for (.dat in .data_names) {
    if (exists(.dat )) {
      write_csv(get(.dat ), paste0(.path, .dat, ".csv"))
      rm(list = .dat, envir = .GlobalEnv)
    }
  }
}
# return month from date object as character with leading 0s.
month0 <- function(.date) {
  output <- month(.date) %>% as.character()
  if (str_length(output) == 1) paste0("0", output)
  else output
}
# compress a vector
my_compress <- function(.vec, .fun = function(x) T) {
  newVec <- as.list(.vec)
  for (i in length(newVec):2) {
    condition <- if (T) identical(newVec[i], newVec[i-1]) & .fun(newVec[i-1])
    if (condition) {
      newVec[i] <- "lmao broski"
    }
  } 
  newVec[which(newVec != "lmao broski")] %>%
    as_vector()
}
# return object name as character
obj_name <- function(.obj) {
  deparse(match.call()$.obj)
}
# Population Standard Deviation
psd <- function(.vec) {
  sqrt(mean((.vec - mean(.vec))^2))
} 
# Population Variance
pvar <- function(.vec) {
  sum((.vec - mean(.vec))^2)/length(.vec)
}
# contains operator
`%includes%` <- function(.vec, .string) {
  str_detect(.vec, .string)
}
# negates "includes" operator
`%excludes%` <- Negate(`%includes%`) 
# negate %in% operator
`%notin%` <- Negate(`%in%`) 
# expands indices that satisfy a condition provided weights
weighted_expand <- function(.vec, .weights, .indices) {
  expandedVec <- .vec
  indexAdjust <- 0
  numNew <- 1
  for (i in .indices) {
    expandedVec <- append(expandedVec, rep(expandedVec[i + indexAdjust], .weights[numNew]-1),
                          after = i + indexAdjust)
    indexAdjust <- sum((.weights - 1)[1:numNew])
    numNew <- numNew + 1
  }
  expandedVec
}
# Get summary stats of var
summary_stats <- function(.data, ..., yes_median = F) {
  out <- NULL
  expr <- expr(c(...))
  pos <- eval_select(expr, data = .data)
  count <- 1
  for (var in names(pos)) {
    if (count == 1){
      out <- out %>% 
        bind_cols(
          .data %>% 
            summarize(
              mean = mean(.data[[var]]),
              median = median(.data[[var]]),
              min = min(.data[[var]]),
              max = max(.data[[var]]),
              psd = psd(.data[[var]]),
              pvar = pvar(.data[[var]])
            ) %>% 
            rename_with(~paste(var, .x, sep = "_"), contains(c("mean", "median", "min", "max", "psd", "pvar")))
        )
      count <- count + 1
    }
    else {
      out <- out %>% 
        bind_cols(
          .data %>% 
            summarize(
              mean = mean(.data[[var]]),
              median = median(.data[[var]]),
              min = min(.data[[var]]),
              max = max(.data[[var]]),
              psd = psd(.data[[var]]),
              pvar = pvar(.data[[var]])
            ) %>% 
            select(where(~is.numeric(.x))) %>% # THIS IS TO DROP GROUPING VARIABLE, ASSUMES IT IS A CHARACTER TYPE
            rename_with(~paste(var, .x, sep = "_"), contains(c("mean", "median", "min", "max", "psd", "pvar")))
        )
    }
  }
  if (!yes_median) {
    out <- out %>% 
      select(!contains("median"))
  }
  out 
}
# From Stack exchange
guide_squarekey <- function(...) {
  # Constructor just prepends a different class
  x <- guide_legend(...)
  class(x) <- c("squarekey", class(x))
  x
}
# From Stack exchange
guide_gengrob.squarekey <- function(guide, theme) {
  legend <- NextMethod()
  is_key <- startsWith(legend$layout$name, "key-")
  is_key_bg <- is_key & endsWith(legend$layout$name, "-bg")
  is_key <- is_key & !endsWith(legend$layout$name, "-bg")
  
  key_col <- unique(legend$layout$l[is_key])
  keywidth <- convertUnit(legend$widths[2], "mm", valueOnly = TRUE)
  
  legend$grobs[is_key] <- lapply(legend$grobs[is_key], function(key) {
    key$height <- unit(keywidth - 0.5, "mm")
    key
  })
  legend$grobs[is_key_bg] <- lapply(legend$grobs[is_key_bg], function(bg) {
    bg$height <- unit(keywidth, "mm")
    bg
  })
  legend
}

# Definitely Specific to Sports Econ Project -----------------------------------

# assigns home team to cbsa
assign_home <- function(.cbsa) {
  case_when(
    .cbsa == "12060" ~ "Atlanta Hawks",
    .cbsa == "14460" ~ "Boston Bruins,Boston Celtics",
    .cbsa == "15380" ~ "Buffalo Sabres",
    .cbsa == "16740" ~ "Charlotte Hornets",
    .cbsa == "16980" ~ "Chicago Blackhawks,Chicago Bulls",
    .cbsa == "17460" ~ "Cleveland Cavaliers",
    .cbsa == "18140" ~ "Columbus Blue Jackets",
    .cbsa == "19100" ~ "Dallas Mavericks,Dallas Stars",
    .cbsa == "19740" ~ "Colorado Avalanche,Denver Nuggets",
    .cbsa == "19820" ~ "Detroit Pistons,Detroit Red Wings",
    .cbsa == "26420" ~ "Houston Rockets",
    .cbsa == "26900" ~ "Indiana Pacers",
    .cbsa == "29820" ~ "Vegas Golden Knights",
    .cbsa == "31080" ~ "Anaheim Ducks,Los Angeles Clippers,Los Angeles Kings,Los Angeles Lakers",
    .cbsa == "32820" ~ "Memphis Grizzlies",
    .cbsa == "33100" ~ "Florida Panthers,Miami Heat",
    .cbsa == "33340" ~ "Milwaukee Bucks",
    .cbsa == "33460" ~ "Minnesota Timberwolves,Minnesota Wild",
    .cbsa == "34980" ~ "Nashville Predators",
    .cbsa == "35380" ~ "New Orleans Pelicans", 
    .cbsa == "35620" ~ "Brooklyn Nets,New Jersey Devils,New York Islanders,New York Knicks,New York Rangers",
    .cbsa == "36420" ~ "Oklahoma City Thunder",
    .cbsa == "36740" ~ "Orlando Magic",
    .cbsa == "37980" ~ "Philadelphia 76ers,Philadelphia Flyers",
    .cbsa == "38060" ~ "Arizona Coyotes,Phoenix Suns",
    .cbsa == "38300" ~ "Pittsburgh Penguins",
    .cbsa == "38900" ~ "Portland Trail Blazers",
    .cbsa == "39580" ~ "Carolina Hurricanes",
    .cbsa == "40900" ~ "Sacramento Kings",
    .cbsa == "41180" ~ "St. Louis Blues", 
    .cbsa == "41620" ~ "Utah Jazz",
    .cbsa == "41700" ~ "San Antonio Spurs",
    .cbsa == "41860" ~ "Golden State Warriors",
    .cbsa == "41940" ~ "San Jose Sharks",
    .cbsa == "45300" ~ "Tampa Bay Lightning",
    .cbsa == "47900" ~ "Washington Capitals,Washington Wizards"
  )
}
# returns time zone based on cbsa code
cbsa_to_time <- function(.cbsa) {
  case_when(
    .cbsa == "12060" ~ "EST",
    .cbsa == "14460" ~ "EST",
    .cbsa == "15380" ~ "EST",
    .cbsa == "16740" ~ "EST",
    .cbsa == "16980" ~ "CST",
    .cbsa == "17460" ~ "EST",
    .cbsa == "18140" ~ "EST",
    .cbsa == "19100" ~ "CST",
    .cbsa == "19740" ~ "MST",
    .cbsa == "19820" ~ "EST",
    .cbsa == "26420" ~ "CST",
    .cbsa == "26900" ~ "EST",
    .cbsa == "29820" ~ "PST",
    .cbsa == "31080" ~ "PST",
    .cbsa == "32820" ~ "CST",
    .cbsa == "33100" ~ "EST",
    .cbsa == "33340" ~ "CST",
    .cbsa == "33460" ~ "CST",
    .cbsa == "34980" ~ "CST",
    .cbsa == "35380" ~ "CST", 
    .cbsa == "35620" ~ "EST",
    .cbsa == "36420" ~ "CST",
    .cbsa == "36740" ~ "EST",
    .cbsa == "37980" ~ "EST",
    .cbsa == "38060" ~ "MST",
    .cbsa == "38300" ~ "EST",
    .cbsa == "38900" ~ "PST",
    .cbsa == "39580" ~ "EST",
    .cbsa == "40900" ~ "PST",
    .cbsa == "41180" ~ "CST", 
    .cbsa == "41620" ~ "MST",
    .cbsa == "41700" ~ "CST",
    .cbsa == "41860" ~ "PST",
    .cbsa == "41940" ~ "PST",
    .cbsa == "45300" ~ "EST",
    .cbsa == "47900" ~ "EST"
  )
}
# policy data function
policy_func <- function(.home, .date) {
  case_when(
    .home == "Anaheim Ducks" & .date > "2022-04-01" ~ "none",
    .home == "Anaheim Ducks" & .date > "2022-01-15" ~ "vaccine",
    .home == "Anaheim Ducks" & .date > "2021-12-15" ~ "both",
    .home == "Anaheim Ducks" & .date > "2021-10-01" ~ "vaccine", 
    .home == "Arizona Coyotes" & .date > "2021-10-01" ~ "none",
    .home == "Atlanta Hawks" & .date > "2021-10-01" ~ "none", 
    .home == "Boston Bruins" & .date >= "2022-03-05" ~ "none",  
    .home == "Boston Bruins" & .date >= "2022-02-21" ~ "mask",
    .home == "Boston Bruins" & .date >= "2021-10-01" ~ "both", 
    .home == "Boston Celtics" & .date >= "2022-03-05" ~ "none",  
    .home == "Boston Celtics" & .date >= "2022-02-21" ~ "mask", 
    .home == "Boston Celtics" & .date >= "2021-10-01" ~ "both", 
    .home == "Brooklyn Nets" & .date >= "2022-03-07" ~ "none", 
    .home == "Brooklyn Nets" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Buffalo Sabres" & .date > "2022-02-28" ~ "none", 
    .home == "Buffalo Sabres" & .date > "2021-10-01" ~ "vaccine", 
    .home == "Carolina Hurricanes" & .date > "2022-02-28" ~ "none", 
    .home == "Carolina Hurricanes" & .date > "2021-10-01" ~ "mask", 
    .home == "Columbus Blue Jackets" & .date > "2021-10-01" ~ "none", 
    .home == "Chicago Bulls" & .date >= "2022-03-22" ~ "none",
    .home == "Chicago Bulls" & .date >= "2022-03-04" ~ "vaccine", 
    .home == "Chicago Bulls" & .date >= "2021-10-01" ~ "both",
    .home == "Charlotte Hornets" & .date >= "2022-02-28" ~ "none",
    .home == "Charlotte Hornets" & .date >= "2021-10-01" ~ "mask",
    .home == "Chicago Blackhawks" & .date >= "2022-03-22" ~ "none",
    .home == "Chicago Blackhawks" & .date >= "2022-03-03" ~ "vaccine", 
    .home == "Chicago Blackhawks" & .date >= "2021-10-01" ~ "both",
    .home == "Chicago Bulls" & .date >= "2022-03-22" ~ "none",
    .home == "Chicago Bulls" & .date >= "2022-03-04" ~ "vaccine", 
    .home == "Chicago Bulls" & .date >= "2021-10-01" ~ "both",
    .home == "Cleveland Cavaliers" & .date >= "2022-02-01" ~ "none",
    .home == "Cleveland Cavaliers" & .date >= "2021-12-31" ~ "mask",
    .home == "Cleveland Cavaliers" & .date >= "2021-10-01" ~ "none",
    .home == "Colorado Avalanche" & .date >= "2022-03-12" ~ "none",
    .home == "Colorado Avalanche" & .date >= "2021-10-01" ~ "both",
    .home == "Dallas Mavericks" & .date >= "2022-03-03" ~ "none",
    .home == "Dallas Mavericks" & .date >= "2021-11-15" ~ "mask",
    .home == "Dallas Mavericks" & .date >= "2021-10-01" ~ "both",
    .home == "Dallas Stars" & .date >= "2022-03-03" ~ "none", # bad data potentially
    .home == "Dallas Stars" & .date >= "2021-10-01" ~ "mask",
    .home == "Denver Nuggets" & .date >= "2022-03-12" ~ "none",
    .home == "Denver Nuggets" & .date >= "2021-10-01" ~ "both",
    .home == "Detroit Pistons" & .date >= "2021-10-01" ~ "none",
    .home == "Detroit Red Wings" & .date >= "2021-10-01" ~ "none",
    .home == "Florida Panthers" & .date >= "2021-10-01" ~ "none",
    .home == "Golden State Warriors" & .date >= "2022-04-01" ~ "none",
    .home == "Golden State Warriors" & .date >= "2022-02-16" ~ "vaccine",
    .home == "Golden State Warriors" & .date >= "2021-10-01" ~ "both",
    .home == "Houston Rockets" & .date >= "2021-10-01" ~ "none",
    .home == "Indiana Pacers" & .date >= "2021-10-01" ~ "none",
    .home == "Los Angeles Clippers" & .date >= "2022-04-01" ~ "none",
    .home == "Los Angeles Clippers" & .date >= "2022-02-25" ~ "vaccine",
    .home == "Los Angeles Clippers" & .date >= "2021-10-01" ~ "both",
    .home == "Los Angeles Kings" & .date >= "2022-04-01" ~ "none",
    .home == "Los Angeles Kings" & .date >= "2022-02-25" ~ "vaccine",
    .home == "Los Angeles Kings" & .date >= "2021-10-01" ~ "both",
    .home == "Los Angeles Lakers" & .date >= "2022-04-01" ~ "none",
    .home == "Los Angeles Lakers" & .date >= "2022-02-25" ~ "vaccine",
    .home == "Los Angeles Lakers" & .date >= "2021-10-01" ~ "both",
    .home == "Minnesota Wild" & .date >= "2022-02-28" ~ "none",
    .home == "Minnesota Wild" & .date >= "2022-02-10" ~ "mask",
    .home == "Minnesota Wild" & .date >= "2022-01-06" ~ "both",
    .home == "Minnesota Wild" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Nashville Predators" & .date >= "2021-11-13" ~ "none",
    .home == "Nashville Predators" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Memphis Grizzlies" & .date >= "2021-11-24" ~ "none",
    .home == "Memphis Grizzlies" & .date >= "2021-10-01" ~ "both",
    .home == "New Jersey Devils" & .date >= "2022-03-02" ~ "none",
    .home == "New Jersey Devils" & .date >= "2022-01-10" ~ "both",
    .home == "New Jersey Devils" & .date >= "2022-12-22" ~ "mask",
    .home == "New Jersey Devils" & .date >= "2021-10-01" ~ "none",
    .home == "New York Islanders" & .date >= "2022-02-17" ~ "none",
    .home == "New York Islanders" & .date >= "2021-10-01" ~ "both",
    .home == "New York Rangers" & .date >= "2022-03-07" ~ "none",
    .home == "New York Rangers" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Pittsburgh Penguins" & .date >= "2021-10-01" ~ "none",
    .home == "San Jose Sharks" & .date >= "2021-03-28" ~ "none",
    .home == "San Jose Sharks" & .date >= "2022-03-02" ~ "vaccine",
    .home == "San Jose Sharks" & .date >= "2021-10-01" ~ "both",
    .home == "St. Louis Blues" & .date >= "2022-03-06" ~ "none",
    .home == "St. Louis Blues" & .date >= "2021-10-01" ~ "both",
    .home == "Tampa Bay Lightning" & .date >= "2021-10-01" ~ "none",
    .home == "Vegas Golden Knights" & .date >= "2022-02-10" ~ "none",
    .home == "Vegas Golden Knights" & .date >= "2021-10-01" ~ "mask",
    .home == "Washington Capitals" & .date >= "2022-03-01" ~ "none",
    .home == "Washington Capitals" & .date >= "2022-02-15" ~ "mask",
    .home == "Washington Capitals" & .date >= "2022-01-15" ~ "both",
    .home == "Washington Capitals" & .date >= "2021-10-01" ~ "mask",
    .home == "Miami Heat" & .date >= "2022-02-26" ~ "none",
    .home == "Miami Heat" & .date >= "2021-10-1" ~ "mask",
    .home == "Milwaukee Bucks" & .date >= "2022-03-02" ~ "none",
    .home == "Milwaukee Bucks" & .date >= "2022-01-01" ~ "mask",
    .home == "Milwaukee Bucks" & .date >= "2021-10-1" ~ "none",
    .home == "Minnesota Timberwolves" & .date >= "2022-02-28" ~ "none",
    .home == "Minnesota Timberwolves" & .date >= "2022-02-24" ~ "vaccine",
    .home == "Minnesota Timberwolves" & .date >= "2022-01-26" ~ "both",
    .home == "Minnesota Timberwolves" & .date >= "2022-01-16" ~ "mask",
    .home == "Minnesota Timberwolves" & .date >= "2021-10-01" ~ "none",
    .home == "New Orleans Pelicans" & .date >= "2022-03-22" ~ "none", 
    .home == "New Orleans Pelicans" & .date >= "2022-03-03" ~ "vaccine",
    .home == "New Orleans Pelicans" & .date >= "2021-10-01" ~ "both",
    .home == "New York Knicks" & .date >= "2022-03-07" ~ "none",
    .home == "New York Knicks" & .date >= "2021-10-01" ~ "both",
    .home == "Oklahoma City Thunder" & .date >= "2022-01-15" ~ "none",
    .home == "Oklahoma City Thunder" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Orlando Magic" & .date >= "2021-10-01" ~ "none",
    .home == "Philadelphia 76ers" & .date >= "2022-03-02" ~ "none",
    .home == "Philadelphia 76ers" & .date >= "2022-02-06" ~ "mask",
    .home == "Philadelphia 76ers" & .date >= "2022-01-03" ~ "both",
    .home == "Philadelphia 76ers" & .date >= "2021-10-01" ~ "mask",
    .home == "Philadelphia Flyers" & .date >= "2022-03-02" ~ "none",
    .home == "Philadelphia Flyers" & .date >= "2022-02-06" ~ "mask",
    .home == "Philadelphia Flyers" & .date >= "2022-01-03" ~ "both",
    .home == "Philadelphia Flyers" & .date >= "2021-10-01" ~ "mask",
    .home == "Phoenix Suns" & .date >= "2021-10-01" ~ "none",
    .home == "Portland Trail Blazers" & .date >= "2022-03-12" ~ "none",
    .home == "Portland Trail Blazers" & .date >= "2021-10-01" ~ "both",
    .home == "Sacramento Kings" & .date >= "2022-04-01" ~ "none",
    .home == "Sacramento Kings" & .date >= "2022-03-22" ~ "vaccine",
    .home == "Sacramento Kings" & .date >= "2021-10-01" ~ "both",
    .home == "San Antonio Spurs" & .date >= "2021-10-01" ~ "none",
    .home == "Utah Jazz" & .date >= "2022-02-25" ~ "none",
    .home == "Utah Jazz" & .date >= "2022-01-21" ~ "vaccine",
    .home == "Utah Jazz" & .date >= "2022-01-08" ~ "both",
    .home == "Utah Jazz" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Washington Wizards" & .date >= "2022-03-01" ~ "none",
    .home == "Washington Wizards" & .date >= "2022-02-15" ~ "mask",
    .home == "Washington Wizards" & .date >= "2022-01-15" ~ "both",
    .home == "Washington Wizards" & .date >= "2021-10-01" ~ "mask",
    T ~ NA_character_
  )
}





