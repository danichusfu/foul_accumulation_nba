library(tidyverse)
library(lubridate)


col_defaults <-
  cols(
    .default = col_double(),
    HOMEDESCRIPTION = col_character(),
    NEUTRALDESCRIPTION = col_logical(),
    PCTIMESTRING = col_time(format = ""),
    PLAYER1_NAME = col_character(),
    PLAYER1_TEAM_ABBREVIATION = col_character(),
    PLAYER1_TEAM_CITY = col_character(),
    PLAYER1_TEAM_NICKNAME = col_character(),
    PLAYER2_NAME = col_character(),
    PLAYER2_TEAM_ABBREVIATION = col_character(),
    PLAYER2_TEAM_CITY = col_character(),
    PLAYER2_TEAM_NICKNAME = col_character(),
    PLAYER3_NAME = col_character(),
    PLAYER3_TEAM_ABBREVIATION = col_character(),
    PLAYER3_TEAM_CITY = col_character(),
    PLAYER3_TEAM_NICKNAME = col_character(),
    SCORE = col_character(),
    SCOREMARGIN = col_character(),
    VISITORDESCRIPTION = col_character(),
    WCTIMESTRING = col_character(),
    HOME_TEAM = col_character(),
    AWAY_TEAM = col_character(),
    TEAM = col_logical(),
    TYPE = col_logical(),
    SUB_TYPE = col_logical(),
    REBOUND_TEAM = col_logical(),
    FREE_THROW_MADE = col_logical(),
    TEAM_FOUL_COUNT = col_character(),
    FOUL_TYPE = col_character(),
    DOUBLE_PERSONAL_FOUL_COUNT_1 = col_logical(),
    DOUBLE_PERSONAL_FOUL_COUNT_2 = col_logical(),
    COACH_TECHNICAL = col_character(),
    POINTS_SCORED = col_logical(),
    SHOT_MADE = col_logical(),
    SHOT_TYPE = col_character(),
    TURNOVER_TYPE = col_character(),
    POSSESSION_ID = col_logical(),
    PERIOD_START = col_logical(),
    PERIOD_END = col_logical(),
    TIMEOUT_TYPE = col_character(),
    VIOLATION = col_character(),
    EJECTION = col_character(),
    EJECTION_PLAYER_ID = col_character()
  )

count_games <- function(file_name){
  data <- read_csv(file_name, col_types = col_defaults) %>%
    count(GAME_ID)
  # full data starts in 2013-2014
  # one game missing from 2012-2013
}

read_foul_data <- function(file_name = "online-data/events_2012-2013_pbp.csv"){
  data <- read_csv(file_name, col_types = col_defaults)
  
  
  id_name_key <-
    data %>%
    select(PLAYER1_ID, PLAYER1_NAME, PLAYER2_ID, PLAYER2_NAME, PLAYER3_ID, PLAYER3_NAME) %>%
    pivot_longer(starts_with("PLAYER"),
                 names_to = c("player_num", ".value"),
                 names_sep = "_") %>%
    rename(player_id = ID, player_name = NAME) %>%
    select(- player_num) %>%
    filter(player_id != 0) %>%
    distinct()
  
  data <- 
    data %>% 
    rename(EVENT_ORDER = X1) %>%
    rename_at(vars(matches("(HOME|AWAY)_PLAYER_ID_\\d$")), ~ paste(., "ID", sep = "_"))
  
  
  df_fouls <- 
    data %>%
    # Only want foul events
    filter(!is.na(FOULED_BY_PLAYER_ID) | !is.na(DOUBLE_PERSONAL_PLAYER_ID_1)) %>% 
    # Technical fouls do not count towards fouling out
    filter(!(FOUL_TYPE %in% c("T.FOUL", 
                              "Taunting Technical", 
                              "Technical", 
                              "HANGING.TECH.FOUL",
                              "Non-Unsportsmanlike Technical"))) 
  
  df_fouls <- 
    df_fouls %>%
    # select which columns we want
    select(FOULED_BY_PLAYER_ID, DOUBLE_PERSONAL_PLAYER_ID_1, DOUBLE_PERSONAL_PLAYER_ID_2, 
           GAME_ID, EVENT_ORDER, AWAY_SCORE, HOME_SCORE, PCTIMESTRING, PERIOD, EVENTMSGACTIONTYPE,
           EVENTMSGTYPE, HOMEDESCRIPTION, NEUTRALDESCRIPTION, VISITORDESCRIPTION,
           # FOUL COUNT doesn't caputre everything we need since it doesn't get added to on Double Personals
           # Theres no information in Double Personal Foul Counts 1 or 2
           # I will calculate foul count myself later
           # Keeping in foul count so i can check descrepancies
           # they should only occur when there is a double personal foul
           #FOUL_COUNT, FOUL_TYPE, #DOUBLE_PERSONAL_FOUL_COUNT_1, DOUBLE_PERSONAL_FOUL_COUNT_2,
           HOME_PLAYER_ID_1_ID:AWAY_PLAYER_ID_5_PLAY_TIME) %>%
    #  create 2 columns one for type one for the player fouling
    #  this amalgamates the fouled by, double personal 1 and double personal 2
    gather(TYPE, FOULER, FOULED_BY_PLAYER_ID, DOUBLE_PERSONAL_PLAYER_ID_1, DOUBLE_PERSONAL_PLAYER_ID_2) %>%
    pivot_longer(matches("(HOME|AWAY)_PLAYER_ID"),
                 names_to = c("player_num", ".value"),
                 names_sep = "(?<=_PLAYER_ID_\\d)_") %>% 
    rename(PT = PLAY_TIME) %>%
    # only keep the row if the id is for the person doing the fouling
    filter(FOULER == ID) %>%
    # drop the column
    select(-ID) %>%
    arrange(GAME_ID, EVENT_ORDER) %>%
    group_by(GAME_ID, FOULER) %>%
    # Count the number of personal fouls
    mutate(FOUL_TO = 1,
           FOUL_TO = cumsum(FOUL_TO),
           FOUL_FROM = FOUL_TO - 1,
           PT = as.numeric(PT)) %>%
    ungroup()
  
  
  
  # this data frame will give us the amount of playing time each player had in the game
  max_pt <- 
    data %>% 
    # same idea as above
    select(GAME_ID, EVENT_ORDER, HOME_PLAYER_ID_1_ID:AWAY_PLAYER_ID_5_PLAY_TIME) %>%
    pivot_longer(matches("(HOME|AWAY)_PLAYER_ID"),
                 names_to = c("player_num", ".value"),
                 names_sep = "(?<=_PLAYER_ID_\\d)_") %>% 
    rename(PT = PLAY_TIME) %>%
    filter(!is.na(PT), PT != 0) %>%
    arrange(GAME_ID, EVENT_ORDER) %>%
    group_by(GAME_ID, ID) %>%
    # keep only the player's last event 
    filter(EVENT_ORDER == max(EVENT_ORDER)) %>%
    ungroup() %>%
    select(GAME_ID, FOULER = ID, PT) %>%
    distinct()
  
  df_fouls <-
    left_join(max_pt, df_fouls, by = c("GAME_ID", "FOULER")) %>% 
    # join the two dataframes
    # if the player never fouled then they will have NAs everywhere
    # those NAs correspond to the unrealized first foul
    # I fix the columns appropriately
    mutate(TRANSITION_OBSERVED = if_else(is.na(TYPE), 0, 1),
           # PT.x is Total playing tim
           PT.y      = if_else(is.na(PT.y), PT.x, PT.y),
           FOUL_TO   = if_else(is.na(FOUL_TO), 1, FOUL_TO),
           FOUL_FROM = if_else(is.na(FOUL_FROM), 0, FOUL_FROM)) %>%
    rename(PT_TOTAL  = PT.x, PT_TO_HERE = PT.y) %>%
    group_by(GAME_ID, FOULER) %>%
    arrange(PT_TO_HERE, .by_group = T ) %>% 
    # calculate 
    mutate(PT_UNTIL_FOUL = PT_TO_HERE - lag(PT_TO_HERE, default = 0)) %>%
    ungroup()
  
  
  
  df_unobserved <- 
    df_fouls %>% 
    group_by(GAME_ID, FOULER) %>%
    filter(TRANSITION_OBSERVED == 1, FOUL_TO == max(FOUL_TO)) %>%
    mutate(EVENT_ORDER         = NA,
           wall_clock_as_int   = NA,
           visitor_score       = NA,
           home_score          = NA,
           game_clock          = NA,
           period              = NA,
           description         = NA,
           TYPE                = NA,
           PLAYER              = NA,
           PT_UNTIL_FOUL       = PT_TOTAL - PT_TO_HERE,
           PT_TO_HERE          = PT_TOTAL,
           FOUL_TO             = FOUL_TO + 1,
           FOUL_FROM           = FOUL_FROM + 1,
           TRANSITION_OBSERVED = 0
    ) %>%
    filter(FOUL_FROM != 6) %>% 
    ungroup()
  
  df_fouls <- 
    bind_rows(df_fouls, df_unobserved) %>% 
    arrange(GAME_ID, EVENT_ORDER)
  
  # Check if any fouls happen after their total playing time
  df_fouls %>% filter(PT_UNTIL_FOUL > PT_TOTAL)
  # REMOVE IT FOR NOW
  df_fouls <- df_fouls %>% filter(PT_UNTIL_FOUL <= PT_TOTAL)
  
  # Check if fouls happen in negative time
  df_fouls %>% filter(PT_UNTIL_FOUL < 0)
  # REMOVE IT FOR NOW
  df_fouls <- df_fouls %>% filter(PT_UNTIL_FOUL > 0)
  
  
  df_fouls <-
    df_fouls %>%
    left_join(id_name_key, c("FOULER" = "player_id"))
  
  return(df_fouls)
}

