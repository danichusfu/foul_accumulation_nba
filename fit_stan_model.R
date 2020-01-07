##############################
# Libraries
##############################
library(tidyverse)
library(rstan) # observe startup messages

# R stan options
# rstan_options(auto_write = TRUE)
# Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

##############################
# User Functions
##############################
source("read_foul_data.R")
source("functions.R")

##############################
# ggplot defaults
##############################
theme_set(theme_bw() +
            theme(strip.background = element_rect(fill = "white")))


##############################
# Player Positions
##############################
id_position_map <- read_csv("id_position_map.csv")

id_position_map <-
  id_position_map %>%
  mutate(position_old = position,
         position = str_sub(position, 1, 1)) 

##############################
# Read in Foul Data
##############################
# get data from https://eightthirtyfour.com/data
# save in folder online-data

df_14 <- read_foul_data(file_name = "online-data/events_2013-2014_pbp.csv")
df_15 <- read_foul_data(file_name = "online-data/events_2014-2015_pbp.csv")
df_16 <- read_foul_data(file_name = "online-data/events_2015-2016_pbp.csv")
df_17 <- read_foul_data(file_name = "online-data/events_2016-2017_pbp.csv")
df_18 <- read_foul_data(file_name = "online-data/events_2017-2018_pbp.csv")
df_19 <- read_foul_data(file_name = "online-data/events_2018-2019_pbp.csv")



##############################
# Prior players
##############################
prior_players <-
  df_17 %>%
  distinct(FOULER, player_name, GAME_ID, PT_TOTAL) %>%
  group_by(FOULER, player_name) %>%
  summarise(minutes_per_82 = sum(PT_TOTAL)/60/82,
            total_minutes = sum(PT_TOTAL)/60) %>%
  arrange(desc(minutes_per_82)) %>%
  filter(minutes_per_82 > 20)

all_players <-
  df_18 %>%
  distinct(FOULER, player_name, GAME_ID, PT_TOTAL) %>%
  left_join(id_position_map, by = c("FOULER", "player_name")) %>%
  filter(position_old %in% c("G", "F", "C")) %>%
  group_by(FOULER, player_name, position) %>%
  summarise(minutes_per_82 = sum(PT_TOTAL)/60/82,
            total_minutes = sum(PT_TOTAL)/60) %>%
  ungroup() %>%
  arrange(-minutes_per_82)

players_of_interest <-
  all_players %>%
  group_by(position) %>%
  top_n(10, total_minutes) %>%
  select(-total_minutes, - minutes_per_82)
          

# extra_players <- 
#   c(203506,  # Oladipo
#     1627750, # Jamal Murray
#     203482,  # Kelly
#     202684,  # Tristan
#     1628415, # Dillon Brooks
#     203952,  # Wiggins
#     203939   # Powell
#   )
# 
# players_of_interest <- 
#   all_players %>%
#   filter(FOULER %in% extra_players)

##############################
# MLE for prior definition Exploration
##############################
# do it for 18 so we can test on 19

prior_data <- 
  df_17 %>%
  bind_rows(df_16) %>%
  bind_rows(df_15) %>%
  bind_rows(df_14)

mles <- 
  prior_data %>%
  filter(FOULER %in% prior_players$FOULER) %>%
  group_by(FOULER, FOUL_FROM) %>%
  mutate(i = row_number(GAME_ID),
         censored = if_else(TRANSITION_OBSERVED == 1, FALSE, TRUE)) %>%
  select(FOULER, FOUL_FROM, i, X = PT_UNTIL_FOUL, censored) %>%
  ungroup() %>%
  mutate(censored = as.numeric(censored)) %>%
  select(FOULER, FOUL_FROM, X, censored) %>%
  left_join(id_position_map, by = c("FOULER")) %>%
  drop_na(position) %>%
  group_by(FOULER, FOUL_FROM) %>%
  mutate(num_observed = sum(censored == 0)) %>%
  filter(num_observed > 5) %>%
  nest(data = - c(FOULER, FOUL_FROM)) %>%
  mutate(mle_gamma = map(data, possibly(~ fit_gamma_cens(.$censored, .$X), NA))) %>%
  unnest(mle_gamma)


# get_mle_info_for_paper(mles)

##############################
# Prep Data for Stan
##############################
# do it for 18 so we can test on 19

# data <-
#   df_18 %>%
#   bind_rows(df_19)

data <-
  df_17 %>%
  bind_rows(df_18)

data_to_model <- prep_stan_data(data, players_of_interest)

data_to_model <-
  data_to_model %>%
  filter(FOULER %in% players_of_interest$FOULER)


# data_to_model <-
#   data_to_model %>%
#   filter(FOULER %in% c(2544, 201950))
#priors <- get_priors(prior_data, prior_players, foul_from_position)

##############################
# Fit the stan model to each player individually
##############################

map(players_of_interest$FOULER, fit_ind_stan_model)
 
