# references
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4194196/#R10
# https://onlinelibrary-wiley-com.proxy.lib.sfu.ca/doi/epdf/10.1002/bimj.200610301
# https://square.github.io/pysurvival/metrics/brier_score.html


##############################
# Libraries
##############################
library(tidyverse)
library(survival)

##############################
# User Functions
##############################
source("read_foul_data.R")
source("functions.R")



closest_match <- function(data, time){
  map_dbl(time, ~ filter(data, abs(. - y_new) == min(abs(. - y_new))) %>%
            summarise(sp = mean(survive_prob)) %>%
            pull(sp) )
}

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
  mutate(position = str_sub(position, 1, 1)) 

# fits <-
#   tibble(files = list.files("player_specific_17_18/", pattern = "fit_foul_from_position_constrained_", full.names = T)) %>%
#   mutate(data = map(files, read_rds))
# 
# 
# posterior_results <-
#   fits %>%
#   mutate(FOULER = str_extract(files, "_\\d{3,}"),
#          FOULER = parse_number(FOULER),
#          data_to_model = map(FOULER, ~ filter(data_to_model, FOULER == .)),
#          tidy_fit = map2(data_to_model, data, ~ tidy_stan(.x, players_of_interest, .y))) %>%
#   select(tidy_fit) %>%
#   unnest(tidy_fit)

#write_rds(posterior_results, "posterior_results_17_18.rds")
posterior_results <- read_rds("posterior_results_17_18.rds")
#posterior_results <- read_rds("posterior_results_18_19.rds")


df_19 <- read_foul_data(file_name = "online-data/events_2018-2019_pbp.csv")


estimated_survive_prob <- 
  posterior_results %>% 
  mutate(y_new = map(y_new, ~ mutate(., 
                                     rank         = min_rank(y_new),
                                     survive_prob = 1 - rank/max(rank)))) %>%
  select(FOUL_FROM, FOULER, player_name, position, y_new)

data_censoring_model <- 
  prep_stan_data(df_19, players_of_interest) %>%
  select(- i) %>%
  arrange(X) %>%
  mutate(FOUL_FROM = as.factor(FOUL_FROM))

times <- 
  seq(0, (48 * 60),  by = 60)

foul_time <- 
  tibble(X = times, FOUL_FROM = list(0:5)) %>%
  unnest(FOUL_FROM) %>%
  mutate(FOUL_FROM = as.factor(FOUL_FROM))

fit <- prodlim::prodlim(prodlim::Hist(time = X, event = 1 - censored) ~ FOUL_FROM,
                        data = data_censoring_model,
                        reverse = TRUE)

IPCW.times <- predict(fit, newdata = tibble(FOUL_FROM = 0:5), times = times, level.chaos = 1, mode = "list", type = "surv")

IPCW.subjectTimes <- prodlim::predictSurvIndividual(fit, lag = 0)


data_censoring_model <-
  data_censoring_model %>%
  select(FOULER, FOUL_FROM, censored, X) %>%
  mutate(ipcw_times_i = IPCW.subjectTimes)


brier_scores <-
  foul_time %>%
  arrange(FOUL_FROM) %>%
  mutate(ipcw_times = unlist(IPCW.times)) %>% 
  drop_na(ipcw_times) %>%
  left_join(estimated_survive_prob %>% mutate(FOUL_FROM = as.factor(FOUL_FROM)), by = c("FOUL_FROM"))  %>%
  mutate(est_surv_prob = map2_dbl(y_new, X, closest_match)) %>%
  left_join(data_censoring_model, by = c("FOUL_FROM", "FOULER")) %>%
  mutate(ind_brier_score = (1 - censored) * (X.y <= X.x) * (0 - est_surv_prob)^2/ipcw_times_i + 
                                            (X.x <  X.y) * (1 - est_surv_prob)^2/ipcw_times) %>%
  drop_na(ind_brier_score)

brier_scores <-
  foul_time %>%
  arrange(FOUL_FROM) %>%
  mutate(ipcw_times = unlist(IPCW.times)) %>% 
  drop_na(ipcw_times) %>%
  left_join(estimated_survive_prob %>% mutate(FOUL_FROM = as.factor(FOUL_FROM)), by = c("FOUL_FROM"))  %>%
  mutate(est_surv_prob = map2_dbl(y_new, X, closest_match)) %>%
  left_join(data_censoring_model, by = c("FOUL_FROM", "FOULER")) %>%
  mutate(ind_brier_score = (1 - censored) * (X.y <= X.x) * (0 - est_surv_prob)^2/ipcw_times_i + 
           (X.x <  X.y) * (1 - est_surv_prob)^2/ipcw_times) %>%
  drop_na(ind_brier_score)
  


brier_scores %>%
  group_by(X.x, FOUL_FROM) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
ggplot(aes(x = X.x/60 , y = brier_score)) +
  geom_line() +
  facet_wrap(~ FOUL_FROM)

brier_scores %>% 
  sample_n(10000) %>%
  mutate(ind_error = (1 - censored) * (X.y <= X.x) * (0 - est_surv_prob)/sqrt(ipcw_times_i) + 
           (X.x <  X.y) * (1 - est_surv_prob)/sqrt(ipcw_times)) %>%
  drop_na(ind_brier_score) %>%
  ggplot(aes(x = X.x, y = ind_error)) +
  geom_point()


mles <- 
  data %>%
  filter(FOULER %in% players_of_interest$FOULER) %>%
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

mles <-
  mles %>% 
  select(FOULER, FOUL_FROM, shape, rate) %>%
  ungroup()

mles <- 
  mles %>%
  complete(FOULER, FOUL_FROM) %>%
  fill(shape, rate)

brier_scores_mle <- 
  foul_time %>%
  arrange(FOUL_FROM) %>%
  mutate(ipcw_times = unlist(IPCW.times)) %>% 
  drop_na(ipcw_times) %>%
  left_join(mles %>% mutate(FOUL_FROM = as.factor(FOUL_FROM)), by = c("FOUL_FROM")) %>%
  mutate(est_surv_prob = 1 - pmap_dbl(list(X, shape, rate), ~ pgamma(..1, ..2, ..3))) %>%
  left_join(data_censoring_model, by = c("FOUL_FROM", "FOULER")) %>%
  mutate(ind_brier_score = (1 - censored) * (X.y <= X.x) * (0 - est_surv_prob)^2/ipcw_times_i + 
           (X.x <  X.y) * (1 - est_surv_prob)^2/ipcw_times) %>%
  drop_na(ind_brier_score)



brier_scores_mle %>% 
  group_by(X.x, FOUL_FROM) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  ggplot(aes(x = X.x/60 , y = brier_score)) +
  geom_line() +
  facet_wrap(~ FOUL_FROM)

brier_scores_half <- 
  foul_time %>%
  arrange(FOUL_FROM) %>%
  mutate(ipcw_times = unlist(IPCW.times)) %>% 
  drop_na(ipcw_times) %>%
  left_join(mles %>% mutate(FOUL_FROM = as.factor(FOUL_FROM)), by = c("FOUL_FROM")) %>%
  mutate(est_surv_prob = 0.5) %>%
  left_join(data_censoring_model, by = c("FOUL_FROM", "FOULER")) %>%
  mutate(ind_brier_score = (1 - censored) * (X.y <= X.x) * (0 - est_surv_prob)^2/ipcw_times_i + 
           (X.x <  X.y) * (1 - est_surv_prob)^2/ipcw_times) %>%
  drop_na(ind_brier_score)


##########################
# Cox
##########################

cox_model <- 
  data %>%
  prep_stan_data(players_of_interest) %>%
  filter(FOULER %in% players_of_interest$FOULER) %>%
  unite(fouler_foul_from, FOULER, FOUL_FROM) %>%
  survival::coxph(survival::Surv(time = X, event = 1 - censored) ~ fouler_foul_from, data = ., x = T)



param_list <- 
  mles %>% 
  mutate(FOUL_FROM = as.factor(FOUL_FROM)) %>%
  mutate(fouler_foul_from = paste(FOULER, FOUL_FROM, sep = "_"))

cox_predicts <-
  pec::predictSurvProb(cox_model, 
                     times = unique(foul_time$X), 
                     newdata = tibble(fouler_foul_from = unique(param_list$fouler_foul_from))) %>%
  as_tibble() %>%
  mutate(fouler_foul_from =  unique(param_list$fouler_foul_from)) %>%
  pivot_longer(-fouler_foul_from, names_to = "time", values_to = "est_surv_prob") %>%
  mutate(X = rep(unique(foul_time$X), length(unique(param_list$fouler_foul_from))))

brier_scores_cox <-
  foul_time %>%
  arrange(FOUL_FROM) %>%
  mutate(ipcw_times = unlist(IPCW.times)) %>% 
  drop_na(ipcw_times) %>%
  left_join(mles %>% mutate(FOUL_FROM = as.factor(FOUL_FROM)), by = c("FOUL_FROM")) %>%
  mutate(fouler_foul_from = paste(FOULER, FOUL_FROM, sep = "_")) %>%
  left_join(cox_predicts, by = c("X", "fouler_foul_from")) %>%
  left_join(data_censoring_model, by = c("FOUL_FROM", "FOULER")) %>%
  mutate(ind_brier_score = (1 - censored) * (X.y <= X.x) * (0 - est_surv_prob)^2/ipcw_times_i + 
           (X.x <  X.y) * (1 - est_surv_prob)^2/ipcw_times) %>%
  drop_na(ind_brier_score)

#################### 
# KM
####################


km_model <- 
  data %>%
  prep_stan_data(players_of_interest) %>%
  filter(FOULER %in% players_of_interest$FOULER) %>%
  unite(fouler_foul_from, FOULER, FOUL_FROM) %>%
  survival::survfit(survival::Surv(time = X, event = 1 - censored) ~ fouler_foul_from, data = .)



param_list <- 
  mles %>% 
  mutate(FOUL_FROM = as.factor(FOUL_FROM)) %>%
  mutate(fouler_foul_from = paste(FOULER, FOUL_FROM, sep = "_"))

km_predicts <-
  pec::predictSurvProb(km_model, 
                       times = unique(foul_time$X), 
                       newdata = tibble(fouler_foul_from = unique(param_list$fouler_foul_from))) %>%
  as_tibble() %>%
  mutate(fouler_foul_from =  unique(param_list$fouler_foul_from)) %>%
  pivot_longer(-fouler_foul_from, names_to = "time", values_to = "est_surv_prob") %>%
  mutate(X = rep(unique(foul_time$X), length(unique(param_list$fouler_foul_from))))

brier_scores_km <-
  foul_time %>%
  arrange(FOUL_FROM) %>%
  mutate(ipcw_times = unlist(IPCW.times)) %>% 
  drop_na(ipcw_times) %>%
  left_join(mles %>% mutate(FOUL_FROM = as.factor(FOUL_FROM)), by = c("FOUL_FROM")) %>%
  mutate(fouler_foul_from = paste(FOULER, FOUL_FROM, sep = "_")) %>%
  left_join(cox_predicts, by = c("X", "fouler_foul_from")) %>%
  left_join(data_censoring_model, by = c("FOUL_FROM", "FOULER")) %>%
  mutate(ind_brier_score = (1 - censored) * (X.y <= X.x) * (0 - est_surv_prob)^2/ipcw_times_i + 
           (X.x <  X.y) * (1 - est_surv_prob)^2/ipcw_times) %>%
  drop_na(ind_brier_score)

b_1 <- 
  brier_scores %>%
  group_by(X.x, FOUL_FROM) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "bayesian")


b_2 <- 
  brier_scores_mle %>% 
  group_by(X.x, FOUL_FROM) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "mle")

b_3 <- 
  brier_scores_half %>% 
  group_by(X.x, FOUL_FROM) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "half")

b_4 <-
  brier_scores_cox %>% 
  group_by(X.x, FOUL_FROM) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "cox")


b_5 <-
  brier_scores_km %>% 
  group_by(X.x, FOUL_FROM) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "km")


b_1 %>%
  bind_rows(b_2) %>%
  bind_rows(b_3) %>%
  bind_rows(b_4) %>%
  bind_rows(b_5) %>%
  ggplot(aes(x = X.x/60 , y = brier_score, colour = model)) +
  geom_line() +
  facet_wrap(~ FOUL_FROM)


b_1 %>%
  bind_rows(b_2) %>%
  bind_rows(b_3) %>%
  bind_rows(b_4) %>%
  bind_rows(b_5) %>%
  group_by(FOUL_FROM, model) %>%
  summarise(brier_sum = sfsmisc::integrate.xy(x = X.x, fx = brier_score)/max(X.x)) %>%
  pivot_wider(names_from = model, values_from = brier_sum) %>%
  xtable::xtable(digits = 4)


#################################

b_1 <- 
  brier_scores %>%
  group_by(X.x) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "bayesian")


b_2 <- 
  brier_scores_mle %>% 
  group_by(X.x) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "mle")

b_3 <- 
  brier_scores_half %>% 
  group_by(X.x) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "half")

b_4 <-
  brier_scores_cox %>% 
  group_by(X.x) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "cox")


b_5 <-
  brier_scores_km %>% 
  group_by(X.x) %>%
  summarise(brier_score = mean(ind_brier_score)) %>%
  mutate(model = "km")


b_1 %>%
  bind_rows(b_2) %>%
  bind_rows(b_3) %>%
  bind_rows(b_4) %>%
  bind_rows(b_5) %>%
  ggplot(aes(x = X.x/60 , y = brier_score, colour = model)) +
  geom_line() 


b_1 %>%
  bind_rows(b_2) %>%
  bind_rows(b_3) %>%
  bind_rows(b_4) %>%
  bind_rows(b_5) %>%
  group_by(model) %>%
  summarise(brier_sum = sfsmisc::integrate.xy(x = X.x, fx = brier_score)/max(X.x)) %>%
  pivot_wider(names_from = model, values_from = brier_sum) 
