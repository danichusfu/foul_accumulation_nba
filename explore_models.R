library(tidyverse)
#posterior_results <- read_rds("posterior_results_constrained.rds")
id_position_map <- read_csv("id_position_map.csv")


fits <- 
  tibble(files = list.files("player_specific_18_19/", pattern = "fit_foul_from_position_constrained_", full.names = T)) %>%
  mutate(data = map(files, read_rds))


posterior_results <-
  fits %>%
  mutate(FOULER = str_extract(files, "_\\d{3,}"),
         FOULER = parse_number(FOULER),
         data_to_model = map(FOULER, ~ filter(data_to_model, FOULER == .)),
         tidy_fit = map2(data_to_model, data, ~ tidy_stan(.x, players_of_interest, .y))) %>%
  select(tidy_fit) %>%
  unnest(tidy_fit)


#write_rds(posterior_results, "posterior_results_18_19.rds")

posterior_results <- read_rds("posterior_results_18_19.rds")

##############################
# ggplot defaults
##############################
theme_set(theme_bw() +
            theme(strip.background = element_rect(fill = "white")))


position_players <- 
  posterior_results %>%
  #filter(FOULER %in% top_10s$FOULER) %>% 
  mutate(post_mean_alpha = map_dbl(alpha, ~ mean(.$alpha)),
         post_mean_beta  = map_dbl(beta, ~ mean(.$beta)),
         post_mean_y_new  = map_dbl(y_new, ~ mean(.$y_new)),
         post_mean = map2_dbl(alpha, beta, ~ mean(.x$alpha/.y$beta)),
         position = case_when(position == "G" ~ "Guard",
                              position == "F" ~ "Forward",
                              position == "C" ~ "Big"))


posterior_results %>%
  filter(FOULER == 203507) %>%
  unnest(c(alpha, beta, y_new)) %>%
  ungroup() %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  ggplot(aes(x = y_new/60, group = FOUL_FROM, fill = factor(FOUL_FROM)), alpha = 0.25) +
  ggridges::geom_density_ridges(aes(y = factor(FOUL_FROM), height = ..density..), stat = "density", trim = TRUE) +
  coord_cartesian(xlim = c(0, 48)) +
  labs(y = "Foul Level",
       x = "Time in Minutes") +
  scale_x_continuous(expand = expand_scale(mult = 0.00), lim = c(0, NA)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.00, 0.4))) +
  scale_fill_manual(name = "Foul Level", 
                    values = c("#00471B",
                               "#AC1A2F",
                               "#702F8A",
                               "#000000",
                               "#95999D",
                               "#EEE1C6"
                               )) +
  theme(text = element_text(size = 14))


#ggsave("plots/giannis_predictive_densities.png", height = 5, width = 7, units = "in")


posterior_results %>%
  filter(FOULER == 203507) %>%
  unnest(c(alpha, beta, y_new)) %>%
  ungroup() %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  group_by(foul_level = FOUL_FROM) %>%
  summarise(mean = mean(y_new)/60,
            median = median(y_new)/60)


posterior_results %>%
  filter(FOULER == 2544) %>%
  unnest(c(alpha, beta, y_new)) %>%
  ungroup() %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  group_by(foul_level = FOUL_FROM) %>%
  summarise(mean = mean(y_new)/60,
            median = median(y_new)/60)

posterior_results %>%
  filter(FOULER == 2544) %>%
  unnest(c(alpha, beta, y_new)) %>%
  ungroup() %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  filter(FOUL_FROM == 6) %>%
  mutate(q = cume_dist(y_new)) %>%
  arrange(q) %>%
  #filter(q > 0.098)
  #filter(q > 0.298)
  filter(q > 0.498)


posterior_results %>%
  filter(FOULER == 1626157) %>%
  unnest(c(alpha, beta, y_new)) %>%
  ungroup() %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  filter(FOUL_FROM == 6) %>%
  mutate(q = cume_dist(y_new)) %>%
  arrange(q) %>%
  #filter(q > 0.098)
  #filter(q > 0.298)
filter(q > 0.498)


### CHEAT SHEET LILLARD

posterior_results %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  filter(FOULER %in% c(203081)) %>%
  unnest(c(alpha, beta, y_new)) %>%
  mutate(expectation = alpha / beta) %>%
  group_by(FOUL_FROM, FOULER) %>%
  mutate(iter = row_number()) %>%
  group_by(FOULER, iter) %>%
  arrange(-FOUL_FROM) %>%
  mutate(time_to_out =cumsum(y_new)/60) %>%
  group_by(FOULER, FOUL_FROM) %>%
  mutate(percentile =  100 * cume_dist(time_to_out))  %>% 
  mutate(time_to_out = if_else(time_to_out > 53, 53, round(time_to_out))) %>%
  select(FOUL_FROM, time_to_out, percentile, FOULER) %>%
  distinct() %>%
  group_by(FOUL_FROM, time_to_out) %>%
  filter(percentile == min(percentile)) %>%
  ungroup() %>%
  complete(time_to_out, FOUL_FROM) %>% 
  arrange(FOUL_FROM, time_to_out) %>% 
  tidyr::fill(percentile, .direction = "up") %>% 
  mutate(percentile = replace_na(percentile, 100)) %>%
  rename(`Foul Level` = FOUL_FROM) %>%
  ggplot(aes(x = time_to_out, y = 1, height = 10, fill = percentile)) +
  geom_raster(hjust = 1) +
  geom_vline(xintercept = c(12, 24, 36, 48), colour = "black", linetype = "dashed", size = 1) +
  facet_wrap(~ `Foul Level`, ncol = 1, labeller = label_both) +
  scale_fill_gradient2(name = "Probability of \nFouling Out", 
                       low = "#E03A3E", mid = "white", high = "#000000", 
                       midpoint = 50, 
                       limits = c(0, 100)) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 48, 53), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Playing Time") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        text = element_text(size = 18)) 

ggsave("lillard_cheat_sheet_18.png", height = 5/1.1, width = 7, units = "in")


posterior_results %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  filter(FOULER %in% c(203081)) %>%
  unnest(c(alpha, beta, y_new)) %>%
  mutate(expectation = alpha / beta) %>%
  group_by(FOUL_FROM, FOULER) %>%
  mutate(iter = row_number()) %>%
  group_by(FOULER, iter) %>%
  arrange(-FOUL_FROM) %>%
  mutate(time_to_out =cumsum(y_new)/60) %>%
  group_by(FOULER, FOUL_FROM) %>%
  mutate(percentile =  100 * cume_dist(time_to_out))  %>% 
  mutate(time_to_out = if_else(time_to_out > 53, 53, round(time_to_out))) %>%
  select(FOUL_FROM, time_to_out, percentile, FOULER) %>%
  distinct() %>%
  group_by(FOUL_FROM, time_to_out) %>%
  filter(percentile == min(percentile)) %>%
  ungroup() %>%
  complete(time_to_out, FOUL_FROM) %>% 
  arrange(FOUL_FROM, time_to_out) %>% 
  tidyr::fill(percentile, .direction = "up") %>% 
  mutate(percentile = replace_na(percentile, 100)) %>%
  filter(FOUL_FROM == 3) %>%
  rename(`Foul Level` = FOUL_FROM) %>%
  ggplot(aes(x = time_to_out, y = 100 - percentile, height = 10)) +
  geom_line() +
  geom_vline(xintercept = c(12, 24, 36, 48), colour = "black", linetype = "dashed", size = 1) +
  #facet_wrap(~ `Foul Level`, ncol = 1, labeller = label_both) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 48, 53), expand = expand_scale(add = c(0, 0.6))) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)), limits = c(0, 100)) +
  labs(x = "Playing Time",
       y = "Survival Probability") +
  theme(text = element_text(size = 18)) 

ggsave("lillard_surv_prob_3_foul_level.png", height = 5/1.1, width = 7, units = "in")




posterior_results %>%
  filter(FOULER %in% c(203507, 202699)) %>%
  unnest(c(alpha, beta, y_new)) %>%
  ungroup() %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  group_by(foul_level = FOUL_FROM, player_name) %>%
  summarise(mean = mean(y_new)/60) %>%
  pivot_wider(names_from = player_name, values_from = mean)


h_b_mean_time <-
  posterior_results %>%
  filter(FOULER %in% c(203084)) %>%
  unnest(c(alpha, beta, y_new)) %>%
  ungroup() %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  group_by(foul_level = FOUL_FROM) %>%
  summarise(mean = mean(y_new)/60) 


h_b_sample_size <- 
  data_to_model %>%
  filter(FOULER == 203084) %>% 
  count(foul_level = FOUL_FROM + 1, censored) %>%
  pivot_wider(names_from = censored, values_from = n) %>%
  rename(Observed = `0`, Censored = `1`) %>%
  select(-foul_level)

h_b_mle_mean <- 
  data_to_model %>%
  filter(FOULER == 203084) %>%
  group_by(FOULER, FOUL_FROM) %>%
  ungroup() %>%
  select(FOULER, FOUL_FROM, X, censored) %>%
  left_join(id_position_map, by = c("FOULER")) %>%
  group_by(FOULER, FOUL_FROM) %>%
  mutate(num_observed = sum(censored == 0)) %>%
  nest(data = - c(FOULER, FOUL_FROM)) %>%
  mutate(mle_gamma = map(data, possibly(~ fit_gamma_cens(.$censored, .$X), NA))) %>%
  unnest(mle_gamma) %>%
  mutate(mle_mean = shape/rate/60) %>%
  ungroup() %>%
  select(mle_mean)

prior_mean <-
  priors %>%
  filter(str_detect(foul_from_position, "F")) %>%
  mutate(prior_mean = map_dbl(mean, ~ .[1]/.[2])/60) %>%
  select(prior_mean)


bind_cols(h_b_mean_time, h_b_mle_mean, prior_mean, h_b_sample_size, )

posterior_results %>%
  filter(FOULER %in% c(203507, 202699)) %>%
  unnest(c(alpha, beta, y_new)) %>%
  ungroup() %>%
  mutate(FOUL_FROM = FOUL_FROM + 1) %>%
  ggplot(aes(x = y_new/60, y = factor(FOUL_FROM), group = FOULER, fill = factor(FOUL_FROM)), alpha = 0.25) +
  ggridges::geom_density_ridges(aes(y = factor(FOUL_FROM), height = ..density..), stat = "density", trim = TRUE) +
  coord_cartesian(xlim = c(0, 48)) +
  labs(y = "Foul Level",
       x = "Predictive Densities") +
  scale_x_continuous(expand = expand_scale(mult = 0.00), lim = c(0, NA)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.00, 0.4))) +
  scale_fill_manual(name = "Foul Level", 
                    values = c("#00471B",
                               "#AC1A2F",
                               "#702F8A",
                               "#000000",
                               "#95999D",
                               "#EEE1C6"
                    )) +
  theme(text = element_text(size = 14))






