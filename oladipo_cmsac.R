df_18 <- read_foul_data(file_name = "online-data/events_2017-2018_pbp.csv")

df_18 %>%
  filter(player_name == "Victor Oladipo") %>%
  filter(TRANSITION_OBSERVED == 1) %>%
  summarise(mean(PT_UNTIL_FOUL)/60,
            max(PT_UNTIL_FOUL)/60,
            min(PT_UNTIL_FOUL)/60)

df_18 %>%
  filter(player_name == "Victor Oladipo") %>% 
  filter(TRANSITION_OBSERVED == 1, FOUL_TO == 6)


df_18 %>%
  filter(player_name == "Victor Oladipo") %>% 
  filter(TRANSITION_OBSERVED == 0) %>%
  ggplot(aes(x = PT_UNTIL_FOUL/60)) +
  geom_histogram()

df_18 %>%
  filter(player_name == "Victor Oladipo") %>%
  filter(TRANSITION_OBSERVED == 1) %>%  
  ggplot(aes(x = PT_UNTIL_FOUL/60)) +
  geom_histogram()


df_18 %>%
  filter(player_name == "Victor Oladipo") %>%
  group_by(GAME_ID) %>%
  filter(FOUL_TO == max(FOUL_TO)) %>% 
  ungroup() %>% filter(TRANSITION_OBSERVED == 1) %>% 
  select(GAME_ID)


library(tidyverse)
#posterior_results <- read_rds("posterior_results_constrained.rds")
#id_position_map <- read_csv("id_position_map.csv")


fits <- 
  tibble(files = list.files("extra_player_specific/", pattern = "fit_foul_from_position_constrained_", full.names = T)) %>%
  filter(str_detect(files, "203506")) %>%
  mutate(data = map(files, read_rds))


posterior_results <-
  fits %>%
  mutate(FOULER = parse_number(files),
         data_to_model = map(FOULER, ~ filter(data_to_model, FOULER == .)),
         tidy_fit = map2(data_to_model, data, ~ tidy_stan(.x, players_of_interest, .y))) %>%
  select(tidy_fit) %>%
  unnest(tidy_fit)


##############################
# ggplot defaults
##############################
theme_set(theme_bw() +
            theme(strip.background = element_rect(fill = "white")))


oladipo <- 
  posterior_results %>%
  filter(FOULER %in% c(203506)) %>%
  unnest(c(alpha, beta, y_new)) %>%
  mutate(expectation = alpha / beta) %>%
  group_by(FOUL_FROM, FOULER) %>%
  mutate(iter = row_number()) %>%
  group_by(FOULER, iter) %>%
  arrange(-FOUL_FROM) %>%
  mutate(time_to_out = cumsum(y_new)/60) %>%
  group_by(FOULER, FOUL_FROM) %>%
  mutate(percentile = 100 * row_number(time_to_out)/n())


oladipo %>%
  mutate(expectation = expectation/60,
         expectation = if_else(expectation > 53, 53, expectation)) %>%
  ggplot(aes(x = expectation, group = FOUL_FROM, fill = factor(FOUL_FROM)), alpha = 0.25) +
  ggridges::geom_density_ridges(aes(y = FOUL_FROM)) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#002D62", "#FDBB30", "#BEC0C2", "#002D62", "#FDBB30", "#BEC0C2")) +
  scale_x_continuous(expand = expand_scale(mult = 0), lim = c(0, NA)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)), breaks = 0:5, labels = 0:5) +
  labs(x = "Expected Playing Time To Foul (min)",
       y = "Current Number of Fouls") +
  theme(text = element_text(size = 18))
  

ggsave("oladipo_expectation.png", height = 5/1.1, width = 7/1.1, units = "in")



oladipo %>%
  mutate(y_new = y_new/60,
         y_new = if_else(y_new > 53, 53, y_new)) %>%
  filter(FOUL_FROM == 2) %>%
  ggplot(aes(x = y_new, group = FOUL_FROM, fill = factor(FOUL_FROM)), alpha = 0.25) +
  geom_density() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#002D62", "#FDBB30", "#BEC0C2", "#002D62", "#FDBB30", "#BEC0C2")) +
  scale_x_continuous(expand = expand_scale(mult = 0), lim = c(0, NA)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  labs(x = "Playing Time To 3rd Foul (min)",
       y = "Density") +
  theme(text = element_text(size = 18))

ggsave("oladipo_pred_time_to_foul.png", height = 5/1.1, width = 7/1.1, units = "in")

oladipo %>%
  mutate(time_to_out = if_else(time_to_out > 53, 53, time_to_out)) %>%
  filter(FOUL_FROM == 2) %>%
  ggplot(aes(x = time_to_out, group = FOUL_FROM, fill = "#002D62"), alpha = 0.25) +
  geom_density() +
  geom_vline(aes(xintercept = 47, colour = "Game Time Remaining"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(time_to_out), colour = "Median Playing Time to Foul Out"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 36, colour = "Time Reinserted in Game"), linetype = "dashed", size = 1) +
  scale_fill_identity() +
  scale_colour_manual(values = c("#FDBB30",
                                 "#BEC0C2",
                                 "black")) +
  scale_x_continuous(expand = expand_scale(mult = c(0, 0.05)), lim = c(0, NA)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  labs(x = "Playing Time To Foul Out (min)",
       y = "Density") +
  theme(text = element_text(size = 18),
        legend.title = element_blank())

ggsave("oladipo_pred_time_to_out_2.png", height = 5/1.1, width = 9, units = "in")


oladipo %>%
  mutate(time_to_out = if_else(time_to_out > 53, 53, time_to_out)) %>%
  filter(FOUL_FROM == 3) %>%
  ggplot(aes(x = time_to_out, group = FOUL_FROM, fill = "#002D62"), alpha = 0.25) +
  geom_density() +
  geom_vline(aes(xintercept = 28, colour = "Game Time Remaining"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(time_to_out), colour = "Median Playing Time to Foul Out"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 24, colour = "Time Reinserted in Game"), linetype = "dashed", size = 1) +
  scale_fill_identity() +
  scale_colour_manual(values = c("#FDBB30",
                                 "#BEC0C2",
                                 "black")) +
  scale_x_continuous(expand = expand_scale(mult = c(0, 0.05)), lim = c(0, NA)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  labs(x = "Playing Time To Foul Out (min)",
       y = "Density") +
  theme(text = element_text(size = 18),
        legend.title = element_blank())

ggsave("oladipo_pred_time_to_out_3.png", height = 5/1.1, width = 9, units = "in")


oladipo %>%
  mutate(time_to_out = if_else(time_to_out > 53, 53, round(time_to_out))) %>%
  select(FOUL_FROM, time_to_out, percentile, FOULER) %>%
  distinct() %>%
  group_by(FOUL_FROM, time_to_out) %>%
  filter(percentile == min(percentile)) %>%
  ungroup() %>%
  complete(time_to_out, FOUL_FROM) %>%
  arrange(FOUL_FROM, time_to_out) %>% 
  group_by(FOUL_FROM) %>%
  mutate(percentile = if_else(row_number() == 1 & is.na(percentile), 0, percentile),
         percentile = imputeTS::na_interpolation(percentile, option = "linear")) %>% 
  rename(`Number of Fouls` = FOUL_FROM) %>%
  ggplot(aes(x = time_to_out, y = 1, height = 10, fill = percentile)) +
  geom_raster(hjust = 1) +
  geom_vline(xintercept = c(12, 24, 36, 48), colour = "black", linetype = "dashed", size = 1) +
  facet_wrap(~ `Number of Fouls`, ncol = 1, labeller = label_both) +
  scale_fill_gradient2(name = "Probability of \nFouling Out", 
                       low = "#FDBB30", mid = "white", high = "#002D62", 
                       midpoint = 50, 
                       limits = c(0, 100)) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 48, 53), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Playing Time"
       #,title = "Victor Oladipo Predicted Time to Foul Out"
       ) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        text = element_text(size = 18)) 

ggsave("oladipo_cheat_sheet_18.png", height = 5/1.1, width = 7, units = "in")


oladipo %>%
  ungroup() %>%
  mutate(time_to_out = if_else(time_to_out > 53, 53, round(time_to_out)),
         FOUL_FROM = FOUL_FROM + 1) %>%
  select(FOUL_FROM, time_to_out, percentile, FOULER) %>%
  distinct() %>%
  group_by(FOUL_FROM, time_to_out) %>%
  filter(percentile == min(percentile)) %>%
  ungroup() %>%
  complete(time_to_out, FOUL_FROM) %>%
  arrange(FOUL_FROM, time_to_out) %>% 
  group_by(FOUL_FROM) %>%
  mutate(percentile = if_else(row_number() == 1 & is.na(percentile), 0, percentile),
         percentile = imputeTS::na_interpolation(percentile, option = "linear")) %>% 
  mutate(q_1 = abs(percentile - 25) == min(abs(percentile - 25)),
         q_2 = abs(percentile - 50) == min(abs(percentile - 50)),
         q_3 = abs(percentile - 75) == min(abs(percentile - 75))) %>%
  filter(FOUL_FROM > 2) %>%
  rename(`Next Foul` = FOUL_FROM) %>%
  ggplot(aes(x = time_to_out, y = 1, height = 10, fill = percentile)) +
  geom_raster(hjust = 1) +
  #geom_rect(aes(xmin = time_to_out, xmax = time_to_out + 1, ymin = 0.5, ymax = 1.5), colour = "black", data = . %>% filter(q_1)) + 
  #geom_rect(aes(xmin = time_to_out, xmax = time_to_out + 1, ymin = 0.5, ymax = 1.5), colour = "black", data = . %>% filter(q_2)) + 
  #geom_rect(aes(xmin = time_to_out, xmax = time_to_out + 1, ymin = 0.5, ymax = 1.5), colour = "black", data = . %>% filter(q_3)) + 
  geom_vline(xintercept = c(12, 24, 36, 48), colour = "black", linetype = "dashed", size = 1) +
  geom_text(aes(x = time_to_out, y = 1), hjust = 0, label = "25", colour = "black", data = . %>% filter(q_1)) + 
  geom_text(aes(x = time_to_out, y = 1), hjust = 0,label = "50", colour = "black", data = . %>% filter(q_2)) + 
  geom_text(aes(x = time_to_out, y = 1), hjust = 0,label = "75", colour = "black", data = . %>% filter(q_3)) + 
  facet_wrap(~ `Next Foul`, ncol = 1, labeller = label_both) +
  scale_fill_gradient2(name = "Foul Out \nPercentage", 
                       low = "#FDBB30", mid = "white", high = "#002D62", 
                       midpoint = 50, 
                       limits = c(0, 100)) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 48, 53), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Playing Time"
       #,title = "Victor Oladipo Predicted Time to Foul Out"
  ) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        text = element_text(size = 18)) 

ggsave("oladipo_cheat_sheet_17_18.png", height = 3.76, width = 7, units = "in")

posterior_results %>% 
  filter(position == "G") %>%
  filter(prior_type == "position") %>%
  filter(FOUL_FROM == 0) %>%
  unnest(c(y_new)) %>%
  group_by(prior_type, FOUL_FROM, FOULER) %>%
  mutate(iter = row_number()) %>%
  group_by(prior_type, FOULER, iter) %>%
  arrange(-FOUL_FROM) %>%
  mutate(time_to_out = cumsum(y_new)/60) %>%
  group_by(prior_type, FOULER, FOUL_FROM) %>%
  mutate(percentile = ntile(time_to_out, 100)) %>%
  summarise(m = median(time_to_out)) %>%
  ungroup() %>% top_n(-5)

westbrook <- 
  posterior_results %>%
  filter(FOULER %in% c(201566)) %>%
  unnest(c(alpha, beta, y_new)) %>%
  mutate(expectation = alpha / beta) %>%
  group_by(FOUL_FROM, FOULER) %>%
  mutate(iter = row_number()) %>%
  group_by(FOULER, iter) %>%
  arrange(-FOUL_FROM) %>%
  mutate(time_to_out = cumsum(y_new)/60) %>%
  group_by(FOULER, FOUL_FROM) %>%
  mutate(percentile = ntile(time_to_out, 100))  
  

westbrook %>%
 # filter(prior_type == "position") %>%
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
  rename(`Number of Fouls` = FOUL_FROM) %>%
  ggplot(aes(x = time_to_out, y = 1, height = 10, fill = percentile)) +
  geom_raster(hjust = 1) +
  geom_vline(xintercept = c(12, 24, 36, 48), colour = "black", linetype = "dashed", size = 1) +
  facet_wrap(~ `Number of Fouls`, ncol = 1, labeller = label_both) +
  scale_fill_gradient2(name = "Probability of \nFouling Out", 
                       low = "#007dc3", mid = "white", high = "#f05133", 
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


jokic <- 
  posterior_results %>%
  filter(FOULER %in% c(203999)) %>%
  unnest(c(alpha, beta, y_new)) %>%
  mutate(expectation = alpha / beta) %>%
  group_by(prior_type, FOUL_FROM, FOULER) %>%
  mutate(iter = row_number()) %>%
  group_by(prior_type, FOULER, iter) %>%
  arrange(-FOUL_FROM) %>%
  mutate(time_to_out = cumsum(y_new)/60) %>%
  group_by(prior_type, FOULER, FOUL_FROM) %>%
  mutate(percentile = ntile(time_to_out, 100))  

jokic %>%
  filter(prior_type == "position") %>%
  mutate(time_to_out = if_else(time_to_out > 53, 53, round(time_to_out))) %>%
  select(FOUL_FROM, time_to_out, percentile, prior_type, FOULER) %>%
  distinct() %>%
  group_by(FOUL_FROM, time_to_out) %>%
  filter(percentile == min(percentile)) %>%
  ungroup() %>%
  complete(time_to_out, FOUL_FROM) %>%
  arrange(FOUL_FROM, time_to_out) %>% 
  tidyr::fill(percentile, .direction = "up") %>% 
  rename(`Number of Fouls` = FOUL_FROM) %>% 
  ggplot(aes(x = time_to_out, y = 1, height = 10, fill = percentile)) +
  geom_raster(hjust = 1) +
  geom_vline(xintercept = c(12, 24, 36, 48), colour = "black", linetype = "dashed", size = 1) +
  facet_wrap(~ `Number of Fouls`, ncol = 1, labeller = label_both) +
  scale_fill_gradient2(name = "Probability of \nFouling Out", 
                       low = "#fdb927", mid = "white", high = "#4d90cd", 
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

ggsave("jokic_cheat_sheet.png", height = 5/1.1, width = 7, units = "in")


alpha <-
  oladipo %>%
  filter(FOUL_FROM == 3) %>%
  ggplot(aes(x = alpha, group = FOUL_FROM, fill = factor(FOUL_FROM)), alpha = 0.25) +
  geom_density() +
  scale_fill_manual(values = c("#002D62")) +
  scale_x_continuous(expand = expand_scale(mult = 0), lim = c(0, NA)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  labs(x = "Alpha",
       y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.position = "none")


beta <-
  oladipo %>%
  filter(FOUL_FROM == 3) %>%
  ggplot(aes(x = beta, group = FOUL_FROM, fill = factor(FOUL_FROM)), alpha = 0.25) +
  geom_density() +
  scale_fill_manual(values = c("#002D62")) +
  scale_x_continuous(expand = expand_scale(mult = 0), lim = c(0, NA)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  labs(x = "Beta",
       y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.position = "none")



y_new <-
  oladipo %>%
  filter(FOUL_FROM == 3) %>%
  ggplot(aes(x = y_new/60, group = FOUL_FROM, fill = factor(FOUL_FROM)), alpha = 0.25) +
  geom_density() +
  scale_fill_manual(values = c("#002D62")) +
  scale_x_continuous(expand = expand_scale(mult = 0), lim = c(0, NA)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  labs(x = "Predicted Playing Time To Foul (min)",
       y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.position = "none")


out <- 
  oladipo %>%
  filter(FOUL_FROM == 3) %>%
  ggplot(aes(x = time_to_out, group = FOUL_FROM, fill = factor(FOUL_FROM)), alpha = 0.25) +
  geom_density() +
  scale_fill_manual(values = c("#002D62")) +
  scale_x_continuous(expand = expand_scale(mult = 0), lim = c(0, NA)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  labs(x = "Predicted Playing Time to Foul Out (min)",
       y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.position = "none")



library(patchwork)

(alpha + beta) / (y_new + out)

ggsave("posterior_samples.png", height = 5/1.1, width = 7/1.1, units = "in")


oladipo %>%
  filter(prior_type == "position") %>%
  slice(425) %>%
  select(iter, alpha, beta, y_new, time_to_out) %>%
  mutate()
