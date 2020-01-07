library(tidyverse)

tibble(action = c("On", "Off", "On", "Off", "On", "Off", "On"),
       quarter = c(1, 2, 2, 3, 3, 4, 4),
       time_remaining_in_qtr = c(658, 720, 287, 720, 57, 530, 0),
       home_score = c(0, 18, 35, 48, 67, 76, 97),
       away_score = c(4, 33, 43, 58, 74, 83, 100)) %>%
  mutate(quarter_remaining = 4 - quarter,
         time_remaining = quarter_remaining * 720 + time_remaining_in_qtr,
         time_remaining_up = 2880 - time_remaining,
         home_diff = home_score - away_score,
         diff = home_diff - lag(home_diff, default = 0),
         action = factor(action, levels = c("On", "Off"))) %>%
  ggplot() +
  geom_rect(aes(xmin = lag(time_remaining_up, default = 0), xmax = time_remaining_up, ymin = 0, ymax = 150, fill = action)) +
  geom_text(aes(y = 75, x = time_remaining_up - (time_remaining_up - lag(time_remaining_up, default = 0))/2, label = diff), colour = "white") +
  annotate("text", x = 46, y = 280, label = "1st", hjust = 0.75) +
  annotate("line", x = c(46, 46), y = c(150, 250), colour = "#BEC0C2") +
  annotate("text", x = 62, y = 225, label = "2nd Foul", hjust = -0.1) +
  annotate("line", x = c(62, 62), y = c(150, 250), colour = "#BEC0C2") +
  annotate("text", x = 1153, y = 225, label = "3rd Foul", hjust = -0.1) +
  annotate("line", x = c(1153, 1153), y = c(150, 250), colour = "#BEC0C2") +
  scale_fill_manual(name = "Oladipo is", values = c("#002D62", "#FDBB30")) +
  scale_y_continuous(expand = c(0, 0), lim = c(0, 350)) +
  scale_x_continuous(name = "Quarter", 
                     breaks = c(360, 1080, 1800, 2520), 
                     labels = c("1st", "2nd", "3rd", "4th")) +
  lemon::coord_flex_fixed(ratio = 1, bottom = lemon::brackets_horizontal(length = unit(0.112, 'npc'))) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(vjust = -1),
        axis.title.x = element_text(vjust = -0.1),
        legend.position = "bottom",
        text = element_text(size = 18)) 

ggsave("oladipo_pm.png", height = 4/1.5, width = 12/1.5, units = "in")
  