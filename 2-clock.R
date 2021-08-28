# Title:           Clock
# Author:          Andi Herlan
# Email:           andi.herlan@protonmail.com
# Data Used:       gojek.rds
# Packages Used:   dplyr, lubridate, ggplot2
# Output File:     clock.png
# Data Output:     -
# Reference:       https://www.wjakethompson.com/post/2018-11-27-ggclock/


# clear environment
rm(list = ls())

library(dplyr)
library(lubridate)
library(ggplot2)

# main data
gojek <- readRDS("output/gojek.rds")

# data for clocking
clock <- gojek %>% 
  summarise(datetime = datetime) %>% 
  mutate(y = as.numeric(pm(datetime)),
         x = hour(datetime),
         x = ifelse(y == 1, x-12L, x)*60L,
         x = x + minute(datetime),
         col = gojek$vehicle) %>% 
  arrange(datetime)

total_time <- round(sum(gojek$duration)/3600, 2)

# plot
ggplot(data = clock) +
  # am / pm
  annotate(geom = "text", x = 0, y = 0.5, label = "AM",
           size = 4, colour = "gray40", alpha = 0.3) +
  annotate(geom = "text", x = 0, y = 1.5, label = "PM",
           size = 4, colour = "gray40", alpha = 0.3) +
  # total time
  annotate(geom = "text", x = 0, y = -1,
           size = 4, colour = "gray40", alpha = 0.8,
           label = paste0("Total\n", total_time, " hours\non the road")) +
  # ride
  geom_segment(aes(x, y, xend = x + 1, yend = y + 0.8), 
               colour = "#00AA13", alpha = 0.4) +
  # car
  geom_point(data = filter(clock, clock$col == "Car"), 
             aes(x, y), colour = "#000000", alpha = 0.8) +
  # coordinate
  coord_polar() +
  expand_limits(y = c(-1, 1)) +
  scale_x_continuous(limits = c(0, 720),
                     breaks = seq(180, 720, 180), 
                     labels = c(3, 6, 9, "12")) +
  # text
  labs(title = "Time with GOJEK", x = NULL, y = NULL,
       subtitle = paste("Andi's movement in", 
                        min(year(gojek$datetime)), "-", 
                        max(year(gojek$datetime)), "(• is GoCar)", sep = " "),
       caption = paste("Last movement at", tail(gojek$datetime, 1),
                       "AM for vaccination\nGithub: akherlan | Data: GOJEK", 
                       sep = " ")) +
  # styling
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.3),
    panel.grid.major.y = element_line(size = 0.3, linetype = 2),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(colour = "gray40"),
    plot.caption = element_text(colour = "gray60"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", size = 0)
  )

# save PNG
ggsave("clock.png", path = "figs", dpi = 150, units = "px",
       width = 2*540, height = 2*507)
