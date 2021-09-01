# Title:           Clock
# Author:          Andi Herlan
# Email:           andi.herlan@protonmail.com
# Data Used:       gojek.rds
# Packages Used:   dplyr, lubridate, ggplot2, gridExtra
# Source:          colours.R
# Output File:     clock.png
# Data Output:     -
# Reference:       https://www.wjakethompson.com/post/2018-11-27-ggclock/


# clear environment
rm(list = ls())

library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

# import colour definition
source("colours.R")

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

# total time on the road
total_time <- round(sum(gojek$duration)/3600, 2)

# duration per year
dura <- gojek %>% 
  select(datetime, duration) %>% 
  mutate(year = year(datetime)) %>% 
  group_by(year) %>% 
  summarise(duration = sum(duration), .groups = "drop") %>% 
  mutate(x1 = cumsum(duration),
         x0 = x1 - duration)

limit_x_max <- round(max(dura$x1)) + 15500

# plot clock
p1 <- ggplot(data = clock) +
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
               colour = gc_green50, alpha = 0.4) +
  # car
  geom_point(data = filter(clock, clock$col == "Car"), 
             aes(x, y), colour = "black", alpha = 0.8) +
  # coordinate
  coord_polar() +
  expand_limits(y = c(-1, 1)) +
  scale_x_continuous(limits = c(0, 720),
                     breaks = seq(180, 720, 180), 
                     labels = c(3, 6, 9, "12")) +
  # text
  labs(title = "Time with GOJEK", x = NULL, y = NULL,
       subtitle = paste("Movement in", 
                        min(year(gojek$datetime)), "-", 
                        max(year(gojek$datetime)), "(• is GoCar)", sep = " "),
       caption = paste0("Last movement at ", tail(gojek$datetime, 1),
                        " AM for vaccination")) +
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
    # panel.border = element_rect(colour = "black", size = 1, fill = "transparent", linetype = 2),
    plot.background = element_rect(fill = "white", size = 0)
  )

# plot duration
p2 <- ggplot(data = dura) +
  geom_text(aes(x = x1 + 1500, y = year, 
                label = paste0("~", round(duration/3600), " hours")),
           size = 2.8, colour = "gray60", hjust = "left") +
  geom_linerange(aes(xmin = x0, xmax = x1, y = year),
                 size = 4.6, alpha = 0.5, colour = gc_green50) + 
  scale_x_continuous(limits = c(0, limit_x_max)) +
  labs(title = "Duration per year",
       caption = "CC BY-SA | Github: akherlan | Data: GOJEK") +
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8, colour = "gray40"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(colour = "gray40"),
    plot.caption = element_text(colour = "gray60"),
    plot.title = element_text(size = 10, colour = "gray40"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", size = 0)
  )

# combine plot
p3 <- grid.arrange(p1, p2, ncol = 1, nrow = 2, heights = c(4, 1.2))

# save PNG
ggsave("clock.png", plot = p3, path = "figs", dpi = 150, units = "px",
       width = 2.5*306, height = 2.5*488)

