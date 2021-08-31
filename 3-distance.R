# Title:           Distance
# Author:          Andi Herlan
# Email:           andi.herlan@protonmail.com
# Data Used:       gojek.rds, java.rds
# Packages Used:   dplyr, mapboxapi, ggplot2, ggrepel, gridExtra
# Source:          colours.R
# Output File:     distance.png
# Data Output:     -
# Reference:       https://ggplot2-book.org/annotations.html
#                  https://genviz.org/module-02-r/0002/03/02/arrangingPlots/


# clear environment
rm(list = ls())

library(dplyr)
library(mapboxapi)
library(ggplot2)
library(ggrepel)
library(gridExtra)

# import colour definition
source("colours.R")

# main data
gojek <- readRDS("output/gojek.rds")

# data for distance 
distn <- gojek %>% 
  select(vehicle, distance, duration)

# resettlement
comeback <- gojek %>% 
  arrange(distance) %>% 
  tail(1)

# aladdin's carpet
wrongride <- gojek %>% 
  arrange(duration) %>% 
  head(3)

# late for the last train schedule
latetrain <- gojek %>% 
  arrange(distance) %>% 
  tail(2) %>% 
  head(1) %>% 
  select(duration, distance, driver)

# data for map
java <- readRDS("output/java.rds")

# geocoding
od <- tribble(~city, "Pelabuhan Merak", "Banyuwangi")
odcoord <- lapply(od$city, mb_geocode) %>% as.data.frame()
odcoord <- as_tibble(t(odcoord))
names(odcoord) <- c("long", "lat")
od <- bind_cols(od, odcoord)

# longtrip Merak - Banyuwangi
odline <- mb_directions(origin = "pelabuhan merak", 
                        destination = "banyuwangi",
                        profile = "driving",
                        language = "id")

# data for density plot
dens <- gojek %>% 
  select(duration, distance)

# scatter plot -----
p1 <- ggplot(data = distn) +
  # total distance
  annotate(geom = "text", x = 1.8, y = 28, size = 3.2, hjust = "left",
           label = paste0("Total distance:\n", sum(gojek$distance),
                         " km (", nrow(gojek), " trips)"),
           colour = "gray50") +
  # total duration
  annotate(geom = "text", x = 56, y = 2, size = 3.2, hjust = "right",
           label = paste0(round(sum(gojek$duration)/3600, 2), 
                          " hours\non the road"),
           colour = "gray50") +
  # late for the last train
  annotate(geom = "point", x = latetrain$duration/60, alpha = 0.3,
           y = latetrain$distance, size = 4, col = gc_orange) +
  annotate(geom = "curve", x = 50, y = 17, size = 0.3, col = "gray50",
           xend = latetrain$duration/60+0.6, yend = latetrain$distance,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 50.4, y = 16, hjust = "left",
           label = paste("Late for the last", 
                         "CommuterLine schedule",
                         "during Covid-19 outbreak restriction",
                         "(Sudirman to Depok)",
                         sep = "\n"), 
           size = 3, col = "gray50") +
  # resettlement
  annotate(geom = "point", x = comeback$duration/60, alpha = 0.3,
           y = comeback$distance, size = 4, col = gc_orange) +
  annotate(geom = "curve", x = 62, y = 25, size = 0.3, col = "gray50",
           xend = comeback$duration/60-0.2, yend = comeback$distance-0.4,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", label = "Resettled\nfrom Srengseng\nto Depok", 
           x = 61.4, y = 25, hjust = "right", size = 3.2, col = "gray50") +
  # must be wrong
  annotate(geom = "curve", x = 10.4, y = 15.5, size = 0.3, col = "gray50",
           xend = wrongride$duration[1]/60+0.2, yend = wrongride$distance[1]+0.4,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", label = "Aladdin's carpet\n(only ~3-6 second!)", 
           hjust = "left", x = 11.4, y = 15.5, size = 3, col = "gray50") +
  # scatter plot
  geom_point(aes(duration/60, distance, colour = vehicle), 
             size = 2, alpha = 0.4) +
  # scales
  scale_y_continuous(breaks =  seq(0, 30, 5)) +
  scale_x_continuous(limits = c(0, 75), breaks = c(seq(0, 15, 5), 30, 45, 60, 75)) +
  scale_colour_manual(values = c("black", gc_green70)) +
  # main labels
  labs(
    title = "Movement with GOJEK",
    subtitle = paste0("As far as Merak to Banyuwangi (>1100 km) by car"),
    # caption = "Github: akherlan | Data: GOJEK",
    # x = "Duration (minutes)", y = "Distance (km)",
    colour = "GO") +
  # styling
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    legend.position = c(0.85, 0.28),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "gray40"),
    plot.caption = element_text(colour = "gray60"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = unit(c(0.2, 0.2, 0, 0), "cm"),
    plot.background = element_rect(fill = "white", size = 0)
  )

# density plot -----
# duration density
dx <- ggplot(dens) +
  geom_density(aes(x = duration/60, y = -..density..), 
               fill = gc_green20, colour = "gray35", alpha = 0.5) +
  labs(x = "Duration (minutes)", caption = "Github: akherlan | Data: GOJEK") +
  scale_x_continuous(limits = c(0, 75), breaks = c(seq(0, 15, 5), 30, 45, 60, 75)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_text(colour = "gray50"),
    axis.text = element_text(colour = "gray50"),
    plot.caption = element_text(colour = "gray60"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0, 0.2, .2, 0), "cm"),
    plot.background = element_rect(fill = "white", size = 0))

# distance density
dy <- ggplot(dens) +
  geom_density(aes(x = -..density.., y = distance), 
               fill = gc_green20, colour = "gray35", alpha = 0.5) +
  scale_y_continuous(breaks =  seq(0, 30, 5)) +
  labs(y = "Distance (km)", title = " ", subtitle = " ") +
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_text(colour = "gray50"),
    axis.text = element_text(colour = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.2, 0, 0, 0.2), "cm"),
    plot.background = element_rect(fill = "white", size = 0))

# map plot -----
p2 <- ggplot(java) +
  # island
  geom_sf(fill = "gray90", alpha = 0.3, colour = "gray80", size = 0.2) +
  # road
  geom_sf(data = odline, colour = "black", size = 0.5, linetype = 2) +
  # location
  geom_point(data = od, aes(long, lat), 
             colour = gc_red, size = 2, alpha = 0.6) +
  coord_sf() +
  theme_void()

p3 <- p1 + annotation_custom(ggplotGrob(p2), xmin = 0, xmax = 30, ymin = 19, ymax = 26)

# composition -----
c1 <- grid.arrange(p1, dy, dx,
                   ncol = 2, nrow = 2, 
                   layout_matrix = rbind(c(2,1), c(NA,3)),
                   widths = c(0.1, 0.9), heights = c(0.8, 0.2))

# save PNG
ggsave("distance.png", path = "figs", dpi = 150, units = "px",
       width = 2*817, height = 2*516, plot = c1)

# composition plot with map -----
c2 <- grid.arrange(p3, dy, dx,
                   ncol = 2, nrow = 2, 
                   layout_matrix = rbind(c(2,1), c(NA,3)),
                   widths = c(0.1, 0.9), heights = c(0.8, 0.2))

# save PNG
ggsave("distance_map.png", path = "figs", dpi = 150, units = "px",
       width = 2*817, height = 2*516, plot = c2)
