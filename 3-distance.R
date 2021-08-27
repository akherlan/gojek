# Title:           Distance
# Author:          Andi Herlan
# Email:           andi.herlan@protonmail.com
# Data Used:       gojek.rds
# Packages Used:   dplyr, mapboxapi, ggplot2, ggrepel
# Output File:     distance.png
# Data Output:     -
# Reference:       https://ggplot2-book.org/annotations.html


# clear environment
rm(list = ls())

library(dplyr)
library(mapboxapi)
library(ggplot2)
library(ggrepel)

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

# plot 
p1 <- ggplot(data = distn) +
  # total distance
  annotate(geom = "text", x = 1.8, y = 28, size = 3.2, hjust = "left",
           label = paste0("Total distance:\n", sum(gojek$distance),
                         "km (", nrow(gojek), " trips)"),
           colour = "gray50") +
  # total duration
  annotate(geom = "text", x = 56, y = 2, size = 3.2, hjust = "right",
           label = paste0(round(sum(gojek$duration)/3600, 2), 
                          " hours\non the road"),
           colour = "gray50") +
  # resettlement
  annotate(geom = "point", x = comeback$duration/60, alpha = 0.4,
           y = comeback$distance, size = 3.5, col = "orange") +
  annotate(geom = "curve", x = 62, y = 25, size = 0.3, col = "gray50",
           xend = comeback$duration/60-0.2, yend = comeback$distance-0.4,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", label = "Resettled\nfrom Srengseng\nto Depok", 
           x = 61.4, y = 25, hjust = "right", size = 3.5, col = "gray50") +
  # must be wrong
  annotate(geom = "curve", x = 10.4, y = 15.5, size = 0.3, col = "gray50",
           xend = wrongride$duration[1]/60+0.2, yend = wrongride$distance[1]+0.4,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", label = "Aladdin's carpet\n(only ~3-6 second!)", 
           hjust = "left", x = 11.4, y = 15.5, size = 3, col = "gray50") +
  # scatter plot
  geom_point(aes(duration/60, distance, colour = vehicle), alpha = 0.4) +
  # scales
  scale_y_continuous(breaks =  seq(0, 30, 5)) +
  scale_x_continuous(limits = c(0, 75), breaks = c(seq(0, 15, 5), 30, 45, 60, 75)) +
  scale_colour_manual(values = c("#000000", "#00AA13")) +
  # main labels
  labs(title = "Travel with GOJEK", 
       subtitle = paste0("Equivalent to Jakarta-Surabaya (781 km) by car"),
       caption = "Github: akherlan | Data: GOJEK",
       x = "Duration (minutes)", y = "Distance (km)",
       colour = "GO") +
  # styling
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    legend.position = c(0.85, 0.28),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(colour = "gray40"),
    plot.caption = element_text(colour = "gray60"),
    axis.title = element_text(colour = "gray50"),
    axis.text = element_text(colour = "gray50"),
    plot.background = element_rect(fill = "white", size = 0)
  )

# save PNG
ggsave("distance.png", path = "figs", dpi = 150, units = "px",
       width = 2*817, height = 2*516)

# add map data
java <- readRDS("output/java.rds")

# geocoding
js <- tribble(~city, "Jakarta", "Surabaya")
jscoord <- lapply(js$city, mb_geocode) %>% as.data.frame()
jscoord <- as_tibble(t(jscoord))
names(jscoord) <- c("long", "lat")
js <- bind_cols(js, jscoord)

# longtrip Jakarta - Surabaya
jsline <- mb_directions(origin = "Jakarta", destination = "Surabaya")

p2 <- ggplot(java) +
  geom_sf(fill = "gray90", alpha = 0.3, colour = "gray80", size = 0.2) +
  geom_sf(data = jsline, colour = "black", size = 0.5, linetype = 2) +
  geom_point(data = js, aes(long, lat), 
             colour = "red", size = 2, alpha = 0.6) +
  coord_sf() +
  theme_void()

p1 + annotation_custom(ggplotGrob(p2), xmin = 3, xmax = 45, ymin = 18, ymax = 27)

# save PNG
ggsave("distance_map.png", path = "figs", dpi = 150, units = "px",
       width = 2*817, height = 2*516)
