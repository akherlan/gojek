# Title:           Needs
# Author:          Andi Herlan
# Email:           andi.herlan@protonmail.com
# Data Used:       gojek.rds
# Packages Used:   dplyr, tidyr, stringr, ggplot2, ggalluvial
# Output File:     needs.png
# Data Output:     od.rds
# Reference:       https://corybrunson.github.io/ggalluvial


# clear environment
rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggalluvial)

# main data
gojek <- readRDS("output/gojek.rds")

# origin destination
od <- gojek %>% 
  select(datetime, origin = pickup, destination) %>% 
  pivot_longer(cols = c(origin, destination), 
               names_to = "pin",
               values_to = "address") %>% 
  separate(address, sep = ",", into = c("location", "drop"), remove = FALSE) %>% 
  mutate(location = ifelse(is.na(location), address, location)) %>% 
  select(-address, -drop)

# better for string
od$location <- tolower(od$location)
od$location <- str_replace_all(od$location, "-", " ")

# location categories, OMG my code! $#^$#@%
od <- od %>% 
  mutate(
    # detect station
    station = ifelse(str_detect(location, "stasiun"), TRUE, NA),
    station = ifelse(str_detect(location, "station"),
                    TRUE, station),
    # detect bus stop
    bus = ifelse(str_detect(location, "halte"), TRUE, NA),
    bus = ifelse(str_detect(location, "kelapa dua sasak rt.2/rw.2"), 
                    TRUE, bus),
    # detect office
    office = ifelse(str_detect(location, "tempo"), TRUE, NA),
    office = ifelse(str_detect(location, "palmerah barat no.22"),
                       TRUE, office),
    office = ifelse(str_detect(location, "kementerian pekerjaan umum"),
                       TRUE, office),
    office = ifelse(str_detect(location, "ministry of public works"),
                       TRUE, office),
    office = ifelse(str_detect(location, "pattimura no.20"),
                       TRUE, office),
    office = ifelse(str_detect(location, "raden patah"),
                       TRUE, office),
    office = ifelse(str_detect(location, "al azhar"),
                       TRUE, office),
    # detect home
    home = ifelse(str_detect(location, "jalan taman indah"), TRUE, NA), # h. ranto
    home = ifelse(str_detect(location, "al makmur"), # bu lastri
                     TRUE, home),
    home = ifelse(str_detect(location, "alfamidi raya rtm"), # h. ranto
                     TRUE, home),
    home = ifelse(str_detect(location, "tugu kp areman rt 04/ rw 05 no. 38"),
                     TRUE, home),
    home = ifelse(str_detect(location, "sasak i no.39"), # bu mamay
                     TRUE, home),
    home = ifelse(str_detect(location, "srengseng raya no.45"), # h. nasir
                     TRUE, home),
    home = ifelse(str_detect(location, "lap. tenis"), # h. nasir
                     TRUE, home),
    home = ifelse(str_detect(location, "rumbut no.4"), # h. ranto
                     TRUE, home),
    home = ifelse(str_detect(location, "h. moat"), # bu lastri
                     TRUE, home),
    home = ifelse(str_detect(location, "wisma srikandi"), # bu lastri
                     TRUE, home),
    home = ifelse(str_detect(location, "rtm lampu merah"), # bu lastri
                     TRUE, home),
    home = ifelse(str_detect(location, "bu lastri"), # bu lastri
                     TRUE, home),
    home = ifelse(str_detect(location, "srengseng raya no.7b"), # h. nasir
                     TRUE, home),
    # other places
    place = ifelse(!is.na(station), NA, ifelse(
                     !is.na(bus), NA, ifelse(
                       !is.na(office), NA, ifelse(
                         !is.na(home), NA, TRUE))))
  )

# empty category
nrow(od) - colSums(!is.na(od[,4:8])) %>% as.matrix() %>% sum()

# odplot <- od %>% 
#   select(-datetime) %>%
#   mutate(count = 1) %>% 
#   group_by(origin, destination, payment) %>% 
#   summarise(freq = sum(count), .groups = "drop")

# enrichment
od <- od %>% 
  select(-location) %>% 
  pivot_longer(cols = c("station", "bus", "office", "home", "place"),
               names_to = "category",
               values_to = "value") %>% 
  filter(value == TRUE) %>% 
  select(-value) %>% 
  pivot_wider(id_cols = "datetime", 
              names_from = "pin",
              values_from = "category")

# save od data
saveRDS(od, "output/od.rds")

# add payment
od <- gojek %>% 
  select(datetime, payment) %>% 
  left_join(od, by = "datetime")

# data for needs
odp <- od %>% 
  select(-datetime) %>% 
  mutate(destination = ifelse(destination == "home", "Go Home",
                              ifelse(
                                destination == "bus", "Transit",
                                ifelse(
                                  destination == "station", "Transit",
                                  ifelse(
                                    destination == "office", "Work",
                                    "Business")))),
         origin = ifelse(origin %in% c("station", "bus"),
                         "stop", origin),
         origin = str_to_title(origin), 
         count = 1) %>% 
  group_by(origin, destination, payment) %>% 
  summarise(freq = sum(count), .groups = "drop")

odp$origin <- odp$origin %>% 
  factor(levels = c("Home", "Office", "Place", "Stop"))

odp$destination <- odp$destination %>% 
  factor(levels = c("Go Home", "Work", "Business", "Transit"))

# plot
ggplot(data = odp,
       aes(axis1 = origin, axis2 = destination, y = freq)) +
  scale_x_discrete(limits = c("Origin", "Needs"), 
                   expand = c(0, 0.2, 0, 0.7)) +
  geom_alluvium(aes(fill = payment)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_manual(values = c("orange", "#00BBE0"),
                    label = c("Cash", "GoPay")) +
  annotate(geom = "text", x = 2.25, y = 190, 
           label = paste("The most frequence trip", 
                         "was from stop (a station",
                         "or other transit node)",
                         "to come back home.",
                         " ",
                         "The second was a trip",
                         "for go home after doing",
                         "some activities.",
                         " ",
                         "There was a strong relation",
                         "between office and transit.",
                         " ",
                         "If I had to pay by cash,",
                         "maybe GoPay balance was 0",
                         "when doing activity outside.",
                         sep = "\n"),
           hjust = "left", vjust = "top",
           size = 3.2, col = "gray50") +
  labs(title = "Effort with GOJEK",
       subtitle = "I even want to come home from home #SelaluAdaJalan #DiRumahAja",
       caption = "Github: akherlan | Data: GOJEK", 
       fill = "Payment") +
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    axis.title = element_blank(),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.75, 0.85),
    legend.direction = "vertical",
    legend.justification = "left",
    # panel.border = element_rect(colour = "black", size = 1, fill = "transparent", linetype = 2),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "gray40"),
    plot.caption = element_text(colour = "gray60"),
    plot.background = element_rect(fill = "white", size = 0)
  )

# save PNG
ggsave("needs.png", path = "figs", dpi = 150, units = "px",
       width = 2*760, height = 2*462)

