# Title:           Cost
# Author:          Andi Herlan
# Email:           andi.herlan@protonmail.com
# Data Used:       gojek.rds
# Packages Used:   dplyr, tidyr, stringr, ggplot2
# Source:          colours.R
# Output File:     cost.png
# Data Output:     -
# Reference:       https://stackoverflow.com/questions/46597278/


# clear environment
rm(list = ls())

# import colour definition
source("colours.R")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# main data
gojek <- readRDS("output/gojek.rds") %>% 
  select(datetime, payment, price, discount, fee, paid)

# data for cost (rupiah in thousands)
cost <- gojek %>% 
  select(-datetime) %>% 
  pivot_longer(cols = 2:5, names_to = "type", values_to = "rupiah") %>% 
  group_by(payment, type) %>% 
  summarise(rupiah = sum(rupiah/1000), .groups = "drop") %>% 
  mutate(percent = round((rupiah/sum(rupiah))*100, 1),
         cat = ifelse(type %in% c("discount", "paid"), 0, 1),
         type = str_to_title(type),
         payment = ifelse(payment == "Gopay", "GoPay", payment))

# all discount
disc_total <- cost %>% group_by(type) %>% summarise(sum = sum(rupiah))

# discount, fee, price + fee in app
godisc <- cost$rupiah[5]
gofee <- cost$rupiah[6] 
goprice <- round((cost$rupiah[8] + gofee)/1000, 2) # in millions IDR

# percent cash expenses between cash and gopay
paid_percent <- cost[cost$type == "Paid",] %>% 
  mutate(percent = round((rupiah/sum(rupiah))*100, 2))

# percent cost (price + fee) in both cash and gopay
cost_percent <- cost %>% 
  filter(cat == 1) %>% 
  mutate(percent = round((rupiah/sum(rupiah))*100, 2))

# transactions done in app and cash
n_trans <- gojek %>% 
  select(paid, payment) %>% 
  group_by(payment) %>% 
  summarise(n_trans = n(), .groups = "drop") %>% 
  mutate(percent = round((n_trans/sum(n_trans))*100, 2))

# text postion transaction done in app
y_trans <- (cost$rupiah[5] + cost$rupiah[7])*2/3

# cost_growth <- gojek %>% 
#   pivot_longer(cols = 3:6, names_to = "type", values_to = "rupiah") %>% 
#   group_by(type) %>% 
#   summarise(datetime = datetime, 
#             cumsum = cumsum(rupiah), .groups = "drop") %>% 
#   arrange(datetime)
# 
# ggplot(cost_growth) +
#   geom_line(aes(datetime, cumsum, colour = type), show.legend = F)

# plot for cost
ggplot() + 
  # bar price + fee
  geom_bar(data = filter(cost, cat == 1), 
           aes(x = cat, y = rupiah, fill = type), 
           stat = "identity", position = "stack") + 
  # bar paid + discount
  geom_bar(data = filter(cost, cat == 0), 
           aes(x = cat, y = rupiah, fill = type), 
           stat = "identity", position = "stack") + 
  # text paid percent cash
  geom_text(data = filter(cost, cat == 1 & payment == "Cash"), 
            aes(x = 0, y = paid_percent$rupiah[1]/2, 
                label = paste0(paid_percent$percent[1], "%\nof expenses")),
            # size = 4, colour = "#B3E7F3") +
            size = 4, colour = "#DEF5FD") +
  # text price of all cost percent cash
  geom_text(data = filter(cost, cat == 1 & payment == "Cash"), 
            aes(x = 1, y = cost_percent$rupiah[2]/2, 
                label = paste0(cost_percent$percent[2], "%\nof costs")),
            size = 4, colour = "#005400") +
  # text paid percent gopay
  geom_text(data = filter(cost, cat == 1 & payment == "GoPay"), 
            aes(x = 0, y = paid_percent$rupiah[2]/2, 
                label = paste0(paid_percent$percent[2], "%\nof expenses\n(IDR ",
                               paid_percent$rupiah[2], ",000)")), 
            # size = 4, colour = "#B3E7F3") +
            size = 4, colour = "#DEF5FD") +
  # text discount gopay
  geom_text(data = filter(cost, cat == 1 & payment == "GoPay"), 
            aes(x = 0, y = cost$rupiah[7] + cost$rupiah[5]/2, 
                label = paste0("IDR ", godisc, ",000")), 
            # size = 4, colour = "#00BBE0") +
            size = 4, colour = "#388192") +
  # text price gopay
  geom_text(data = filter(cost, cat == 1 & payment == "GoPay"), 
            aes(x = 1, y = cost$rupiah[8]/2, 
                label = paste0("Total costs\n IDR ", goprice, 
                               " millions\n(include IDR ", gofee, 
                               ",000\nof additional app fee,\nparking, toll, etc.)\nwere paid inside app\nwith IDR ",
                               godisc, ",000\ndiscount+voucher")),
            size = 3.4, colour = "#005400") +
  # percent transaction done in app
  geom_text(data = filter(cost, cat == 1 & payment == "Cash"), 
            aes(x = 0.5, y = y_trans + 150, label = paste0(n_trans$percent[2], "%")), 
            size = 12, colour = "gray60") +
  geom_text(data = filter(cost, cat == 1 & payment == "Cash"), 
            aes(x = 0.5, y = y_trans - 365, 
                label = "of transactions\nwere completed\nwithin the app"), 
            size = 5, colour = "gray60") +
  # graph's labels
  labs(x = NULL, y = "Thousands of IDR", fill = NULL,
       title = "Expences with GOJEK",
       subtitle = "Cost for movement (Cash Vs. GoPay)",
       caption = "CC BY-SA | Github: akherlan | Data: GOJEK") +
  # styling
  facet_wrap(~payment, strip.position = "bottom") +
  # scale_fill_manual(values = c("#B3E7F3", "#000000", "#00BBE0", "#00AE00")) +
  scale_fill_manual(values = c("lightblue", "black", gc_blue, gc_green50)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8, colour = "gray50"),
    axis.title = element_text(colour = "gray50"),
    plot.subtitle = element_text(colour = "gray40"),
    plot.caption = element_text(colour = "gray60"),
    plot.title.position = "plot",
    strip.text = element_text(face = "bold", size = 11, colour = "gray40"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(0, "cm"),
    legend.position = c(0.25, 0.85),
    legend.direction = "horizontal",
    legend.justification = "center",
    plot.background = element_rect(fill = "white", size = 0)
  )

# save PNG
ggsave("cost.png", path = "figs", dpi = 150, units = "px",
       width = 2*600, height = 2*469)
