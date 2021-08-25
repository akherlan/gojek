# Title:           Travel with GOJEK
# Author:          Andi Herlan
# Email:           andi.herlan@protonmail.com
# Purpose:         I don't know
# File Name:       travel.R
# Data Used:       travel.csv
# Packages Used:   stringr, lubridate, ggplot2, dplyr, tidyr
# Output File:     clock.png, distance.png, cost.png
# Data Output:     gojek.rds
# Machine:         RStudio Server on Docker (hatmatrix/blog:base), 
#                  RStudio Desktop on Ubuntu 20.04

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# Wrangling -----

# invoice mail from GOJEK
travel <- read.csv("travel.csv")
content <- travel$body
gojek <- travel %>% as_tibble()

# datetime order == datetime email
gojek$datetime <- as_datetime(gojek$datetime, 
                              format = "%m/%d/%Y %H:%M:%S", 
                              tz = "Asia/Jakarta")

# ride or car
gojek$vehicle <- content %>% 
  str_extract("G[O|o]-?\\w{3,4}") %>% 
  str_to_lower() %>% 
  str_remove("go-?") %>% 
  str_to_title()

# distance
gojek$distance <- content %>% 
  str_extract("[D|J]\\w+.+\\skm") %>% 
  str_extract("\\d+(\\.\\d+)?") %>% 
  as.numeric()

# duration
dur <- content %>% 
  str_extract("[T|W]\\w+\\s\\w+\\s(:?\\d{2}){3}") %>% 
  str_extract("(:?\\d{2}){3}") %>% 
  as_tibble() %>% 
  separate(value, sep = ":", into = c("h", "m", "s"))

dur <- sapply(dur, as.numeric) %>% as_tibble()
dur$duration <- dur$h*60*60 + dur$m*60 + dur$s
gojek$duration <- duration(dur$duration)

# price
gojek$price <- content %>% 
  str_extract("\\w+\\s\\(.+\\)\\sRp\\d+\\.\\d+") %>% 
  str_extract("Rp\\d+(\\.\\d+)?") %>% 
  str_remove("Rp") %>% 
  str_remove("\\.") %>% 
  as.numeric()

# discount
gojek$discount <- content %>% 
  str_extract("Diskon.+") %>% 
  str_remove("Diskon.+Rp") %>% 
  str_remove("\\.") %>% 
  as.numeric()

# voucher
gojek$voucher <- content %>%
  str_extract("Voucher.+") %>% 
  str_remove("Voucher.+Rp") %>% 
  str_remove("\\.") %>% 
  as.numeric()

# app service fee
gojek$fee <- content %>% 
  str_extract("Biaya jasa aplikasi.+\\d{3}") %>% 
  str_remove("^B.+Rp") %>% 
  str_remove("\\.") %>% 
  as.numeric()

# additional fee
gojek$additional <- content %>% 
  str_extract("Pendapatan tambahan.+") %>% 
  str_remove("^P.+Rp") %>% 
  str_remove("\\.") %>% 
  as.numeric()

# toll and parking
gojek$toll <- content %>% 
  str_extract("Ongkos tol/parkir.+") %>% 
  str_remove("^O.+Rp") %>% 
  str_remove("\\.") %>% 
  as.numeric()

# total payment
gojek$paid <- content %>% 
  str_extract("TOTAL.+") %>% 
  str_extract("Rp\\d+(\\.\\d+)?") %>% 
  str_remove("Rp") %>% 
  str_remove("\\.") %>% 
  as.numeric()

# payment method
gojek$payment <- content %>% 
  str_extract("G[O|o]-?P.+") %>% 
  str_remove("-") %>% 
  str_to_title() %>% 
  str_trim()

gojek <- gojek %>% 
  mutate(payment = ifelse(is.na(payment), "Cash", payment))

# pickup
gojek$pickup <- content %>% 
  str_squish() %>% 
  str_extract("image: pickup.+image: drop") %>% 
  str_remove_all("image:") %>% 
  str_remove("pickup") %>% 
  str_remove("drop") %>% 
  str_remove("\\]") %>% 
  str_remove("\\[") %>% 
  str_remove("  ") %>% 
  str_remove("Penjemputan\\s?\\*\\s?\\•\\s?\\d{2}:\\d{2}\\s?\\*") %>% 
  str_remove("pick up\\s?\\*\\s?\\•\\s?\\d{2}:\\d{2}\\s?\\*") %>% 
  str_trim()

# destination
gojek$destination <- content %>% 
  str_squish() %>% 
  str_extract("image: drop.+image: Driver Image") %>% 
  str_remove_all("[I|i]mage:?") %>% 
  str_remove("drop") %>% 
  str_remove("\\]") %>% 
  str_remove("\\[") %>% 
  str_remove("Driver") %>% 
  str_remove("   ") %>% 
  str_remove("Tujuan\\s?\\*\\s?\\•\\s?\\d{2}:\\d{2}\\s?\\*") %>% 
  str_remove("destination\\s?\\*\\s?\\•\\s?\\d{2}:\\d{2}\\s?\\*") %>% 
  str_trim()

# driver
gojek$driver <- content %>% 
  str_extract("Driver Image\\]\\s\n\n.+") %>%
  str_remove_all("Driver Image] \n\n") %>% 
  str_remove("(Your driver)?(Driver Anda)?") %>% 
  str_trim()

gojek <- gojek %>% select(-2:-4)

# add voucher to discount; gather fees; NA == 0
gojek <- gojek %>% 
  mutate(discount = ifelse(is.na(discount), 0, discount),
         voucher = ifelse(is.na(voucher), 0, voucher),
         discount = discount + voucher,
         fee = ifelse(is.na(fee), 0, fee),
         additional = ifelse(is.na(additional), 0, additional),
         toll = ifelse(is.na(toll), 0, toll),
         fee = fee + additional + toll) %>% 
  select(-voucher, -additional, -toll)


# check: price - discount + fee = paid
gojek$price - gojek$discount + gojek$fee == gojek$paid

# total consumption
colSums(gojek[,c(3:8)])

# save to RDS
saveRDS(gojek, file = "gojek.rds")

# Clock -----

# data for clocking
clock <- gojek %>% 
  summarise(datetime = datetime) %>% 
  mutate(y = as.numeric(pm(datetime)),
         x = hour(datetime),
         x = ifelse(y == 1, x-12L, x)*60L,
         x = x + minute(datetime),
         col = gojek$vehicle)

# plot for clock
ggplot(clock) +
  # am / pm
  annotate(geom = "text", x = 0, y = 0.5, label = "AM",
           size = 4, colour = "gray40", alpha = 0.3) +
  annotate(geom = "text", x = 0, y = 1.5, label = "PM",
           size = 4, colour = "gray40", alpha = 0.3) +
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
  labs(title = "Travel with GOJEK", x = NULL, y = NULL,
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
    plot.caption = element_text(colour = "gray60")
  )

# Ref: https://www.wjakethompson.com/post/2018-11-27-ggclock/

# Distance -----

# data for distance 
dstncs <- gojek %>% 
  select(vehicle, distance, duration)

comeback <- gojek %>% 
  arrange(distance) %>% 
  tail(1)

wrongride <- gojek %>% 
  arrange(duration) %>% 
  head(3)

# plot for distance
ggplot(dstncs) +
  # total distance
  annotate(geom = "text", x = 2, y = 28, size = 3, hjust = "left",
           label = paste("Total distance:\n", sum(gojek$distance),
                         "km", sept = " "),
           colour = "gray50") +
  # total duration
  annotate(geom = "text", x = 56, y = 2, size = 3, hjust = "right",
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
  scale_x_continuous(limits = c(0, 90), breaks = c(seq(0, 15, 5), 30, 45, 60, 90)) +
  scale_colour_manual(values = c("#000000", "#00AA13")) +
  # main labels
  labs(title = "Travel with GOJEK", 
       subtitle = paste0("Equivalent to Jakarta-Surabaya (almost) from ",
                         nrow(gojek), "x travels"),
       caption = "Github: akherlan | Data: GOJEK",
       x = "Duration (minutes)", y = "Distance (km)",
       colour = "GO") +
  # styling
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    legend.position = c(0.87, 0.28),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(colour = "gray40"),
    plot.caption = element_text(colour = "gray60"),
    axis.title = element_text(colour = "gray50"),
    axis.text = element_text(colour = "gray50")
  )

# Ref: https://ggplot2-book.org/annotations.html

# Costs -----

# data for costs
cost <- gojek %>% 
  select(payment, price, discount, fee, paid) %>% 
  pivot_longer(cols = 2:5, names_to = "type", values_to = "rupiah") %>% 
  group_by(payment, type) %>% 
  summarise(rupiah = sum(rupiah/1000), .groups = "drop") %>% 
  mutate(percent = round((rupiah/sum(rupiah))*100, 1),
         cat = ifelse(type %in% c("discount", "paid"), 0, 1),
         type = str_to_title(type),
         payment = ifelse(payment == "Gopay", "GoPay", payment))

# all discount
disc_total <- cost %>% group_by(type) %>% summarise(sum = sum(rupiah))

# discount in app
godisc <- cost$rupiah[5]

# price + fee in app
fee <- cost$rupiah[6] # thousands
goprice <- round((cost$rupiah[8] + fee)/1000, 2) # millions

# percent cash expenses
paid_percent <- cost[cost$type == "Paid",] %>% 
  mutate(percent = round((rupiah/sum(rupiah))*100, 2))

# percent cost (price + fee in both cahs and gopay)
cost_percent <- cost %>% 
  filter(cat == 1) %>% 
  mutate(percent = round((rupiah/sum(rupiah))*100, 2))

# percent transactions done in app
n_trans <- gojek %>% 
  select(paid, payment) %>% 
  group_by(payment) %>% 
  summarise(n_trans = n(), .groups = "drop") %>% 
  mutate(percent = round((n_trans/sum(n_trans))*100, 2))

# plot for costs
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
            size = 4, colour = "#B3E7F3") +
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
            size = 4, colour = "#B3E7F3") +
  # text discount gopay
  geom_text(data = filter(cost, cat == 1 & payment == "GoPay"), 
            aes(x = 0, y = cost$rupiah[7] + cost$rupiah[5]/2, 
                label = paste0("IDR ", godisc, ",000")), 
            size = 4, colour = "#00BBE0") +
  # text price gopay
  geom_text(data = filter(cost, cat == 1 & payment == "GoPay"), 
            aes(x = 1, y = cost$rupiah[8]/2, 
                label = paste0("Total costs\n IDR ", goprice, 
                               " millions\n(include IDR ", fee, 
                               ",000\nof additional app fee,\nparking, toll, etc.)\nwere paid inside app\nwith IDR ",
                               godisc, ",000\ndiscount+voucher")),
            size = 3.4, colour = "#005400") +
  # percent transaction done in app
  geom_text(data = filter(cost, cat == 1 & payment == "Cash"), 
            aes(x = 0.5, y = 1380, label = paste0(n_trans$percent[2], "%")), 
            size = 12, colour = "gray60") +
  geom_text(data = filter(cost, cat == 1 & payment == "Cash"), 
            aes(x = 0.5, y = 1070, 
                label = "of transactions\nwere completed\nwithin the app"), 
            size = 5, colour = "gray60") +
  # graph's labels
  labs(x = NULL, y = "Thousands of IDR", fill = NULL,
       title = "Travel with GOJEK",
       subtitle = "Expences for movement (Cash Vs. GoPay)",
       caption = "Github: akherlan | Data: GOJEK") +
  # styling
  facet_wrap(~payment, strip.position = "bottom") +
  scale_fill_manual(values = c("#B3E7F3", "#000000", "#00BBE0", "#00AE00")) +
  theme_minimal() +
  theme(
    text = element_text(family = "Sans"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8, colour = "gray50"),
    axis.title = element_text(colour = "gray50"),
    plot.subtitle = element_text(colour = "gray40"),
    plot.caption = element_text(colour = "gray60"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(0, "cm"),
    legend.position = "top",
    legend.justification = c(0, 1)
  )

# Ref: https://stackoverflow.com/questions/46597278/

