# Title:           Data Wrangling
# Author:          Andi Herlan
# Email:           andi.herlan@protonmail.com
# Data Used:       travel.csv
# Packages Used:   stringr, lubridate, dplyr, tidyr
# Output File:     -
# Data Output:     gojek.rds

# clearing
rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# invoice mail from GOJEK
travel <- read.csv("data/travel.csv")
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

# order id
gojek$order_id <- content %>% 
  str_extract("RB-.+\\d+") %>% 
  str_squish()

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
colSums(gojek[,c(4:9)])

# sort by datetime
gojek <- gojek %>% arrange(datetime)

# check: double invoice
n_distinct(select(gojek, order_id)) == nrow(gojek)

# handle double record
# ???

# save to RDS
saveRDS(gojek, file = "output/gojek.rds")

