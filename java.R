# Title:           Java Island Boundary Generation
# Author:          Andi Herlan
# Email:           andi.herlan@protonmail.com
# Data Used:       https://www.naturalearthdata.com/downloads/ (using pkg)
# Packages Used:   rnaturalearth, rnaturalearthdata, sf, dplyr
# Output File:     -
# Data Output:     java.rds

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)

idn <- ne_countries(returnclass = "sf", 
                    country = "indonesia",
                    scale = 50)

print(t(as.data.frame(idn)))

java <- st_cast(idn, "POLYGON") %>% 
  mutate(id = 1:n()) %>% 
  select(id, name) %>% 
  filter(id %in% c(31, 38))

island <- tribble(~id, ~island, 31, "Madura", 38, "Jawa") %>% 
  mutate(scale = 50, source = "Natural Earth")

java <- java %>% 
  left_join(island, by = "id") %>% 
  select(-id) %>% 
  rename("country" = "name")

saveRDS(java, "output/java.rds")
