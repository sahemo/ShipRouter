library(tidyverse)
library(arrow)



df <- read_csv_arrow("data/ships.csv", as_data_frame = FALSE)

df %>% 
  head() %>% 
  collect() %>% 
  glimpse()


df %>% 
  summarise(Lon = mean(LON, na.rm = TRUE), Lat = mean(LAT, na.rm = TRUE)) %>% 
  collect()

df %>% 
  count(ship_type) %>% 
  collect() %>% 
  arrange(ship_type) %>% 
  janitor::adorn_totals()



df <- read_csv_arrow("data/ships.csv", as_data_frame = TRUE) %>% 
  arrange(SHIP_ID, DATETIME) %>% 
  group_by(SHIP_ID) %>% 
  mutate(LON1 = lag(LON), LAT1 = lag(LAT)) %>% 
  mutate(distance = get_geo_distance(long1 = `LON`, lat1 = `LAT`,
                                     long2 = LON1, lat2 = LAT1, units = "meters")) %>% 
  ungroup() %>% 
  mutate(contents_pop = showShipPopup(.))

df %>% 
  write_csv_arrow("data/ship_arrow.csv")


x1 <- function(){
  "Tanker"
}

df %>% 
  filter(ship_type == "Cargo") %>% 
  arrange(desc(distance), desc(DATETIME)) %>% 
  select(SHIPNAME, LAT, LON, LAT1, LON1, distance, contents_pop) %>% 
  head() %>% 
  collect() %>% 
  slice_max(order_by = "distance", n = 1)
