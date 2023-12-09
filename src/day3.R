# Day 3

# clues: 
# - cancer (June 21-July 22) born in the year('63, '75, '87, '99) of the rabbit
# - had a bag from Noahs,
# - had a hat with a spider on it (red haring)
# - same neighborhood as past guy (phone: 332-274-4185)

pacman::p_load(dplyr, stringr)
data_path = here::here("data")
load_data = function (f) read.csv(file.path(data_path, f))

orders       = load_data("noahs-orders.csv")
orders_items = load_data("noahs-orders_items.csv")s
products     = load_data("noahs-products.csv")
customers    = load_data("noahs-customers.csv")

rabbit_years = c(1939, 1951, 1963, 1975, 1987, 1999)

neighborhood = customers |> 
  filter(phone == "332-274-4185") |> 
  pull("citystatezip")

customers |> 
  mutate(year = lubridate::year(birthdate), 
         month = lubridate::month(birthdate), 
         day = lubridate::day(birthdate)) |> 
  filter(year %in% rabbit_years, 
         month == 6 & day >= 21 | month == 7 & day <= 22, 
         citystatezip == neighborhood)

# phone: 917-288-9635