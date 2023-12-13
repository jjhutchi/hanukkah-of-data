# Hanukkah of Data 2023 -- Day 7
#
# clues: 
# - an item with colour
# - purchased the same item with different colour to the person found in day 6
# - in store purchase
# 

pacman::p_load(dplyr)
data_path = here::here("data")
load_data = function (f) read.csv(file.path(data_path, f))

orders       = load_data("noahs-orders.csv")
orders_items = load_data("noahs-orders_items.csv")
products     = load_data("noahs-products.csv")
customers    = load_data("noahs-customers.csv")

past_customer = customers |> 
  filter(phone == "585-838-9161") |> 
  pull(customerid)

color_purchases = products |> 
  left_join(orders_items |> select(orderid, sku), by = "sku") |>
  left_join(orders |> select(orderid, customerid, ordered, shipped), by = "orderid") |> 
  filter(grepl("col", tolower(sku)), 
         ordered == shipped) 

gf_purchases = color_purchases |> 
  filter(customerid == past_customer)

color_purchases |> 
  mutate(same_item1 = grepl("poster", tolower(desc)), 
         same_item2 = grepl("action", tolower(desc)), 
         time_diff = case_when(
           same_item1 ~ abs(difftime(ordered, gf_purchases$ordered[1])), 
           same_item2 ~ abs(difftime(ordered, gf_purchases$ordered[2]))
         )) |> 
  filter(customerid != past_customer,
         same_item1 | same_item2,
         time_diff < 60 * 10) # say purchased within 10 minutes of eachother 

customers |> filter(customerid == "5783")

# phone: 838-335-7157