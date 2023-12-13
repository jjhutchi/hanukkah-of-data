# Hanukkah of Data 2023 -- Day 5
#
# clues: 
# - Woman from Staten Island
# - owns many old cats
# 

pacman::p_load(dplyr)
data_path = here::here("data")
load_data = function (f) read.csv(file.path(data_path, f))

orders       = load_data("noahs-orders.csv")
orders_items = load_data("noahs-orders_items.csv")
products     = load_data("noahs-products.csv")
customers    = load_data("noahs-customers.csv")

old_cat_food = products |> 
  filter(grepl("senior cat", tolower(desc))) |> 
  pull("sku")

cat_order_items = orders_items |> 
  filter(sku %in% old_cat_food, 
         qty > 2) |> 
  pull("orderid")

cat_orders_customers = orders |> 
  filter(orderid %in% cat_order_items) |> 
  pull("customerid")

customers |> 
  filter(customerid %in% cat_orders_customers, 
         grepl("staten island", tolower(citystatezip)))

# phone: 631-507-6048