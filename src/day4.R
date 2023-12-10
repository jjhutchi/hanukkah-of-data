# Day 4

# clues: 
# - pastries from Noah's, purchased before 5am

pacman::p_load(dplyr, stringr)
data_path = here::here("data")
load_data = function (f) read.csv(file.path(data_path, f))

orders       = load_data("noahs-orders.csv")
orders_items = load_data("noahs-orders_items.csv")
products     = load_data("noahs-products.csv")
customers    = load_data("noahs-customers.csv")

bakery_skus = products |> 
  filter(grepl("bky", tolower(sku))) |> 
  filter(!grepl("bagel", tolower(desc)))

bakery_order_items = orders_items |> 
  filter(sku %in% bakery_skus$sku, 
         qty > 1) |> 
  pull("orderid")

bakery_orders_customers = orders |> 
  filter(orderid %in% bakery_order_items, 
         lubridate::hour(shipped) < 5)

customers |> 
  filter(customerid %in% bakery_orders_customers$customerid)

# take customer who shows up multiple times getting pasteries before 5am
# also is the order closest to the initial year - 2017

# phone: 607-231-3605
