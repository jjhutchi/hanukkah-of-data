# Hanukkah of Data 2023 -- Day 8
#
# clues: 
# - owns an entire set of Noah's collectibles

pacman::p_load(dplyr)
data_path = here::here("data")
load_data = function (f) read.csv(file.path(data_path, f))

orders       = load_data("noahs-orders.csv")
orders_items = load_data("noahs-orders_items.csv")
products     = load_data("noahs-products.csv")
customers    = load_data("noahs-customers.csv")

products |> 
  filter(grepl("col", tolower(sku))) |> 
  left_join(orders_items |> select(orderid, sku), by = "sku") |>
  left_join(orders |> select(orderid, customerid), by = "orderid") |> 
  group_by(customerid) |> 
  tally() |> 
  left_join(customers |> select(customerid, name, phone), by = "customerid") |>
  arrange(desc(n))

# phone: 212-547-3518