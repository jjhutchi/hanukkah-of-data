# Day 6

# clues: 
# - wood floors (red haring)
# - cousin of Nicole Wilson (red haring)
# - takes subway to visit, so is not in Staten Island? (red haring)
# - buys products from Noah's only on sale - that Noah's loses money i.e. price < wholesale cost

pacman::p_load(dplyr)
data_path = here::here("data")
load_data = function (f) read.csv(file.path(data_path, f))

orders       = load_data("noahs-orders.csv")
orders_items = load_data("noahs-orders_items.csv")
products     = load_data("noahs-products.csv")
customers    = load_data("noahs-customers.csv")


# check which prices are offered below wholesale cost 
orders_below_cost = orders_items |> 
  left_join(products |> select(wholesale_cost, sku), by = "sku") |> 
  mutate(sale = unit_price < wholesale_cost) |> 
  filter(sale) |> 
  pull(orderid)

frugal_customers = orders |> 
  filter(orderid %in% orders_below_cost) |>
  group_by(customerid) |> 
  tally() |>
  arrange(desc(n)) |> 
  pull(customerid)

customers |> 
  filter(customerid == frugal_customers[1])

# phone: 585-838-9161