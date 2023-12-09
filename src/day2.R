# Day 2

# clues: 
# - look for receipts of bagels and coffee
# - the contractors have initials JP, and purchased in 2017

# comments: dplyr::pull() is a nice way to get a column of a dataframe into its own vector

pacman::p_load(dplyr, stringr)
data_path = here::here("data")
load_data = function (f) read.csv(file.path(data_path, f))
orders = load_data("noahs-orders.csv")
orders_items = load_data("noahs-orders_items.csv")
products = load_data("noahs-products.csv")
customers = load_data("noahs-customers.csv")

bagel_skus = products |> 
  filter(grepl("bagel", tolower(desc))) |> 
  pull(sku)

bagel_order_ids = orders_items |> 
  filter(sku %in% bagel_skus)

bagel_order_customers = orders |> 
  mutate(year = lubridate::year(shipped)) |> 
  filter(orderid %in% bagel_order_ids$orderid, 
         year == 2017) |> 
  pull(customerid)

customers |> 
  filter(customerid %in% bagel_order_customers) |> 
  mutate(name = gsub(" jr.| iii| v| ii| iv | i", "", tolower(name)),
         last_name = str_extract(name, "\\w+$"), 
         first_name = str_extract(name, "^\\w+"), 
         initals = paste0(str_sub(first_name, 1, 1), str_sub(last_name, 1, 1))) |> 
  filter(initals == "jp")

# phone number: 332-274-4185