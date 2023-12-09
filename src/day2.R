# day 2 --- 

# look for receipts of bagels and coffess
# the contractors have initals JP and purchased in 2017

# read in the data
pacman::p_load(dplyr, stringr)
data_path = here::here("data")
orders = read.csv(file.path(data_path, "noahs-orders.csv"))
orders_items = read.csv(file.path(data_path, "noahs-orders_items.csv"))
products = read.csv(file.path(data_path, "noahs-products.csv"))
customers = read.csv(file.path(data_path, "noahs-customers.csv"))

# chase around the data with our clues 
bagel_skus = products[grepl("bagel", tolower(products$desc)), ]$sku

bagel_order_ids = orders_items |> 
  filter(sku %in% bagel_skus)

bagel_order_customers = orders |> 
  mutate(year = lubridate::year(shipped)) |> 
  filter(orderid %in% bagel_order_ids$orderid, 
         year == 2017) 

customers |> 
  filter(customerid %in% bagel_order_customers$customerid) |> 
  mutate(name = gsub(" jr.| iii| v| ii| iv | i", "", tolower(name)),
         last_name = str_extract(name, "\\w+$"), 
         first_name = str_extract(name, "^\\w+"), 
         initals = paste0(str_sub(first_name, 1, 1), str_sub(last_name, 1, 1))) |> 
  filter(initals == "jp")

# phone number: 332-274-4185
