# Hannukkah of Data speedrun 2023 

pacman::p_load(dplyr, stringr)
data_path = here::here("data/speedrun-data")
load_data = function (f) read.csv(file.path(data_path, f))
orders       = load_data("noahs-orders.csv")
orders_items = load_data("noahs-orders_items.csv")
products     = load_data("noahs-products.csv")
customers    = load_data("noahs-customers.csv")


# Day 1 ---- 

day1_phone = read.csv(file.path(data_path, "noahs-customers.csv")) |>
  mutate(name = gsub(" jr.| iii| v| ii| iv | i", "", tolower(name)),
         last_name = str_extract(name, "\\w+$"),
         name_number = str_replace_all(last_name, "a|b|c", "2") |> 
           str_replace_all("d|e|f", "3") |>
           str_replace_all("g|h|i", "4") |>
           str_replace_all("j|k|l", "5") |>
           str_replace_all("m|n|o", "6") |>
           str_replace_all("p|q|r|s", "7") |>
           str_replace_all("t|u|v", "8") |>
           str_replace_all("w|x|y|z", "9"),
         phone_number = gsub("-", "", phone)) |>
  filter(name_number == phone_number) |> 
  pull(phone)

sprintf("Day 1: %s", day1_phone)


# Day 2 ---- 

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

day2_phone = customers |> 
  filter(customerid %in% bagel_order_customers) |> 
  mutate(name = gsub(" jr.| iii| v| ii| iv | i", "", tolower(name)),
         last_name = str_extract(name, "\\w+$"), 
         first_name = str_extract(name, "^\\w+"), 
         initals = paste0(str_sub(first_name, 1, 1), str_sub(last_name, 1, 1))) |> 
  filter(initals == "ds") |> 
  pull(phone)

sprintf("Day 2: %s", day2_phone[1])


## Day 3 ---

goat_years = c(1931, 1943, 1955, 1967, 1979, 1991, 2003, 2015)

neighborhood = customers |> 
  filter(phone == day2_phone[1]) |> 
  pull("citystatezip")

day3_phone = customers |> 
  mutate(year = lubridate::year(birthdate), 
         month = lubridate::month(birthdate), 
         day = lubridate::day(birthdate)) |> 
  filter(year %in% goat_years, 
         month == 9 & day >= 23 | month == 10 & day <= 22, 
         citystatezip == neighborhood) |> 
  pull(phone)

sprintf("Day 3: %s", day3_phone)


# Day 4 ---- 

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

day4_phone = customers |> 
  filter(customerid %in% bakery_orders_customers$customerid) |> 
  pull(phone)

sprintf("Day 4: %s", day4_phone[2])


# Day 5 ----

old_cat_food = products |> 
  filter(grepl("senior cat", tolower(desc))) |> 
  pull("sku")

cat_order_items = orders_items |> 
  filter(sku %in% old_cat_food, 
         qty == 10) |> 
  pull("orderid")

day5_phone = orders |> 
  filter(orderid %in% cat_order_items) |> 
  left_join(customers, by = "customerid") |> 
  select(customerid, phone) |> 
  unique() |> 
  pull(phone)

sprintf("Day 5: %s", day5_phone)


# Day 6 --- 

orders_below_cost = orders_items |> 
  left_join(products |> select(wholesale_cost, sku), 
            by = "sku") |> 
  mutate(sale = unit_price < wholesale_cost) |> 
  filter(sale) |> 
  pull(orderid)

day6_phone = orders |> 
  filter(orderid %in% orders_below_cost) |>
  group_by(customerid) |> 
  summarise(num_purchases = n()) |>
  arrange(desc(num_purchases)) |>
  left_join(customers |> select(customerid, name, phone), by = "customerid") |> 
  head() |> 
  pull(phone)

sprintf("Day 6: %s", day6_phone[1])


# Day 7 ---- 

past_customer = customers |> 
  filter(phone == day6_phone[1]) |> 
  pull(customerid)

color_purchases = products |> 
  left_join(orders_items |> select(orderid, sku), by = "sku") |>
  left_join(orders |> select(orderid, customerid, ordered, shipped), by = "orderid") |> 
  filter(grepl("hom", tolower(sku)), 
         ordered == shipped) |> 
  mutate(item = str_extract(desc, "\\w+\\s\\w+"), 
         colour = str_extract(desc, "\\(\\w+\\)"), 
         colour = gsub("\\(|\\)", "", colour))

gf_purchases = color_purchases |> 
  filter(customerid == past_customer, 
         grepl("\\(", desc)) 
  

search_for_purchases = function(dat) {
  
  gf_item = dat$item
  gf_colour = dat$colour
  gf_time = dat$ordered
  
  out = color_purchases |> 
    filter(item == gf_item & colour != gf_colour) |> 
    mutate(time_diff = abs(difftime(ordered, gf_time, units = "mins"))) |> 
    filter(time_diff < 10)
  
  return(out)
}

out_list = list()
for(i in seq_along(gf_purchases$item)) {
  out = search_for_purchases(gf_purchases[i, ])
  out_list[[i]] = out
}

day7_phone = out_list |> 
  bind_rows() |> 
  select(customerid) |> 
  left_join(customers |> select(customerid, name, phone), by = "customerid")

sprintf("Day 7: %s", day7_phone$phone)


# Day 8 --- 

day8_phone = products |> 
  filter(grepl("col", tolower(sku))) |> 
  left_join(orders_items |> select(orderid, sku), by = "sku") |>
  left_join(orders |> select(orderid, customerid), by = "orderid") |> 
  group_by(customerid) |> 
  tally() |> 
  left_join(customers |> select(customerid, name, phone), by = "customerid") |>
  arrange(desc(n)) 

sprintf("Day 8: %s", day8_phone[1, ]$phone)
