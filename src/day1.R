# Hanukkah of Data 2023 -- Day 1

pacman::p_load(dplyr, stringr)
data_path = here::here("data")

read.csv(file.path(data_path, "noahs-customers.csv")) |>                # read in the data
  mutate(name = gsub(" jr.| iii| v| ii| iv | i", "", tolower(name))) |> # tidy up the suffixes in names 
  mutate(last_name = str_extract(name, "\\w+$")) |>                     # extract last name
  mutate(name_number = str_replace_all(last_name, "a|b|c", "2") |>      # convert letters to phone T9 phone number 
                       str_replace_all("d|e|f", "3") |> 
                       str_replace_all("g|h|i", "4") |> 
                       str_replace_all("j|k|l", "5") |> 
                       str_replace_all("m|n|o", "6") |> 
                       str_replace_all("p|q|r|s", "7") |> 
                       str_replace_all("t|u|v", "8") |> 
                       str_replace_all("w|x|y|z", "9")) |> 
  mutate(phone_number = gsub("-", "", phone)) |>                       # remove dashes from phone number
  filter(name_number == phone_number)                                  # filter to only those where the last name matches the phone number

