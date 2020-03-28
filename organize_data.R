## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(datasets)


## ----message=FALSE-------------------------------------------------------


names_key <- c(
  "Active" = "active",
  "Admin2" = "admin2",
  "Combined_Key" = "combined_key",
  "Confirmed" = "confirmed",
  "Country_Region" = "country_region",
  "Country/Region" = "country_region",
  "Deaths" = "deaths",
  "FIPS" = "FIPS",
  "Last Update" = "update",
  "Last_Update" = "update",
  "Lat" = "latitude",
  "Latitude" = "latitude",
  "Long_" = "longitude",
  "Longitude" = "longitude",
  "Province_State" = "province_state",
  "Province/State" = "province_state",
  "Recovered" = "recovered"
)

col_type_keys <- list(
  "Active" = col_double(),
  "Admin2" = col_character(),
  "Combined_Key" = col_character(),
  "Confirmed" = col_double(),
  "Country_Region" = col_character(),
  "Country/Region" = col_character(),
  "Deaths" = col_double(),
  "FIPS" = col_character(),
  "Last_Update" = col_character(),
  "Last Update" = col_character(),
  "Lat" = col_double(),
  "Latitude" = col_double(),
  "Long_" = col_double(),
  "Longitude" = col_double(),
  "Province_State" = col_character(),
  "Province/State" = col_character(),
  "Recovered" = col_double()
)

read_data <- function(fname) {
  cat(paste0("Reading file: ", fname, "\n"))
  
  con <- file(fname, "r")
  first_line <- readLines(con, n = 1)
  close(con)
  
  header_names <- str_split(first_line, ",")[[1]]
  
  col_types <- col_type_keys[header_names]
  names(col_types) <- unlist(names_key[names(col_types)])
  
  data <- read_csv(fname, 
                   col_names = names_key[header_names], 
                   col_types = do.call(cols, col_types),
                   skip = 1)
  
  if (all(str_detect(data$update, "^[:digit:][:digit:]?/[:digit:][:digit:]?/[:digit:]{2} [:digit:][:digit:]?:[:digit:]{2}$"))) {
    data$update <- mdy_hm(data$update)
  } else if (all(str_detect(data$update, "^[:digit:][:digit:]?/[:digit:][:digit:]?/[:digit:]{4} [:digit:][:digit:]?:[:digit:]{2}$"))) {
    data$update <- mdy_hm(data$update)
  } else {
    data$update <- as_datetime(data$update)
  }
  
  return(data)
}


## ------------------------------------------------------------------------
data_dir <- "../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/"

data <- tibble(reports = dir(data_dir, ".*\\.csv", full.names = TRUE))

data <- data %>%
  mutate(data = map(reports, read_data)) %>%
  unnest(data)

data <- data %>%
  mutate(update = case_when(update == ymd_hms("2020-03-11 20:00:00") ~ update + days(2), 
                            TRUE ~ update),
         date_only = as_date(update)) %>%
  group_by(province_state, date_only) %>%
  summarize(confrimed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>%
  ungroup()

state_data <- data %>% 
  filter(province_state %in% state.name)

write_csv(state_data, "./data/state_date.csv")
write_csv(data, "./data/all_daily_reports.csv")


## ------------------------------------------------------------------------
read_global_data <- function(fname) {
  data <- read_csv(fname) %>%
    rename(province_state = `Province/State`,
           country_region = `Country/Region`) %>%
    select(-Lat, -Long) %>%
    pivot_longer(c(-province_state, -country_region), names_to = "date") %>%
    mutate(date = mdy(date)) %>%
    group_by(country_region, date) %>%
    summarize(value = sum(value))
}


global_confirmed <- read_global_data("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  rename(confirmed = value)

global_recovered <- read_global_data("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>%
  rename(recovered = value)

global_deaths <- read_global_data("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  rename(deaths = value)

global_data <- global_confirmed %>%
  left_join(global_recovered) %>%
  left_join(global_deaths)

write_csv(global_data, "./data/global_data.csv")

