library(tidyverse)
library(lubridate)
library(tigris)


# Load data polygons -----------------------------------------------

cty <- counties(cb = TRUE, resolution = "20m")

url_confirm <- 'https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv'
cv_us_confirm <- read_csv(url(url_confirm))
url_deaths <- 'https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv'
cv_us_deaths <- read_csv(url(url_deaths))

cv_us_deaths <- cv_us_deaths %>% select(-ncol(cv_us_deaths))

# names(cv_us_confirm)[5:ncol(cv_us_confirm)] <- paste0(names(cv_us_confirm)[5:ncol(cv_us_confirm)], '20')
# names(cv_us_deaths)[5:ncol(cv_us_deaths)] <- paste0(names(cv_us_deaths)[5:ncol(cv_us_deaths)], '20')

cv_us_confirm$type <- "CONFIRM"
cv_us_deaths$type <- "DEATH"

# cv_us_confirm <- cv_us_confirm %>% select(-(ncol(cv_us_confirm)-1))
names(cv_us_confirm) <- names(cv_us_deaths)

cv_us <- bind_rows(cv_us_confirm, cv_us_deaths)
# cv_us <- cv_us %>% select(-(ncol(cv_us)-1))

today <- as.Date(Sys.Date())
string_today <- paste(str_sub(today,6,7), str_sub(today,9,10), year(today), sep = '-')

url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/', string_today, '.csv')
cv_us_today <- NULL
tryCatch(cv_us_today <- read_csv(url(url)), 
         error = function(e) "error")

cv_us <- cv_us %>%
  mutate(countyFIPS = str_pad(countyFIPS,5,'left', '0')) %>%
  distinct(countyFIPS, type, .keep_all = T) %>% # Distinct county codes
  mutate(countyFIPS = ifelse(countyFIPS == '46031', '45031', countyFIPS)) # Darlington county FIPS code fixed

if(!is.null(cv_us_today)){
  cv_us_today <- cv_us_today %>%
    rename(countyFIPS = FIPS,
           CONFIRM = Confirmed,
           DEATH = Deaths,
           last_update = Last_Update) %>%
    select(countyFIPS, last_update, CONFIRM, DEATH) %>%
    distinct(countyFIPS, .keep_all = T) %>%
    gather(type, value, 3:4)
}

## combine historical and today

if(!is.null(cv_us_today)){
  cv_us_today <- cv_us_today %>%
    left_join(cv_us, by = c('countyFIPS', 'type'))
} else{
  cv_us_today <- cv_us
}

# Remove duplicating counties 

cv_us_today <- cv_us_today %>%
  distinct(countyFIPS, type, .keep_all = T)

cty_data <- cty@data
cty_data <- cty_data %>%
  mutate(countyFIPS = paste0(STATEFP, COUNTYFP)) %>%
  left_join(filter(cv_us_today, type == 'CONFIRM'), by = c('countyFIPS' = 'countyFIPS')) %>%
  mutate_if(is.numeric, function(x) replace_na(x, 0)) # Replace all NA to zero

cty_data <- cty_data %>%
  select(1:12,14:ncol(cty_data),13)

col_index <- grep('^[0-9]|^value',names(cty_data))

cty_data_gather <- cty_data %>%
  tidyr::gather(date,value,all_of(col_index))

cty_data_gather <- cty_data_gather %>%
  mutate(date = ifelse(date == 'value', paste(month(today), day(today), year(today), sep = '/'), date))

cty_data_gather <- cty_data_gather %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'))

cty@data <- cty_data

# Death 

cv_death_gather <- cv_us_today %>%
  select(1:3,5:ncol(cv_us_today),4)

col_index <- grep('^[0-9]|^value',names(cv_death_gather))

cv_death_gather <- cv_death_gather %>%
  filter(type == 'DEATH') %>%
  tidyr::gather(date,value,col_index) 

cv_death_gather <- cv_death_gather %>%
  mutate(date = ifelse(date == 'value', paste(month(today), day(today), year(today), sep = '/'), date))

cv_death_gather <- cv_death_gather %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'))

# Palette for color coding
bins <- c(-Inf, 0, 10, 100, 500, Inf) # create color bins
pal <- colorBin("Reds", domain = cty_data[, ncol(cty_data)-1], bins = bins)

# State name append

state_name <- cty_data_gather %>%
  distinct(STATEFP, State) %>%
  filter(!is.na(State))

cty@data <- cty@data %>%
  left_join(state_name, by = 'STATEFP') %>%
  rename(State = State.y) %>%
  select(-State.x)

cty_data_gather <- cty_data_gather %>%
  left_join(state_name, by = 'STATEFP') %>%
  rename(State = State.y) %>%
  select(-State.x)

# Collect state level data

state_coord <- read_csv('state_coord.csv')

state_gather <- cv_us_today %>%
  select(1:3,5:ncol(cv_us_today),4)

col_index <- grep('^[0-9]|^value',names(state_gather))

state_gather <- state_gather %>%
  tidyr::gather(date,value,col_index) 

state_gather <- state_gather %>%
  mutate(date = ifelse(date == 'value', paste(month(today), day(today), year(today), sep = '/'), date))

state_gather <- state_gather %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'))

state_gather <- state_gather %>%
  group_by(State, stateFIPS, date, type) %>%
  summarise(value = sum(value))

state_gather <- state_gather %>%
  left_join(select(state_coord,1:3), by = c('State' = 'state')) %>%
  filter(!is.na(value)) %>%
  filter(!is.na(State))

# Collect US level data

us_gather <- state_gather %>%
  group_by(type, date) %>%
  summarise(value = sum(value))

# Load worldwide data ------------------------------------------------

url_confirm <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
url_death <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
covid_confirm <- read_csv(url(url_confirm))
covid_death <- read_csv(url(url_death))

covid_confirm$type = 'CONFIRM'
covid_death$type = 'DEATH'

covid <- bind_rows(covid_confirm, covid_death)

col_index <- grep('^[0-9]', names(covid))
names(covid)[col_index] <- paste0(names(covid)[col_index], '20')

covid_by_ctry <- covid %>%
  group_by(`Country/Region`, type) %>%
  summarise_if(is.numeric, sum, na.rm = T) %>%
  rename(country = `Country/Region`)

# identify countries with more than 200 confirmed cases
ctry_with_200plus <- covid_by_ctry %>% dplyr::filter(type == 'CONFIRM')
ctry_with_200plus <- ctry_with_200plus[ctry_with_200plus[, ncol(ctry_with_200plus)] > 200, ]
ctry_with_200plus <- ctry_with_200plus$country

covid_tidy <- covid_by_ctry %>%
  filter(country %in% ctry_with_200plus) %>%
  tidyr::gather(date,value,5:ncol(covid_by_ctry))

# Data frame for COVID19 today

cv_today <- covid_tidy %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  filter(date == max(date)) %>%
  arrange(-value)

save(state_gather, state_coord, us_gather, cty, cty_data_gather, cv_death_gather, cv_us_confirm, cv_us_deaths, 
     cv_us_today, cv_us, covid_tidy, cv_today, covid_by_ctry, ctry_with_200plus, covid, today, bins, pal,
     file = 'data/covid_data.RData')