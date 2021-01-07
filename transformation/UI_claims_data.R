## Amelia Ockert
## December 23 2020

## Clean and Transform UI Claims by Industry data for the exploration of how COVID-19
# has affected employment across different industries in Connecticut

## Source: https://data.ct.gov/dataset/UI-Claims-by-Industry/r437-8xv7

## Load relevant libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)


## Read in "UI Claims by Industry" as csv
ui <- read.csv("data/economic/ui_claims_data.csv") %>% 
  # add columns that parse out the "year" & "month" from "new_claim_date"
  dplyr::mutate(
    new_claim_date = as.Date(new_claim_date), 
    year = lubridate::year(new_claim_date), 
    month = lubridate::month(new_claim_date)
  ) %>% 
  # Find total number of claims by month (for each year)
  dplyr::group_by(year, month) %>%
  dplyr::mutate(total_month = sum(total)) %>% 
  dplyr::ungroup() %>% 
  # Find total number of claims by year
  dplyr::group_by(year) %>%
  dplyr::mutate(total_year = sum(total)) %>%  
  dplyr::ungroup() %>% 
  # Month abbreviation column
  dplyr::mutate(month_abbr = lubridate::month(new_claim_date, label = TRUE)) 

## Examine data
dplyr::glimpse(ui)


## Pivot data from wide to long format, creating "Industry" and "Claims" columns
ui_industry <- ui %>%
  dplyr::select(-unnamed_column) %>%
  tidyr::pivot_longer(
    cols = agric_forestry_fishing_hunting:other_unknown, 
    names_to = "industry", 
    values_to = "claims") 

# Clean industry names
ui_industry$industry <- stringr::str_replace_all(ui_industry$industry,"_"," ")
ui_industry$industry <- stringr::str_to_title(ui_industry$industry)


## Per Industry data
ui_industry <- ui_industry %>%
  # Total claims per month (by industry)
  dplyr::group_by(month, year, industry) %>%
  dplyr::mutate(industry_month = sum(claims, na.rm=TRUE)) %>% 
  dplyr::ungroup() %>% 
  # Total claims per year (by industry)
  dplyr::group_by(year, industry) %>%
  dplyr::mutate(industry_year = sum(claims, na.rm=TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year_fact = cut(
    year, breaks = c(2004:2020), 
    labels = c(2005:2020))) 
  
## Change year names
ui_industry2 <- ui_industry
ui_industry2$year <- paste0("year",ui_industry2$year)

## Long to Wide data for Dumbbell Plot
ui_dumb <- ui_industry %>%
  select(year, industry, industry_year) %>%
  pivot_wider(names_from = "year",
              values_from = "industry_year",
              values_fn = mean) 
ui_dumb2 <- ui_industry2 %>%
  select(year, industry, industry_year) %>%
  pivot_wider(names_from = "year",
              values_from = "industry_year",
              values_fn = mean)

## Function to change user input from 2005 to "year2005", eg

myFunc <- function(x){
  new <- paste0("year",x)
  print(new)
  ui_dumb2 <- ui_dumb2 %>%
    mutate(hey = year2020 - (paste0("year",x)))
 # ui_dumb2 <- ui_dumb2 %>%
  #  dplyr::mutate(delta2 = (year2020 - new)/new * 100)
}
myFunc(2015)

ui_dumb2 %>%
  mutate(hi = year2020 - year2019)


## Percent change column formula:
## [new (2020) - old (selected year)] / old (selected year)
ui_dumb <- ui_dumb %>%
  mutate(delta = (`2020`-`2013`)/`2013` * 100)

ui_dumb2 <- ui_dumb2 %>%
  mutate(delta = (year2020 - year2013)/year2013 * 100)

## Clean up for Data Table 
library(DT)
ui_dt <- ui_industry %>%
  # select relevant columns
  dplyr::select(Year = year,
                Industry = industry,
                Total = industry_year) %>%
  # year as character makes filtering easier
  dplyr::mutate(Year = as.character(Year)) %>%
  # summarize stats so one row per industry (per year)
  dplyr::group_by(Year, Industry) %>%
  dplyr::summarize(Total = mean(Total),
                   .groups = "drop")





