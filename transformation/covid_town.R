library(dplyr)
library(stringr)
library(tigris)
options(tigris_use_cache = TRUE)
library(maps)
library(USAboundaries)
library(USAboundariesData)
library(here)


# CT Town Map -------------------------------------------------------------

## Get CT county map data
towns <- tigris::county_subdivisions(state = "Connecticut") %>%
  dplyr::filter(NAMELSAD != "County subdivisions not defined")

## County borders
map <- us_counties() %>%
  dplyr::filter(state_name == "Connecticut") %>%
  dplyr::rename(COUNTYFP = countyfp)

## Read in covid town data from medical subfolder
covid_town <- read.csv(
  here::here("data/medical/covid_town.csv")
) %>%
  dplyr::rename(
    date = lastupdatedate,
    totalcases = towntotalcases,
    per100k = towncaserate
  ) %>% 
  dplyr::mutate(
    date = as.Date(
      stringr::str_sub(
        string = date, 
        start = 1, 
        end = 10
      )
    )
  )

## Merge the Town Map data with the Town Covid data
## Subset data to the most recent updated date--This includes cumulative cases thru mid-December
# and makes our dataset smaller and easier to manage
mid_dec <- towns %>% 
  dplyr::full_join(
    covid_town, 
    by = c("NAME" = "town")
  ) %>% 
  dplyr::filter(date == "2020-12-15")

# Remove dataframes we don't use in app
rm(towns, map, covid_town)