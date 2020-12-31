## Objective: Map COVID cases in CT, by town

## use census data to get cases per 100K
## use age data to explore age groups most affected
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tigris)
options(tigris_use_cache = TRUE)
library(maps)
library(USAboundaries)
library(USAboundariesData)


# CT Town Map -------------------------------------------------------------

## Get CT county map data
towns <- tigris::county_subdivisions(state = "Connecticut") %>%
  filter(NAMELSAD != "County subdivisions not defined")
class(towns) #sf

## County borders
map <- us_counties() %>%
  filter(state_name == "Connecticut") %>%
  rename(COUNTYFP = countyfp)

## Maps towns with county overlapping
ggplot() +
  geom_sf(data = towns, fill = "white", color = "gray") +
  geom_sf(data = map, color = "black", alpha = 0) +
  labs(title = "Map of CT Towns and Counties") +
  theme_void()



# Covid Cases by Town -----------------------------------------------------

## RSocrata method
#covid_town <- RSocrata::read.socrata("https://data.ct.gov/resource/28fr-iqnx.csv")

## Read in covid town data from medical subfolder
covid_town <- read.csv("data/medical/covid_town.csv") %>%
  rename(date = lastupdatedate,
         totalcases = towntotalcases,
         per100k = towncaserate)
dplyr::glimpse(covid_town)

## Remove time character that comes after the date
covid_town$date <- sub("T.*", "", covid_town$date)
## Convert date character string to date format
covid_town$date <- lubridate::ymd(covid_town$date)
class(covid_town$date) #check that it worked


## Merge the Town Map data with the Town Covid data
towns <- towns %>%
  rename(town = NAME)

## Join them together
town_cases_map <- full_join(towns, covid_town, by = "town")

## Subset data to the most recent updated date--This includes cumulative cases thru mid-December
# and makes our dataset smaller and easier to manage
mid_dec <- town_cases_map %>%
  filter(date == "2020-12-15")

## Create color palette
cols <- colorRampPalette(colors = c("#f9e8bf","#f2dc71","#eea353","#f78037",
                                    "#ea4b24","#eb411f","#eb371a","#eb220f",
                                    "#c11108","#960000"))

## Map COVID cases thru mid December, by CT town
p1 <- mid_dec %>%
  ggplot() +
  geom_sf() +
  geom_sf(aes(fill = per100k)) +
  labs(title = "The Coronavirus in Connecticut Towns",
       subtitle = "Cases per 100,000",
       fill = "") +
  scale_fill_gradientn(colors = cols(10)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) ; p1

## Make interactive
library(plotly)
ggplotly(p1)



# Stats by Age Group ------------------------------------------------------

## Covid Ages
## find census pop data for age group if possible
install.packages("here")
library(here)
# Read in data from "data/" folder
covid_age <- readr::read_csv(file = here::here("data/covid_age_data.csv"))

# See that we need to scrub these age groups to create consistency
unique(covid_age$agegroups)

# Clean up 'covid_age' data frame...
covid_age <- covid_age %>% 
  dplyr::mutate(
    # convert "dateupdated" column variable from type "datetime" to just type "date"
    dateupdated = as.Date(dateupdated), 
    # remove any whitespace in the values in the "agegroups" column variable
    agegroups = stringr::str_replace_all(agegroups, " ", ""), 
    # perform the following case logic...
    agegroups = dplyr::case_when(
      # convert values of "80andolder" in the "agegroups" column variable to "80+"
      agegroups == "80andolder" ~ "80+", 
      # it looks like Excel probably incorrectly formatted some values of "10-19" to
      # "Oct-19" before they uploaded it; let's change those back to "10-19"
      agegroups == "19-Oct" ~ "10-19", 
      # for everything else, leave it as its current value
      TRUE ~ agegroups
    )
  )

# Plot a line chart of the total cases by date and age group
covid_age %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = dateupdated, 
      y = totalcases, 
      color = agegroups)
  ) +
  ggplot2::geom_path()

# Bar plot of total cases by age group
covid_age %>%
  filter(dateupdated == "2020-12-15") %>%
  ggplot2:: ggplot(
    aes(
      x = agegroups,
      y = totalcases
    )
  ) +
  geom_col()

covid_age %>%
  #filter(agegroups == "20-29") %>%
  ggplot() +
  # geom_line(aes(dateupdated, totalcases)) +
  geom_line(aes(dateupdated, totaldeaths, color = agegroups)) +
  labs(y = "Total Deaths",
       title = "COVID-19 Deaths in Connecticut",
       color = "Age Group") +
  theme_minimal()

## Death rate-- deaths per cases for each age group
covid_age_dr <- covid_age %>%
  group_by(agegroups, dateupdated) %>%
  mutate(deathrate = totaldeaths/totalcases)

## Broaden Age Groups
# still to do

## Census data to find %s
td <- read.csv(file.choose())

covid_age_dr %>%
  ggplot(aes(dateupdated, deathrate, color = agegroups)) +
  geom_line() +
  labs(y = "Death Rate",
       title = "Covid Deathrate across Age Groups",
       color = "Age Group") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))



# Claims by Town ----------------------------------------------------------

# load in town claims dataset
tc <- read.csv("data/geo/initial_claims_town.csv") %>%
  pivot_longer(names_to = "date",
               values_to = "claims",
               January.09.2005:December.20..2020) %>%
  dplyr::mutate(date = lubridate::mdy(date))
dplyr::glimpse(tc)

## Subset dataset for the year 2020
tc2020 <- tc %>%
  filter(date >= "2020-01-01") %>%
  filter(Town.Name != "Total") %>%
  dplyr::mutate(claims = str_remove_all(claims, ",")) #remove commas so can convert claims from char to numeric

tc2020$claims <- as.numeric(tc2020$claims)

# Summarize dataset to find the total claims for each town in the year 2020
tc2020 <- tc2020 %>%
  group_by(Town.Name) %>%
  summarize(total_claims = sum(claims, na.rm=TRUE)) %>%
  ungroup()

# Join with town data for mapping
towns <- towns %>%
  rename(Town.Name = town)

town_claim_map <- full_join(towns, tc2020, by = "Town.Name")

# Create map of claims
town_claim_map %>%
  ggplot() +
  geom_sf() +
  geom_sf(aes(fill = total_claims)) +
  labs(title = "Economic Impact of COVID-19 in CT",
       subtitle = "UI Files Claimed 2020",
       fill = "") +
  scale_fill_gradientn(colors = cols(10)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 
