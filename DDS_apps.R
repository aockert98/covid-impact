
library(RSocrata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

options(tigris_use_cache=TRUE)
library(tigris)
library(maps)
library(USAboundaries)
library(USAboundariesData)

## Download Weekly DSS Application Activity from data.ct.gov
df <- RSocrata::read.socrata("https://data.ct.gov/resource/ymej-83fh.csv")
dplyr::glimpse(df)

df$month <- month(df$week_ending_date, label = TRUE)
df <- df %>%
  rename(cash = weekly_applications_received_cash_assistance,
         medical = weekly_applications_received_medical_assistance,
         snap = weekly_applications_received_snap,
         all_apps = weekly_applications_received_all)
## Wide to long
df2 <- df %>%
  pivot_longer(names_to = "Type",
               values_to = "Applications",
               cash:snap)

df2 %>%
  ggplot(aes(week_ending_date, Applications,
             color = Type)) +
  geom_line() +
  labs(title = "Weekly DDS Claims in Connecticut") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 16,
                                  hjust = 0.5))

## Get CT county map data
towns <- tigris::county_subdivisions(state = "Connecticut") %>%
  filter(NAMELSAD != "County subdivisions not defined")
class(towns) #sf

## County borders
map <- us_counties() %>%
  filter(state_name == "Connecticut") %>%
  rename(COUNTYFP = countyfp)


ggplot() +
  geom_sf(data = towns, fill = "white", color = "gray") +
  geom_sf(data = map, color = "black", alpha = 0) +
  labs(title = "Map of CT Towns and Counties") +
  theme_void()

## Covid town data

covid_town <- RSocrata::read.socrata("https://data.ct.gov/resource/28fr-iqnx.csv")
dplyr::glimpse(covid_town)

covid_town %>%
  filter(lastupdatedate > 2020-07-01) %>%
  ggplot(aes(lastupdatedate, towntotalcases, color = town)) +
  geom_path() +
  theme(legend.position = "none")


## Covid Ages
## NEEDS MUCH CLEANING
## age brackets-- remove spaces for some
## find census pop data for age group if possible

covid_age <- RSocrata::read.socrata("https://data.ct.gov/resource/ypz6-8qyf.csv")

covid_age %>%
  ggplot(aes(dateupdated,totalcases,color = agegroups)) +
  geom_path()
