
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

## Transform
town_cases <- covid_town %>%
  rename(date = lastupdatedate,
         totalcases = towntotalcases,
         per100k = towncaserate)

town_cases %>%
  ggplot(aes(date, per100k, color = town)) +
  geom_path() +
  theme(legend.position = "none")

## Merge town map data with town covid data
towns <- towns %>%
  rename(town = NAME)

all_together <- full_join(towns, town_cases, by = "town")

## Map town covid data
all_together2 <- all_together %>%
  filter(date == "2020-12-10")

p1 <- all_together2 %>%
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

all_together %>%
  filter(date > "2020-08-01") %>%
  ggplot() +
  geom_sf() +
  geom_sf(aes(fill = per100k)) +
  labs(title = "The Coronavirus in Connecticut Towns",
       subtitle = "Cases per 100,000 as of: {closest_state}",
       fill = "") +
  scale_fill_gradientn(colors = cols(10)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 
 # transition_time(date) #animates


library(gganimate)
## Make interactive
library(plotly)
ggplotly(p1)

## Create color palette
cols <- colorRampPalette(colors = c("#f9e8bf","#f2dc71","#eea353","#f78037",
                                    "#ea4b24","#eb411f","#eb371a","#eb220f",
                                    "#c11108","#960000"))

## Covid Ages
## NEEDS MUCH CLEANING
## age brackets-- remove spaces for some
## find census pop data for age group if possible

covid_age <- RSocrata::read.socrata("https://data.ct.gov/resource/ypz6-8qyf.csv")

covid_age %>%
  ggplot(aes(dateupdated,totalcases,color = agegroups)) +
  geom_path()
