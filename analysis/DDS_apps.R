
library(RSocrata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)

options(tigris_use_cache = TRUE)
library(tigris)
library(maps)
library(USAboundaries)
library(USAboundariesData)


# 1.0 - DSS Applications Activity -----------------------------------------------

## Read in the DDS Applications Activity data from the "data/" folder
dss <- readr::read_csv(
  file = "data/dss_data.csv", 
  # ensure the "week_ending_date" column variable is read in as type "character"
  col_types = readr::cols(
    week_ending_date = readr::col_character()
  )
) %>% 
  # convert the "week_ending_date" column variable to type "date" by extracting 
  # the first 10 characters from the values in that column
  dplyr::mutate(
    week_ending_date = as.Date(
      stringr::str_sub(
        string = week_ending_date, 
        start = 1, 
        end = 10
      )
    )
  )

## If you want to quickly read in the latest data from Socrata without updating the
## data in the "data/" directory, uncomment and run the line below
# df <- RSocrata::read.socrata("https://data.ct.gov/resource/ymej-83fh.csv")

# Take a peek at the data in the 'df' data frame
dplyr::glimpse(dss)

# Overwrite the 'df' data frame by performing the following transformations...
dss <- dss %>% 
  # create a new column called "month" by parsing the month from the 
  # "week_ending_date" column variable 
  dplyr::mutate(
    month = month(
      week_ending_date, 
      label = TRUE
    )
  ) %>% 
  # rename the four columns included below along our new naming conventions 
  dplyr::rename(
    cash = weekly_applications_received_cash_assistance,
    medical = weekly_applications_received_medical_assistance,
    snap = weekly_applications_received_snap,
    all_apps = weekly_applications_received_all
  ) %>% 
  # convert the data from *wide* to *long* format
  tidyr::pivot_longer(
    names_to = "Type",
    values_to = "Applications",
    cols = cash:snap
  )

# Plot a line chart of the # of DSS applications rec'd by date & type
dss %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = week_ending_date, 
      y = Applications,
      color = Type
    )
  ) +
  ggplot2::geom_line() +
  geom_line(aes(x=week_ending_date,
                y = all_apps), inherit.aes=FALSE, color = "darkgray") +
  ggplot2::labs(title = "Weekly DSS Claims in Connecticut") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(
      size = 16, 
      hjust = 0.5
    )
  )


dss %>%
  filter(Type == "snap") %>%
  ggplot(aes(x = week_ending_date,
             y = Applications)) +
  geom_col()


# CT Town Map -------------------------------------------------------------

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

## Read in covid town data from data folder
covid_town <- read.csv(file.choose())

## Remove time character that comes after the date
covid_town$lastupdatedate <- sub("T.*", "", covid_town$lastupdatedate)
## Convert date character string to date format
covid_town$lastupdatedate <- lubridate::ymd(covid_town$lastupdatedate)
class(covid_town$lastupdatedate) #check that it worked


## Transform
town_cases <- covid_town %>%
  rename(date = lastupdatedate,
         totalcases = towntotalcases,
         per100k = towncaserate)


## Merge town map data with town covid data
towns <- towns %>%
  rename(town = NAME)

all_together <- full_join(towns, town_cases, by = "town")

## Map town covid data
all_together2 <- all_together %>%
  filter(date == "2020-12-10")


## Create color palette
cols <- colorRampPalette(colors = c("#f9e8bf","#f2dc71","#eea353","#f78037",
                                    "#ea4b24","#eb411f","#eb371a","#eb220f",
                                    "#c11108","#960000"))

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
