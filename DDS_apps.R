
library(RSocrata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

options(tigris_use_cache=TRUE)
library(tigris)

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

towns %>%
  ggplot() +
  geom_sf()
