
## County data from New York Times' Github Page
counties <- read.csv("data/geo/us-counties.csv") %>%
  dplyr::filter(state == "Connecticut")


library(USAboundariesData)
library(maps)
library(USAboundaries)

## Get map
county_map <- us_counties() %>%
  filter(state_abbr == "CT") %>%
  mutate(fips = as.numeric(geoid))

## Merge with covid data, by FIPS code
covid_df <- full_join(county_map, counties_df, by = "fips")
anti_df <- anti_join(county_map, counties_df, by = "fips")


## Map 
map_plot <- covid_df %>%
  filter(date == "2020-12-10") %>%
  na.omit() %>%
  ggplot() +
  geom_sf(aes(fill = cases)) +
  geom_sf_text(aes(label = name), size = 3) +
  scale_fill_gradientn(colors = cols_logged(6)) +
  labs(title = "COVID-19 throughout Connecticut Counties",
       fill = "Total Cases") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(map_plot)
