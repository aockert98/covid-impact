library(tidyverse)
library(plotly)
library(USAboundariesData)
library(maps)
library(USAboundaries)

## County data from New York Times' Github Page
counties <- read.csv("data/geo/us-counties.csv") %>%
  dplyr::filter(state == "Connecticut")

## Get map
county_map <- USAboundaries::us_counties() %>% 
  dplyr::filter(state_abbr == "CT") %>%
  dplyr::mutate(fips = as.numeric(geoid))

## Merge with covid data, by FIPS code
covid_df <- dplyr::full_join(county_map, counties, by = "fips")
anti_df <- dplyr::anti_join(county_map, counties, by = "fips")

# Create color palette
cols_logged <- colorRampPalette(colors = c("#ede8b0","#e06c00","#e60404","#760000"))

## Map 
map_plot <- covid_df %>%
  dplyr::filter(date == "2020-12-10") %>%
  na.omit() %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = cases)) +
  ggplot2::geom_sf_text(
    ggplot2::aes(label = name), 
    size = 3
  ) +
  ggplot2::scale_fill_gradientn(colors = cols_logged(6)) +
  ggplot2::labs(
    title = "COVID-19 throughout Connecticut Counties",
    fill = "Total Cases") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
map_plot
plotly::ggplotly(map_plot)

## Map plot
cols <- colorRampPalette(colors = c("#f9e8bf","#f2dc71","#eea353","#f78037",
                                    "#ea4b24","#eb411f","#eb371a","#eb220f",
                                    "#c11108","#960000"))

covid_map <- mid_dec %>%
  ggplot() +
  geom_sf() +
  geom_sf(aes(fill = per100k)) +
  labs(title = "The Coronavirus in Connecticut Towns",
       subtitle = "Cases per 100,000",
       fill = "") +
  scale_fill_gradientn(colors = cols(10)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggplotly(covid_map)

plot_ly(mid_dec,
        split = ~NAME,
        color= ~per100k,
        colors = cols(10))
