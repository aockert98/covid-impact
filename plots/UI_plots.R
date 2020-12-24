## Objective-- Create several exploratory plots using the UI Claims data

## Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggalt)

# Grab the UI claims data 
source("transformation/UI_claims_data.R")

## Plot 1: Barplot total claims 2019 vs 2020
ui %>%
  dplyr::filter(year > 2018, month < 12) %>%
  ggplot2::ggplot(ggplot2::aes(month, total_month, fill = as.factor(year))) +
  ggplot2::geom_col(position = "dodge") + 
  ggplot2::scale_fill_discrete(name = "Year") + 
  ggplot2::labs(x = "Month", y = "Total claims",
                title = "Unemployment Insurance Claims by Month",
                fill = "Year") +
  ggplot2::theme_minimal()

## Plot 2: Line plot total claims by year
ui %>%
  ggplot2::ggplot(ggplot2::aes(year, total_year/1000)) +
  ggplot2::geom_point() +
  ggplot2::geom_path() +
  ggplot2::labs(
    y = "Total Claims per year (in thousands)",
    title = "COVID-19's Economic Impact in Connecticut",
    subtitle = "Unemployment Insurance Claims 2005-2020") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

## Plot 3: Heatmap 
## create color palette
cols_logged <- colorRampPalette(colors = c("#ede8b0","#e06c00","#e60404","#760000"))

ui_industry %>%
  ggplot2::ggplot(aes(month_abbr, year)) +
  ggplot2::geom_tile(aes(fill = log(total))) +
  ggplot2::scale_fill_gradientn(colors = cols_logged(6)) +
  ggplot2::labs(title = "COVID-19 and Connecticut's Economy",
    subtitle = "UI Claims by Month, 2005-2020",
    fill = "Total Claims (logged)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 16),
        axis.text.x = element_text(angle = 45),
        panel.grid = element_blank())


## Plot 4: Dumbbell plot

## Option 1: Rectangle Column
ui_dumb %>%
  filter(industry != "Self Employed") %>%
  ggplot2::ggplot(aes(x = `2013`/1000, xend = `2020`/1000, y = industry, group = industry)) +
  ggalt::geom_dumbbell(color = "darkgray",
                       colour_x = "darkgray",
                       colour_xend="black") +
  ggplot2::labs(x = "UI Claims Filed (in Thousands)",
                title = "The Economic Impact of the Coronavirus Pandemic in Connecticut",
                subtitle = "Comparing Unemployment Insurance Claims Filed in 2013 vs. 2020", 
                caption = "Source: (fill in)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.margin = unit(c(1.5,3,1,0), "cm"),
                 axis.title.y = element_blank()) +
  geom_rect(aes(xmin = 90, xmax = 100, ymin = -Inf, ymax = Inf), fill = "white") +
  geom_text(aes(label=paste0(round(delta), "%"), y = industry, x = 95), size = 3) +
  geom_text(data = filter(ui_dumb, industry=="Wholesale Trade"),
            aes(x = 95, y = industry, label = "Percent Change"),
            vjust = -1, size = 3, fontface = "bold") +
  scale_y_discrete(expand=c(0.15,0))  


## Option 2: Next to end
ui_dumb %>%
  filter(industry != "Self Employed") %>%
  ggplot2::ggplot(aes(x = `2013`/1000, xend = `2020`/1000, y = industry, group = industry)) +
  ggalt::geom_dumbbell(color = "darkgray",
                       colour_x = "darkgray",
                       colour_xend="black") +
  ggplot2::labs(x = "UI Claims Filed",
                y = "Industry",
                title = "The Economic Impact of the Coronavirus Pandemic in Connecticut",
                subtitle = "Comparing Unemployment Insurance Claims Filed in 2013 vs. 2020", 
                caption = "Source: (fill in)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.margin = unit(c(1.5,3,1,0), "cm")) +
  #geom_rect(aes(xmin = 100, xmax = 120, ymin = -Inf, ymax = Inf), fill = "grey") +
  geom_text(aes(label=paste0(round(delta), "%"), y = industry, x = `2020`/1000), hjust = -0.5, 
            size = 3) +
  geom_text(data = filter(ui_dumb, industry=="Wholesale Trade"),
            aes(x = 90, y = industry, label = "Percent Change"), vjust = -1, hjust = 0.5) +
  scale_y_discrete(expand=c(0.15,0))
