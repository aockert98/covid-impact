## Amelia Ockert
## First edit: Dec 9 2020

## Objective: Examine UI Claims by Industry dataset to explore how COVID-19 has affected
# employment across different industries

## Source: https://data.ct.gov/dataset/UI-Claims-by-Industry/r437-8xv7

## Load relevant libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(RSocrata) # get data from ct.gov via API 

## Read in "UI Claims by Industry" as csv
df <- read.csv("data/economic/ui_claims_data.csv") %>% 
  # add columns that parse out the "year" & "month" from "new_claim_date"
  dplyr::mutate(
    year = lubridate::year(as.Date(new_claim_date)), 
    month = lubridate::month(as.Date(new_claim_date))
  ) %>% 
  # Find total number of claims by month (for each year)
  dplyr::group_by(year, month) %>%
  dplyr::mutate(total_month = sum(total)) %>% 
  dplyr::ungroup() %>% 
  # Find total number of claims by year
  dplyr::group_by(year) %>%
  dplyr::mutate(total_year = sum(total)) %>%  
  dplyr::ungroup() %>% 
  dplyr::mutate(
    month2 = format(as.Date(new_claim_date), "%m"), 
    month3 = as.factor(month2), 
    month_abbr = lubridate::month(new_claim_date, label = TRUE)
  )

dplyr::glimpse(df)

## test plot
df2 %>%
  ggplot2::ggplot(ggplot2::aes(new_claim_date, total)) +
  ggplot2::geom_point()

## Plot 1: Interactive barplot 2019 and 2020
library(ggrepel)
library(plotly)
p1 <- df2 %>%
  dplyr::filter(year > 2018, month < 12) %>%
  ggplot2::ggplot(ggplot2::aes(month, total_month, fill = as.factor(year))) +
  ggplot2::geom_col(position = "dodge") + 
  ggplot2::scale_fill_discrete(name = "Year") + 
  ggplot2::labs(x = "Month", y = "Total claims",
       title = "Unemployment Insurance Claims by Month",
       fill = "Year") +
  ggplot2::theme_minimal();p1

plotly::ggplotly(p1)
  

## Plot by year
library(gganimate)
df2 %>%
  ggplot2::ggplot(aes(year, total_year/1000)) +
  ggplot2::geom_point() +
  ggplot2::geom_path() +
  ggplot2::labs(y = "Total Claims per year (in thousands)",
       title = "COVID-19's Economic Impact in Connecticut",
       subtitle = "Unemployment Insurance Claims 2005-2020") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



## DATA TRANSFORMATION
## select certain industries

df_industry <- df2 %>%
  dplyr::select(new_claim_date, month, year, total, total_year,
                construction, manufacturing, wholesale_trade, retail_trade,
                real_estate)

## Transform wide data to long data
df_industry2 <- df_industry %>%
  tidyr::pivot_longer(cols = construction:real_estate, names_to = "industry",
                      values_to = "claims")

## Plot
df_industry2 %>%
  dplyr::filter(year == 2020) %>%
  ggplot2::ggplot(aes(new_claim_date, claims, fill = industry)) +
  ggplot2::geom_path()

## By percent of total claims
df_industry2 %>%
  dplyr::filter(year == 2020) %>%
  dplyr::mutate(pct = claims/total) %>%
  ggplot2::ggplot(aes(new_claim_date, pct, fill = industry)) +
  ggplot2::geom_path()


## Heatmap 
## color scale needs work! will be tricky
## interesting to see the next "darkest" row is 2008-09...

## logged cases works MUCH better

p2 <- df_industry2 %>%
  ggplot2::ggplot(aes(x = month_abbr, y = year)) +
  ggplot2::geom_tile(aes(fill = log(total))) +
  scale_fill_gradientn(colors = cols_logged(6)) +
  labs(title = "COVID-19 and Connecticut's Economy", subtitle = "UI Claims by Month, 2005-2020",
       fill = "Total Claims (logged)") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45)) ; p2

#doesnt work for some reason? ggplotly(p2)
## create color palette
cols <- colorRampPalette(colors = c("#f0c897","#c47055","#ad4534","#a22f24",
                                    "#971913","#920e0b","#8c0303"))

cols_logged <- colorRampPalette(colors = c("#ede8b0","#e06c00","#e60404","#760000"))
