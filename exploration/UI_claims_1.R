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

## Read in "UI Claims by Industry" as csv
df <- read.csv("data/economic/ui_claims_data.csv") %>% 
  # add columns that parse out the "year" & "month" from "new_claim_date"
  dplyr::mutate(
    new_claim_date = as.Date(new_claim_date), 
    year = lubridate::year(new_claim_date), 
    month = lubridate::month(new_claim_date)
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
    month2 = format(new_claim_date, "%m"), 
    month3 = as.factor(month2), 
    month_abbr = lubridate::month(new_claim_date, label = TRUE)
  )

dplyr::glimpse(df)

## test plot
df %>%
  ggplot2::ggplot(ggplot2::aes(new_claim_date, total)) +
  ggplot2::geom_point()

## Plot 1: Barplot 2019 and 2020
p1 <- df %>%
  dplyr::filter(year > 2018, month < 12) %>%
  ggplot2::ggplot(ggplot2::aes(month, total_month, fill = as.factor(year))) +
  ggplot2::geom_col(position = "dodge") + 
  ggplot2::scale_fill_discrete(name = "Year") + 
  ggplot2::labs(x = "Month", y = "Total claims",
       title = "Unemployment Insurance Claims by Month",
       fill = "Year") +
  ggplot2::theme_minimal()

## Plot by year
df %>%
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
    plot.subtitle = element_text(hjust = 0.5)
  )

## DATA TRANSFORMATION
## select certain industries

df_industry <- df %>%
  dplyr::select(new_claim_date, month, year, month_abbr, total, total_year,
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
df_industry %>%
  dplyr::filter(year == 2020) %>%
  dplyr::mutate(pct = claims/total) %>%
  ggplot2::ggplot(aes(new_claim_date, pct, fill = industry)) +
  ggplot2::geom_path()


## Heatmap 
## create color palette
cols_logged <- colorRampPalette(colors = c("#ede8b0","#e06c00","#e60404","#760000"))

df_industry %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = month_abbr, 
      y = year
    )
  ) +
  ggplot2::geom_tile(ggplot2::aes(fill = log(total))) +
  ggplot2::scale_fill_gradientn(colors = cols_logged(6)) +
  ggplot2::labs(
    title = "COVID-19 and Connecticut's Economy", 
    subtitle = "UI Claims by Month, 2005-2020", 
    fill = "Total Claims (logged)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(
      hjust = 0.5, 
      size = 20, 
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5, 
      size = 16
    ),
    axis.text.x = ggplot2::element_text(angle = 45)
  )

#doesnt work for some reason? ggplotly(p2)

