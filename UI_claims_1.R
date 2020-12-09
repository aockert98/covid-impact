## Amelia Ockert
## First edit: Dec 9 2020

## Objective: Examine UI Claims by Industry dataset to explore how COVID-19 has affected
# employment across different industries

## Source: https://data.ct.gov/dataset/UI-Claims-by-Industry/r437-8xv7

## Load relevant libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(RSocrata) # get data from ct.gov via API 

## read in "UI Claims by Industry" as csv using 'read.socrata' function
df <- RSocrata::read.socrata("https://data.ct.gov/resource/r437-8xv7.csv")
dplyr::glimpse(df)

## Create year and month columns using lubridate format function
df$year <- as.numeric(format(df$new_claim_date, "%Y"))
df$month <- as.numeric(format(df$new_claim_date, "%m"))



## Find total number of claims by month (for each year)
df2 <- df %>%
  dplyr::group_by(year, month) %>%
  dplyr::mutate(total_month = sum(total))

## Find total number of claims by year
df2 <- df %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(total_year = sum(total))

df2$month2 <- as.character(format(df$new_claim_date, "%m"))


class(df2$month2)
df2$month3 <- as.factor(df2$month2)  
class(df2$month3)

## test plot
df2 %>%
  ggplot2::ggplot(ggplot2::aes(new_claim_date, total)) +
  ggplot2::geom_point()

## Plot 1: Interactive barplot 2019 and 2020
library(ggrepel)
library(plotly)
p1 <- df2 %>%
  dplyr::filter(year > 2018, month < 12) %>%
  ggplot2::ggplot(ggplot2::aes(month3, total_month, fill = year, group = year)) +
  ggplot2::geom_col(position = "dodge") + 
  ggplot2::labs(x = "Month", y = "Total claims",
       title = "Unemployment Insurance Claims by Month",
       fill = "Year") +
  ggplot2::theme_minimal()

plotly::ggplotly(p1)
  

## Plot by year
library(gganimate)
df2 %>%
  ggplot(aes(year, total_year/1000)) +
  geom_point() +
  geom_path() +
  labs(y = "Total Claims per year (in thousands)",
       title = "COVID-19's Economic Impact in Connecticut",
       subtitle = "Unemployment Insurance Claims 2005-2020") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
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
  gather(industry, claims, construction:real_estate); glimpse(df_industry2)

## Plot
df_industry2 %>%
  filter(year == 2020) %>%
  ggplot(aes(new_claim_date, claims, fill = industry)) +
  geom_path()

## By percent of total claims
df_industry2 %>%
  filter(year == 2020) %>%
  mutate(pct = claims/total) %>%
  ggplot(aes(new_claim_date, pct, fill = industry)) +
  geom_path()
