## Amelia Ockert
## First edit: Dec 9 2020

## Objective: Examine UI Claims by Industry dataset to explore how COVID-19 has affected
# employment across different industries

## Source: https://data.ct.gov/dataset/UI-Claims-by-Industry/r437-8xv7

## Load relevant libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(RSocrata) # get data from ct.gov via API 

## read in "UI Claims by Industry" as csv using 'read.socrata' function
df <- RSocrata::read.socrata("https://data.ct.gov/resource/r437-8xv7.csv")
glimpse(df)

## Create year and month columns using lubridate format function
df$year <- as.numeric(format(df$new_claim_date, "%Y"))
df$month <- as.numeric(format(df$new_claim_date, "%m"))



## Find total number of claims by month (for each year)
df2 <- df %>%
  dplyr::group_by(year, month) %>%
  mutate(total_month = sum(total))

df2$month2 <- as.character(format(df$new_claim_date, "%m"))

df2 %>%
  ggplot(aes(new_claim_date, total)) +
  geom_point()

library(ggrepel)
library(plotly)
p1 <- df2 %>%
  filter(year > 2018, month < 12) %>%
  ggplot(aes(month3, total_month, fill = year, group = year)) +
  geom_col(position = "dodge") +
  labs(x = "Month", y = "Total claims",
       title = "Unemployment Insurance Claims by Month",
       fill = "Year") +
  theme_minimal()

ggplotly(p1)
  



class(df2$month2)
df2$month3 <- as.factor(df2$month2)  
class(df2$month3)
