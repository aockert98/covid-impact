## Amelia Ockert
## First edit: Dec 9 2020

## Last updated: Dec 15 2020

## Objective: Examine UI Claims by Industry dataset to explore how COVID-19 has affected
# employment across different industries

## Source: https://data.ct.gov/dataset/UI-Claims-by-Industry/r437-8xv7

## Load relevant libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggrepel)
library(plotly)

## read in "UI Claims by Industry" csv from Data folder
df <- read.csv("data/economic/ui_claims_data.csv") %>% 
  dplyr::mutate(
    new_claim_date = as.Date(stringr::str_sub(new_claim_date, start = 1, end = 10)), 
    year = lubridate::year(new_claim_date), 
    month = lubridate::month(new_claim_date)
  ) %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::mutate(total_month = sum(total)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year) %>%
  dplyr::mutate(total_year = sum(total)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    month2 = format(new_claim_date, "%m"), 
    month3 = as.factor(month2)
  )

dplyr::glimpse(df)


## DATA TRANSFORMATION

df_industry <- df %>%
  dplyr::select(
    new_claim_date, 
    month, 
    year, 
    total, 
    total_month, 
    total_year,
    tidyr::everything()
  ) %>% 
  tidyr::pivot_longer(
    cols = agric_forestry_fishing_hunting:other_unknown, 
    names_to = "industry", 
    values_to = "claims"
  ) %>% 
  dplyr::mutate(month_abbr = lubridate::month(new_claim_date, label = TRUE))


## Plot
df_industry %>%
  dplyr::filter(year == 2020) %>%
  ggplot2::ggplot(aes(new_claim_date, log(claims), color = industry)) +
  ggplot2::geom_path() +
  theme(legend.position = "none")

## Total claims per month per industry, by year
df_industry3 <- df_industry2 %>%
  dplyr::group_by(month, year, industry) %>%
  dplyr::mutate(industry_month = sum(claims, na.rm=TRUE))
df_industry3 <- df_industry3 %>%
  dplyr::group_by(year, industry) %>%
  dplyr::mutate(industry_year = sum(claims, na.rm=TRUE))


## Transform year from continuous to discrete (factor)
df_industry3$year_fact <- cut(df_industry3$year, breaks = c(2004:2020), 
                              labels = c(2005:2020))
## Plot
df_industry3 %>%
  filter(industry == "Construction",
         year > 2018) %>%
  ggplot(aes(month_abbr, industry_month, fill = year_fact)) +
  geom_col(position = "dodge")

## Industry percentage by year
df_industry3 <- df_industry3 %>%
  dplyr::mutate(industry_pct = ((industry_year/total_year) * 100))


## Plot 
df_industry3 %>%
  filter(year > 2018) %>%
  filter(industry %in% c("Construction","Wholesale.Trade",
                         "Retail.Trade","Real.Estate","Manufacturing",
                         "Self.Employed","Accommodation...Food.Services")) %>%
  ggplot(aes(industry, industry_pct, fill = year_fact)) +
  geom_col(position="dodge")


## RENAMING INDUSTRIES



## INTERACTIVE DATATABLE ##
## DT Package creates interactive tables
library(DT)

## Clean up df_industry2 for datatable
df_industry3 <- df_industry2 %>%
  select(Year = year, Month = month_abbr, 
         Industry = industry, Claims = claims)
df_industry3 <- df_industry3 %>%
  dplyr::group_by(Month, Year, Industry) %>%
  dplyr::summarize(total_month = sum(Claims))

## Clean up Industry names-- remove the _ and capitalize first letters of the words
library(stringr)

df_industry3$Industry <- stringr::str_replace(df_industry3$Industry, "_", " ")
df_industry3$Industry <- stringr::str_to_title(df_industry3$Industry)

## Create datatable

## Makes filtering years easier
df_industry4 <- df_industry3 %>%
  dplyr::select(Year = year,
                Industry = industry, Total = industry_year,
                Percent = industry_pct)
df_industry4$Year = as.character(df_industry4$year)


DT::datatable(df_industry4,
              rownames = FALSE,
              filter = "top",
              #colnames = c("Total" = "total_month"),
              extensions = "Buttons",
              options = list(dom = "Bfrtip",
                             buttons = c("csv","excel","pdf")))



## Need help summarizing/ cleaning to just show one value per month etc
