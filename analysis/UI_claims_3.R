## Amelia Ockert
## First edit: Dec 9 2020

## Last updated: Dec 18 2020

## Objective: Examine UI Claims by Industry dataset to explore how COVID-19 has affected
# employment across different industries

## Source: https://data.ct.gov/dataset/UI-Claims-by-Industry/r437-8xv7

## Load relevant libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggrepel)
library(RSocrata) # get data from ct.gov via API 

## read in "UI Claims by Industry" as csv using 'read.socrata' function
#df <- RSocrata::read.socrata("https://data.ct.gov/resource/r437-8xv7.csv")
#dplyr::glimpse(df)


# Data Loading and Cleaning -----------------------------------------------

## read in "UI Claims by Industry" csv from Data folder
ui <- read.csv("data\\ui_claims_data.csv")
dplyr::glimpse(ui)

## Remove time character that comes after the date
ui$new_claim_date <- sub("T.*", "", ui$new_claim_date)
## Convert date character string to date format
ui$new_claim_date <- lubridate::ymd(ui$new_claim_date)
class(ui$new_claim_date) #check that it worked

## extract year and month
ui$year <- as.numeric(format(ui$new_claim_date, "%Y"))
ui$month <- as.numeric(format(ui$new_claim_date, "%m"))


# Data Transformation and Value-Finding -----------------------------------


## Find total number of claims per month and per year
ui2 <- ui %>%
  dplyr::group_by(year, month) %>%
  dplyr::mutate(total_month = sum(total)) %>%
  ungroup() %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(total_year = sum(total))

#df2$month2 <- as.character(format(df$new_claim_date, "%m"))


ui_industry <- ui2 %>%
  tidyr::pivot_longer(cols = agric_forestry_fishing_hunting:other_unknown,
                      names_to = "industry", values_to = "claims")

## Naming Months
ui_industry$month_abbr <- month(ui_industry$new_claim_date, label = TRUE)

## test plot
df2 %>%
  ggplot2::ggplot(ggplot2::aes(new_claim_date, Total)) +
  ggplot2::geom_point()

## Plot 1: Interactive barplot 2019 and 2020
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



## Plot
df_industry2 %>%
  dplyr::filter(year == 2020) %>%
  ggplot2::ggplot(aes(new_claim_date, log(claims), color = industry)) +
  ggplot2::geom_path() +
  theme(legend.position = "none")

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
cols_logged <- colorRampPalette(colors = c("#ede8b0","#e06c00","#e60404","#760000"))

p2 <- df_industry2 %>%
  ggplot2::ggplot(aes(x = month_abbr, y = year)) +
  ggplot2::geom_tile(aes(fill = log(Total))) +
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
