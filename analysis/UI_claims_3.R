## Amelia Ockert
## First edit: Dec 9 2020

## Last updated: Dec 19 2020

## Objective: Examine UI Claims by Industry dataset to explore how COVID-19 has affected
# employment across different industries

## Source: https://data.ct.gov/dataset/UI-Claims-by-Industry/r437-8xv7

## Load relevant libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
#library(RSocrata) # get data from ct.gov via API 

## read in "UI Claims by Industry" as csv using 'read.socrata' function
#df <- RSocrata::read.socrata("https://data.ct.gov/resource/r437-8xv7.csv")
#dplyr::glimpse(df)


# Data Loading and Cleaning -----------------------------------------------

## read in "UI Claims by Industry" csv from Data folder
ui <- read.csv("data/ui_claims_data.csv")
ui <- read.csv(file.choose())
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

## Transform data from wide to long format
ui_industry <- ui2 %>%
  tidyr::pivot_longer(cols = agric_forestry_fishing_hunting:other_unknown,
                      names_to = "industry", values_to = "claims")

## Naming Months
ui_industry$month_abbr <- month(ui_industry$new_claim_date, label = TRUE)


# Note: Plot Vision/Ideas -------------------------------------------------

## How has COVID affected the number of UI claims in CT?
## Which months of COVID have been particularly hard hit?

 # 1. Bar plot comparing avg claims/month or claims/year pre 2020,
  #vs claims in 2020
 
 # 2. Line plot visualizing these trends


## Which industries have been most impacted by COVID?

  # The percentage of claims by industry...(ie, if Construction used to 
  # only make up 5% of claims, and now makes up 20%, Construction industry
  # disproportionately affected)
  
  # Conversely, could find that claims go up for an industry, but % of total claims stays the same,


# Plots -------------------------------------------------------------------


## Plot 1: (Interactive) Barplot 2019 and 2020
## NOTE: vision for app- select year(s) to compare (to 2020)
p1 <- ui_industry %>%
  dplyr::filter(year > 2018, month < 12) %>%
  ggplot2::ggplot(aes(month_abbr, total_month, fill = as.factor(year))) +
  ggplot2::geom_col(position = "dodge") + 
  ggplot2::scale_fill_discrete(name = "Year") + 
  ggplot2::labs(x = "Month", y = "Total claims",
                title = "Unemployment Insurance Claims by Month",
                subtitle = "January - November",
                fill = "Year") +
  ggplot2::theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5));p1

## Add interaction
#plotly::ggplotly(p1)


## Plot by year

ui_industry %>%
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

## By percent of total claims
## UGLY-- just a test plot
ui_industry %>%
  dplyr::filter(year == 2020) %>%
  dplyr::mutate(pct = claims/total) %>%
  ggplot2::ggplot(aes(new_claim_date, pct, fill = industry)) +
  ggplot2::geom_path()


## Heatmap 
## color scale needs work! will be tricky
## interesting to see the next "darkest" row is 2008-09...

## logged cases works MUCH better
## Create color palette
cols_logged <- colorRampPalette(colors = c("#ede8b0","#e06c00","#e60404","#760000"))

p2 <- ui_industry %>%
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


# Data Transformation / Plots on Industry-level ---------------------------

## Find total claims/month for each industry (by year)
ui_industry <- ui_industry %>%
  dplyr::group_by(month, year, industry) %>%
  dplyr::mutate(industry_month = sum(claims, na.rm=TRUE)) %>%
  ungroup() %>%
  dplyr::group_by(year, industry) %>%
  dplyr::mutate(
    industry_year = sum(claims, na.rm=TRUE))
  

## Transform year from continuous to discrete (factor)
ui_industry$year_fact <- cut(ui_industry$year, breaks = c(2004:2020), 
                              labels = c(2005:2020))
## Barplot comparing number of claims/month 2019 v. 2020 for Construction industry 
ui_industry %>%
  filter(industry == "construction",
         year > 2018) %>%
  ggplot(aes(month_abbr, industry_month, fill = year_fact)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Construction UI Claims per Month",
       subtitle = "2019 vs. 2020",
       fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_blank())



# Data Transformation/Plotting on % Industry level ------------------------


## Find each industry's percentage of total claims by year
ui_industry <- ui_industry %>%
  dplyr::mutate(
    industry_pct = round(((industry_year/total_year) * 100), 1))


## Plot Comparing percentage of claims for 5 industries, 2019 v 2020
## This is definitely not the best way to visualize this info--just a prelim. plot
ui_industry %>%
  filter(year > 2018) %>%
  filter(industry %in% c("construction","wholesale_trade",
                         "retail_trade","real_estate","manufacturing",
                         "self_employed")) %>%
  ggplot(aes(industry, industry_pct, fill = year_fact)) +
  geom_col(position="dodge") +
  labs(title = "Which Industries have been most Affected by COVID?",
       subtitle = "Industry Percentage of Total Claims 2019 vs. 2020",
       fill = "Year",
       y = "Industry") +
  #change x axis labels -- is there a function to do this rather than manual?
  scale_x_discrete(labels = c("Construction","Manuf.","Real Estate",
                              "Retail","Self-Employed.","Wholesale")) +
  geom_text(aes(label = industry_year), size = 3) + ## need to move this over
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,
                                   size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

## Plot of Trends in Total Claims / % of total claims for a given industry
ui_industry %>%
  filter(industry == "construction") %>%
  ggplot(aes(new_claim_date, log(total))) +
  geom_line()
ui_industry %>%
  filter(industry == "construction") %>%
  ggplot() +
  geom_line(aes(year, industry_pct)) 

## Pie chart of % Total Claims by industry
ui_industry %>%
  filter(year == 2020,
         industry_pct > 4) %>%
  ggplot(aes(industry, industry_pct)) +
  geom_bar(stat = "identity", aes(fill = industry)) +
  coord_polar("x", start = pi) +
  theme(legend.position = "none")

## Facet grid for percent trends by industry
ui_industry %>%
  ggplot(aes(year, industry_pct)) +
  geom_line() +
  facet_wrap(~name)

## Facet grid total claims by industry
ui_industry %>%
  #filter(industry != "self_employed") %>%
  ggplot(aes(year, industry_year)) +
  geom_line() +
  facet_wrap(~name) +
  labs(title = "Total UI Claims by Industry",
       y = "Claims per year") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
  

# Dumbbell Chart ----------------------------------------------------------
## VISION
# Compare average # claims pre 2020 to # claims in 2020
# problem-- need to eliminate december (bc not yet reported for 2020)


## Find average number of claims per industry from 2005-2019
ui_industry2 <- ui_industry %>%
  dplyr::filter(year < 2020) %>%
  dplyr::group_by(industry) %>%
  mutate(avg_05_19 = round(mean(industry_year),0))

ui_industry_2020 <- ui_industry %>%
  filter(year == 2020) %>%
  mutate(avg_05_19 = industry_year)

ui_comb <- rbind(ui_industry2, ui_industry_2020)

ui_comb %>%
  filter(year %in% c(2005,2020)) %>%
  filter(industry == "construction") %>%
  ggplot(aes(x=avg_05_19)) +
  geom_bar()

## RENAMING INDUSTRIES
library(stringr)

# replace _ with a space 
ui_industry$name <- stringr::str_replace_all(ui_industry$industry, "_"," ")
ui_industry$name <- stringr::str_to_title(ui_industry$name) #converts first letter of each word to uppercase


# Interactive Datatable using DT package ----------------------------------
## BIGGEST THING-- FIND WAY TO SUMMARIZE 
## multiple values are still being displayed for each year

library(DT)

## Clean up for datatable
ui_industry2 <- ui_industry %>%
  select(Year = year, Month = month_abbr, 
         Industry = industry, Claims = claims) %>%
  dplyr::group_by(Month, Year, Industry) %>%
  summarize(Month_Total = sum(Claims))


## For % Industry table
ui_industry3 <- ui_industry %>%
  dplyr::select(Year = year,
                Industry = industry, Total = industry_year,
                Percent = industry_pct)
## Filtering is easier when year is a character
ui_industry3$Year = as.character(ui_industry3$Year)


DT::datatable(ui_industry3,
              rownames = FALSE,
              filter = "top",
              #colnames = c("Total" = "total_month"),
              extensions = "Buttons",
              options = list(dom = "Bfrtip",
                             buttons = c("csv","excel","pdf")))

install.packages("shinyWidgets")
library(shinyWidgets)
