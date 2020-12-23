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

# Data Loading and Cleaning -----------------------------------------------

## read in "UI Claims by Industry" csv from Data folder
ui <- read.csv("data/economic/ui_claims_data.csv") %>% 
  dplyr::mutate(
    new_claim_date = stringr::str_sub(new_claim_date, start = 1, end = 10), 
    new_claim_date = as.Date(new_claim_date), 
    year = as.numeric(format(new_claim_date, "%Y")), 
    month = as.numeric(format(new_claim_date, "%m"))
  )

dplyr::glimpse(ui)

# Data Transformation and Value-Finding -----------------------------------

## Find total number of claims per month and per year
ui2 <- ui %>% 
  dplyr::select(-unnamed_column) %>% 
  dplyr::group_by(year, month) %>%
  dplyr::mutate(total_month = sum(total)) %>%
  ungroup() %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(total_year = sum(total)) %>% 
  dplyr::ungroup()

## Transform data from wide to long format
ui_industry <- ui2 %>%
  tidyr::pivot_longer(
    cols = agric_forestry_fishing_hunting:other_unknown,
    names_to = "industry", 
    values_to = "claims")
## Add month abbr column
ui_industry$month_abbr <- lubridate::month(ui_industry$new_claim_date, label = TRUE)


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

## By percent of total claims
## UGLY-- just a test plot
ui_industry %>%
  dplyr::filter(year == 2020) %>%
  dplyr::mutate(pct = claims/total) %>%
  ggplot2::ggplot(aes(new_claim_date, pct, fill = industry)) +
  ggplot2::geom_path()


# Data Transformation / Plots on Industry-level ---------------------------

## Find total claims/month for each industry (by year)
ui_industry <- ui_industry %>%
  dplyr::group_by(month, year, industry) %>%
  dplyr::mutate(industry_month = sum(claims, na.rm=TRUE)) %>%
  ungroup() %>%
  dplyr::group_by(year, industry) %>%
  dplyr::mutate(
    industry_year = sum(claims, na.rm=TRUE)
  ) %>% 
  dplyr::ungroup()
  

## Transform year from continuous to discrete (factor)
ui_industry$year_fact <- cut(ui_industry$year, breaks = c(2004:2020), 
                              labels = c(2005:2020))

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

# replace _ with a space 
ui_industry$name <- stringr::str_replace_all(ui_industry$industry, "_"," ")
ui_industry$name <- stringr::str_to_title(ui_industry$name) #converts first letter of each word to uppercase

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
  facet_wrap(~name, scales = "free_y") +
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


# Interactive Datatable using DT package ----------------------------------
## BIGGEST THING-- FIND WAY TO SUMMARIZE 
## multiple values are still being displayed for each year

library(DT)

## Clean up for datatable
ui_industry2 <- ui_industry %>%
  select(Year = year, Month = month_abbr, 
         Industry = industry, Claims = claims) %>%
  dplyr::group_by(Month, Year, Industry) %>%
  summarize(
    Month_Total = sum(Claims), 
    .groups = "drop"
  )


## For % Industry table
ui_industry3 <- ui_industry %>%
  dplyr::select(Year = year,
                Industry = industry, Total = industry_year,
                Percent = industry_pct)
## Filtering is easier when year is a character
ui_industry3$Year = as.character(ui_industry3$Year)


ui_industry3 %>% 
  dplyr::group_by(Year, Industry) %>% 
  dplyr::summarise(
    Total = mean(Total), 
    Percent = mean(Percent), 
    .groups = "drop"
  ) %>% 
  DT::datatable(
    rownames = FALSE,
    filter = "top",
    #colnames = c("Total" = "total_month"),
    extensions = "Buttons",
    options = list(dom = "Bfrtip",
                   buttons = c("csv","excel","pdf")))


# Dumbbell Data Trans + Plot ----------------------------------------------
library(ggalt) #for dumbbell plot

ui_dumb <- ui_industry %>%
  select(year, industry, industry_year) %>%
  pivot_wider(names_from = "year",
              values_from = "industry_year",
              values_fn = mean) 

# Clean up industry names
ui_dumb$industry <- str_replace_all(ui_dumb$industry, "_", " ")
ui_dumb$industry <- str_to_title(ui_dumb$industry)

## Percent change column formula:
## [new (2020) - old (selected year)] / old (selected year)
ui_dumb <- ui_dumb %>%
  mutate(delta = (`2020`-`2013`)/`2013` * 100)

## Dumbbell plot

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


## Function to create delta col
## its not working thru function?? what might be wrong??

# seems like it might have something to do with the ` ` and year value

myfunc <- function(x, y) {
  ui_dumb2 <- ui_dumb %>%
    mutate(delta2 = ((ui_dumb$`y`-ui_dumb$`x`)/ui_dumb$`x`) * 100)
  print(ui_dumb2$delta2)
}

myfunc(2006,2019)


(ui_dumb$`2020` - ui_dumb$`2010`)/ui_dumb$`2010`

ui_dumb2 <- ui_dumb %>%
  mutate(delta2 = (ui_dumb$`2020` - ui_dumb$`2010`)/ui_dumb$`2010`)

ui_dumb2 <- ui_dumb2 %>%
  mutate(delta3 = (ui_dumb$`2020` - ui_dumb$`2012`)/ui_dumb$`2012`)

x = `2020`
x = 2020
         

