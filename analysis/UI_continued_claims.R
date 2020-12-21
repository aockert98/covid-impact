## Date created: Dec 21, 2020

## Objective: Further breakdown of UI claims

## Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)

## Load continued_claims_industry
# mine isn't opening for some reason right now so I'm using file.choose()
cui <- read.csv(file.choose())
dplyr::glimpse(cui)


# Data Cleaning and Transformation ----------------------------------------

## Change Date to date format
cui$Date <- lubridate::mdy(cui$Date) #2 failed to parse--I think it's because claims after Nov. 21 are incomplete

## Convert Agriculture, Mining, and Utilities to characters for a moment
## Essentially, we cannot use pivot_longer when some Industry claims are integers, and other characters
## Would be extremely tedious to remove commas from a ton of different columns
## Much easier to convert these 3 to characters, pivot longer, and then remove commas from the single "contClaims" column
cui$Agric...Forestry..Fishing...Hunting <- as.character(cui$Agric...Forestry..Fishing...Hunting)
cui$Mining.Quarrying <- as.character(cui$Mining.Quarrying)
cui$Utilities <- as.character(cui$Utilities)

## Transform data wide to long
## Creating 2nd dataframe in case something goes wrong, can revert back to original "cui"
cui2 <- pivot_longer(cui, names_to = "Industry", values_to = "contClaims",
                     Agric...Forestry..Fishing...Hunting:Other.Unknown)

## Remove commas from contClaims column, and convert to integer
cui2$contClaims <- str_remove_all(cui2$contClaims, ",")
as.integer(cui2$contClaims)

## Repeat process for "Total" column
cui2$Total <- stringr::str_remove_all(cui2$Total, ",")
as.integer(cui2$Total)


