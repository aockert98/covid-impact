library(RSocrata)
library(dplyr)
library(readr)
library(here)


# DSS Application Activity ------------------------------------------------

# Read in the most recent data from the Socrata API as a data frame
RSocrata::read.socrata("https://data.ct.gov/resource/ymej-83fh.csv") %>% 
  # write the data frame out to the "data/dss_data.csv"; write out any NA values
  # in the data frame as empty strings
  readr::write_csv(
    path = here::here("data/dds_data.csv"), 
    na = ""
  )


# UI Claims by Industry ---------------------------------------------------

# Read in the most recent data from the Socrata API as a data frame
RSocrata::read.socrata("https://data.ct.gov/resource/r437-8xv7.csv") %>% 
  # write the data frame out to the "data/ui_claims_data.csv"; write out any NA values
  # in the data frame as empty strings
  readr::write_csv(
    path = here::here("data/ui_claims_data.csv"), 
    na = ""
  )


# COVID Age ---------------------------------------------------------------

# Read in the most recent data from the Socrata API as a data frame
RSocrata::read.socrata("https://data.ct.gov/resource/ypz6-8qyf.csv") %>% 
  # write the data frame out to the "data/covid_age_data.csv"; write out any NA values
  # in the data frame as empty strings
  readr::write_csv(
    path = here::here("data/covid_age_data.csv"), 
    na = ""
  )



  