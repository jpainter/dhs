# DHS Surveys on website
# http://dhsprogram.com/data/available-datasets.cfm
library(tidyverse)

library(rvest)
dhs_list <- read_html("http://dhsprogram.com/data/available-datasets.cfm")
str(dhs_list)

survey = dhs_list %>% 
  html_nodes("#survey a:nth-child(1)") %>%
  html_text() 
survey

type = dhs_list %>% 
  html_nodes(".altwhite2:nth-child(2)") %>%
  html_text() 
type

phase = dhs_list %>% 
  html_nodes(".altwhite2:nth-child(3)") %>%
  html_text() 
phase

recode = dhs_list %>% 
  html_nodes(".altwhite2:nth-child(4)") %>%
  html_text() 
recode

survey_data = dhs_list %>% 
  html_nodes(".altwhite2:nth-child(5)") %>%
  html_text() 
survey_data

gps_data = dhs_list %>% 
  html_nodes(".altwhite2:nth-child(6)") %>%
  html_text() 
gps_data

bio_data = dhs_list %>% 
  html_nodes(".altwhite2:nth-child(7)") %>%
  html_text() 
bio_data

spa_data = dhs_list %>% 
  html_nodes(".altwhite2:nth-child(8)") %>%
  html_text() 
spa_data

library(stringr)
country = word(survey, 1, -2)
year =  word(survey, -1)

library(countrycode)
dhs = data_frame( survey, country , year ,
                  type, phase, recode, survey_data, gps_data, bio_data, spa_data ) %>%
  mutate( 
    iso3 = countrycode( country, "country.name", "iso3c") 
    ) 

dhs = dhs %>%
  mutate( 
    region = countrycode_data[ match(iso3, countrycode_data$iso3c), 'region']
  )

View(dhs)
