
library(tidyverse)
library(readr)
csb_mapped_2014 <- read_csv("~/Dropbox/_Malaria/Projects/WMR/csb_final_mapped_17Oct2014.csv")
View(csb_mapped_2014)

str(csb_mapped)

csb_mapped_2014 %>% count( iso3, startyear, endyear) %>% arrange(-endyear, -startyear, iso3)


csb_mapped_2014 %>% count( varlab, care_level ) %>% spread( care_level , n) %>%  
  gather( care_level, n, -varlab) %>% filter( n>0 ) %>%
  View()
