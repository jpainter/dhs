load("C:/Users/bzp3/dropbox/_Malaria/Projects/DHS/data_dictionary.rda")

library(dplyr)
library(tidyr)

# where to find Malaria test results? ####

malaria_variables = data_dictionary %>%
  filter( grepl("malaria", value, ignore.case = TRUE),
          !grepl("NA - ", value, ignore.case = FALSE)) 

mv = malaria_variables %>%
  count(var, value) %>%
  filter( n>=30) %>%
  as.data.frame() 

mv %>% View()

# HML 32,33, and 35 are listed on both household and individual files. 
# Show country/survey with these...

malaria_variables %>%
  filter( var %in% c("HML32", "HML33", "HML35", "ML2")) %>%
  count(country, survey_year, var, file) %>%
  spread(file, n) %>% 
  arrange(var) %>%
  View()

# HML varabiables are always found in Household Member Recode file;
# ML variables are found in childrens and individual recode


# where to find bednet results? ####

bednet_variables = data_dictionary %>%
  filter( grepl("bednet", value, ignore.case = TRUE) | grepl("net", value, ignore.case = TRUE),
          !grepl("NA - ", value, ignore.case = FALSE)) 

bd = bednet_variables %>%
  count(var, value) %>%
  filter( n>=30) %>%
  as.data.frame() 

bd %>% View()


# Important varaiables are HML 32,33, and 35 
# Show country/survey with these...

bednet_variables %>%
  filter( var %in% c("HV227", "HV228", "V461", "HML1", "V459", "V460", "V461", "HML10", "HML12")) %>%
  count(country, survey_year, var, value, file) %>%
  spread(file, n) %>% 
  arrange(var) %>%
  View()
  
# HML varabiables are always found in Household Member , or Household Recode file;
# V variables are found in childrens and individual recode


# where to find fever results? ####

fever_variables = data_dictionary %>%
  filter( grepl("fever", value, ignore.case = TRUE) ,
          !grepl("NA - ", value, ignore.case = FALSE)) 

fv = fever_variables %>%
  count(var, value) %>%
  filter( n>=30) %>%
  as.data.frame() 

fv %>% View()


# Important varaiables are HML 32,33, and 35 
# Show country/survey with these...

fever_variables %>%
  filter( var %in% c("H22", "H32A", "H37A", "H46A", "ML11")) %>%
  count(country, survey_year, var, value, file) %>%
  spread(file, n) %>% 
  arrange(var) %>%
  View()

# HML varabiables are always found in Household Member Recode file;
# V variables are found in childrens and individual recode
# H variables are found in childrens and individual recode
# ML variables are found in childrens and individual recode
