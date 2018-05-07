# DHS Surveys on website ####
# http://dhsprogram.com/data/available-datasets.cfm
library(tidyverse)
library( countrycode )
library(stringr)

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

country = word(survey, 1, -2)
year =  word(survey, -1)

dhs = data_frame( survey, country , year ,
                  type, phase, recode, survey_data, gps_data, bio_data, spa_data ) %>%
  mutate( 
    iso3 = countrycode( country, "country.name", "iso3c") 
    ) 

dhs = dhs %>%
  mutate( 
    region = codelist[ match(dhs$iso3, codelist$iso3c), 'region']
  )

# View(dhs)

### Datasets already downloaded ####
dhs_downloads = "../DHS/_Surveys/_zip_download/"
dhs_surveys = "../DHS/_Surveys/"
dhs_code = "../DHS/_Code/"

### DHS Country Codes (https://www.dhsprogram.com/pubs/pdf/DHSG4/Recode6_DHS_22March2013_DHSG4.pdf) ####
cc = tribble(
  ~country , ~code , 
  'Afghanistan' ,  'AF'  , 
  'Haiti' ,  'HT' ,
  'Niger'  , 'NI' ,
  'Angola'  , 'AO' ,
  'Honduras'  , 'HN'  ,
  'Nigeria' ,  'NG' ,
  'Armenia'  , 'AM' , 
  'India'  , 'IA' ,
  'Pakistan'  , 'PK' ,
  'Azerbaijan'  , 'AZ' ,
  'Indonesia'  , 'ID'  ,
  'Peru'  , 'PE' ,
  'Bangladesh'  , 'BD' ,  
  'Jordan'  , 'JO'  , 
  'Rwanda'  , 'RW' , 
  'Benin'  , 'BJ'  , 
  'Kenya'  , 'KE'  , 
  'Senegal'  , 'SN' , 
  'Burundi'  , 'BU'  , 
  'Kyrgyz Republic'  , 'KY' ,  
  'South Africa'  , 'ZA' , 
  'Cambodia'  , 'KH'  , 
  "Lao People's Dem. Rep." ,  'LA'  , 
  'Swaziland'  , 'SZ' , 
  'Colombia'  , 'CO'  , 
  'Lesotho'  , 'LS'  , 
  'Tajikistan'  , 'TJ' , 
  'Congo (Brazzaville)' , 'CG' ,  
  'Liberia'  , 'LB'  , 
  'Tanzania'  , 'TZ' , 
  'Congo Dem. Rep.'  , 'CD'  , 
  'Madagascar'  , 'MD'  , 
  'Timor-Leste'  , 'TP' , 
  "Cote d'Ivoire" , 'CI' ,  
  'Malawi'  , 'MW'  , 
  'Uganda'  , 'UG' , 
  'Egypt'  , 'EG'  , 
  'Mali'  , 'ML'  , 
  'Yemen'  , 'YE' , 
  'Ethiopia'  , 'ET'  , 
  'Mauritania' ,  'MR' ,  
  'Zambia'  , 'ZM' , 
  'Gabon'  , 'GA'  , 
  'Mozambique'  , 'MZ'  , 
  'Zimbabwe'  , 'ZW' , 
  'Gambia'  , 'GM'  , 
  'Namibia'  , 'NM'    ,  
  'Guinea'  , 'GN'  , 
  'Nepal'  , 'NP'  
  
)


# find zip files and unzip  ####
source( paste0( dhs_code, "file_list.r") ) 

files = survey_files() %>%
  mutate( iso3 = countrycode( country, "country.name", "iso3c") ) %>%
  count( country , iso3, survey, year )

# View( files )

# which surveys are available, but not prepared

subsahara = codelist %>%
  filter( region %in% c("Middle Africa", "Western Africa", "Eastern Africa", "Southern Africa")) %>%
  rename( country.name = country.name.en) %>%
  select( country.name, iso3c )

missing = anti_join( dhs, files, by = c("iso3", "year") ) %>% 
  filter( iso3 %in% subsahara$iso3c ,
          year >2000 
          )
glimpse( missing )
View(missing)

available = missing %>% 
  gather( survey_type, availability , survey_data, gps_data, bio_data, spa_data ) %>%
  filter(availability %in% 'Data Available' , !type %in% 'SPA') %>%
  arrange( country, desc( year ) )
View( available )
## Sort by survey_data to see which surveys are publicly available, and gps_data to 
## see which can be mapped


