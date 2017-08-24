
# libraries ####

library(data.table)
library(tidyverse)
library(stringr)
library(scales)
library(forcats)
library(survey)
library(gridExtra)
library(countrycode)
library(readr)
library(readxl)
library(DT)
library(plotly)
library(zoo)

# Get list of available surveys ####

  dhs = "../DHS/"
  
  source( paste0( dhs, "file_list.r") )
  
  subsahara = countrycode_data %>%
    filter( region %in% c("Middle Africa", "Western Africa", "Eastern Africa", "Southern Africa")) %>% 
    select( country.name, iso3c ) %>%
    filter( !country.name %in% "Lesotho") 
  
  
  files = files %>% 
    mutate( country.name = countrycode(country, "country.name", "country.name")) %>%
    filter( survey!="", year >= 2000 , country.name %in% subsahara$country.name) %>% 
    count(country, survey, year) %>% select(-n)

  # View(files)
  
# select survey as row number: ####
  i = 37 # e.g. liberia files are 37:39
  .country = files[i, ]$country
  .survey = files[i, ]$survey
  .year = files[i, ]$year

# select variables ####

  if (!exists("dd")) dd = readRDS( 'dictionaryNew.RDS')

# View(dd)

individual_vars  = c( 
  'hml32', # microscopy
  'hml35', # RDT 
  'hml1', # Number of mosquito nets
  "hml4", # Months ago obtained mosquito net
  "hml5", # net Was net treated with insecticide when bought
  "hml6", # Mosquito net treated with insecticide
  "hml7", # Brand of bednet
  "hml8",  # Bednet treated since receiving
  'hml19', 'hml20', 'hml12', # slept under llin
  'hv102', # de jure member of household
  'hv103', # de facto memeber of household (slept there last night)
  'hml16', # corrected age in years
  "hv253", # dwelling sprayed (some multiple responses)
  
  "hv000",  # country and survey phase
  "hv001", # Cluster number
  "hv002", #Household number
  'HV003', # Respondent's line number answering Household q
  'HV005', # sample weight
  
  
  "HV021", # psu: enumeration area or sample cluster
  "HV022", # Sample stratum number
  "hv023", # STRATA
  "hv024", # region
  "HV025" , # type of residence : urban rural
  
  'HHID', 'HVIDX',
  "HV008", # date of interview
  "HV270", # wealth index
  "latitude", "longitude"
)

variables <- unique( toupper( individual_vars) )
variables = variables[ order(variables) ]

# translation table of dhs file codes to file names
filecodes = data_frame( abrev  = c("kr", "br", "ir", "pr", "hr", "hr" ), 
                        file = c("Children's Recode","Supplemental Births Recode",
                                 "Individual Recode",
                                 "Household Member Recode", "Household Recode", 
                                 "Supplemental Household Recode")) 

variable_defn = dd %>% 
  filter( country %in% .country, survey %in% .survey, year %in% .year) %>%
  filter( Item_Name %in% variables ) %>%
  filter( !grepl( "NA", Item_Label, ignore.case = FALSE, fixed = TRUE) ,
          !grepl( "Presence", Item_Label, ignore.case = FALSE, fixed = TRUE)
  ) %>%
  left_join( filecodes , by = "file", copy = TRUE) %>%
  mutate( 
    abrev = ifelse( is.na(abrev) , tolower( substr(file, 3, 4) ) , abrev)
  ) %>%
  left_join( filecodes , by = "abrev", copy = TRUE) %>%
  rename( file = file.y ) %>%
  filter( grepl( "member", file, ignore.case= T)) %>%
  count( Item_Name, Item_Label) %>%
  select( -n) %>%
  arrange( Item_Label ) %>%
  as.data.frame()

variable_defn


# load survey design ####

source( paste0( dhs, "load_survey_file.R") )

.file =  "Household Member Recode"

svy <- try( silent = TRUE,
            load_survey_file( design = TRUE,
                                # printout = TRUE ,  # un comment when testing
                                vars = variables, 
                                .country = .country, 
                                .survey = .survey,
                                .year = .year,
                              .file = .file
                              )
)

# TODO: FIGURE OUT WAY TO UPDATE SVY DESIGN OBJECT BY PASSING CODE
# library(pryr)
# update.defn = substitute( x = hv001)
# svy = update.survey.design(svy, update.defn )

# class( svy )

# Get survey estimates ####
  
  .var = 'hml32'  # RDT
  .by = 'hv000'  # country
  
  # create/modify variables
  svy = update( svy, one = 1 )
  svyby( ~one, ~hml35, svy , svymean, na.rm = TRUE )
  
  svy = update( svy, rdt = ifelse(hml35 %in% 0:1, hml35, NA) )
  .var = 'rdt'
  
  #TODO: FORMAT RETURN FOR CASES WHERE .VAR OR .BY DO NOT EXIST IN DATASET

   sb =svyby( as.formula(paste("~", tolower(.var))) , 
           by = as.formula(paste("~", tolower(.by))), 
           svy, 
           svymean, na.rm = TRUE )

  sb$upper = sb[, names(sb)[2]] + 1.96*sb[, names(sb)[3]]
  sb$lower = sb[, names(sb)[2]] - 1.96*sb[, names(sb)[3]]
  
  # sb
  
  # add labels for by variable
  dictionary = dd %>% filter(country %in% .country, 
                                      survey %in% .survey,
                                      year %in% .year,
                             file %in% .file,
                             Item_Name %in% variables)
  
  dictionary.by = filter(dictionary, Item_Name == toupper(.by))
  dictionary.strata1 = filter(dictionary, Item_Name == "HV024")
  dictionary.strata2 = filter(dictionary, Item_Name == "HV025")
  
  # View(dictionary)
  
  if (.by %in% 'strata'){ 
    
      sb.d = separate(sb, strata, c("strata1", "strata2"), " ") 
      
      sb.d$strata1 = dictionary.strata1[ match(sb.d$strata1, 
                                               dictionary[ dictionary$Item_Name == "HV024", "value"] ),]$label
      
      sb.d$strata2 = dictionary.strata2[match(sb.d$strata2, 
                                              dictionary[ dictionary$Item_Name == "HV025", "value"]),]$label
      
  } else {
    
    sb.d = sb
    
    sb.d[, .by] = dictionary.by[ match(sb.d[, .by], 
                                       dictionary[ dictionary$Item_Name == toupper(.by), "value"] ),]$label
  }
    
  
  sb.d
  
  