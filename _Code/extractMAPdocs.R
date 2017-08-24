
# extract info from MAP files and create a data dictionary


source("parseMAPfile.R")

# get list of surveys and files ####

require(dplyr)
require(readr)
require(tidyr)
require(tibble)
require(countrycode)

source("file_list.r")

# get map files for files not already in dictionary
# xxx data.frame of files not joined to in dhs_rx_wmr.R, wmr_2016_csb_ver2, etc
# f = files %>% 
#   inner_join( xxx, 
#                               by = c('country' = 'country', 'survey' = 'survey', 'year' = 'year')
#                               ) %>%
  # filter( grepl( "Children", file ) | grepl( "kr", file ) )

f = files  # all files...

isMAP = grepl(".MAP", f$file)

mapFiles = f[ isMAP, ] %>% mutate( 
  name =  paste0(folder, "/", file),
  name = gsub( "../DHS/" , "" , name )
  # ,
  # file = sapply( file, FUN = function(x) unlist(strsplit(x, ".MAP"))[1] )
  ) %>%
  separate( file, "file.name", sep = ".MAP", remove = FALSE)

# translate coded file names
filecodes = data_frame( abrev  = c("kr", "br", "ir", "pr", "hr", "hr" ),
                        full = c("Children's Recode","Supplemental Births Recode","Individual Recode",
                                 "Household Member Recode", "Household Recode", "Supplemental Household Recode"))

coded.name = grepl(" " , mapFiles$file.name, fixed = TRUE)

# redo completely?  Overwrite = TRUE
overwrite = FALSE

# if overwrite = TRUE, then get all files in mapFiles
# otherwise, get only those not in the dictionary
if (overwrite){
  mf = mapFiles
  
} else {
  dd = readRDS( "dictionaryNew.rds" )
  mf = anti_join( mapFiles, dd, by = c("country", "survey", "year", "file.name" = "file")) %>%
    filter( year >=2000 )
}

# View(mf)

# loop through map files and convert to .rda files


.NameThenLabel  = TRUE  # for nearly all
# .NameThenLabel  = FALSE  # for a few (Benin 2006, DRC 2007, MADA 2008, Zam 2007)

new_dictionaries = list()
n = nrow(mf)
p =  progress_estimated(n, 0)

for (i in 1:n){ # n
 
  p$pause(0.1)$tick()$print()

  .country = unlist(mf[i, "country"] )
  .iso3c = countrycode( .country, "country.name", "iso3c")
  .survey = unlist(mf[i, "survey"])  
  .year = unlist(mf[i, "year"]) 
  .file = unlist(mf[i, "file"])
  
  cat( paste(.country, .survey, .year, .file ) )
  
  # if (overwrite == FALSE){
  #   in.dictionary = filter(dictionary_new, country == .country, survey == .survey, year == .year, file == .file)
  #   if ( nrow(in.dictionary) > 0 ) cat("...exists\n"); next
  # }
  
  d =  try(  parseMAPfile( mapFile = unlist( mf[i, "name"] ), 
                           NameThenLabel = .NameThenLabel ) )
  
  if ( is.null(d) ) next() 
  
  if ( class(d) == "try-error" ) next() 
  
  d = d %>%
    mutate( country = .country  ,
            iso3c = .iso3c , 
            survey = .survey  ,
            year = .year ,
            file = .file
            )
  
  new_dictionaries[[i]] = d
}

new_dictionaries = rbindlist(new_dictionaries, fill = TRUE) 
nrow(new_dictionaries)
View(new_dictionaries)

# if overwrite == FALSE, combine with original dictionary
if (overwrite ){
  dictionary_new = new_dictionaries
} else {
  dictionary_new = bind_rows(dd, new_dictionaries)
}

saveRDS( dictionary_new, file = "dictionaryNew.rds" )

# combine with old dictionary
# dictionary = readRDS("dictionaryNew.rds")
# newD = bind_rows( dictionary, dictionary_new)
# saveRDS(newD, file = "dictionaryNew.rds")

######
dd = readRDS("dictionaryNew.rds") # about 5 sec
# View(dd)

# library(feather)
# write_feather( dictionary, "dictionary.feather")
# dd = read_feather("dictionary.feather") # about 2 sec
# 
# View(dd)

#### summaries ####
# countries
unique(dd$country) #47
unique(dd$iso3c) #46

#surveys
dd %>% count(country, survey, year) %>% select(country, survey, year) #174

# Mean number of variables
dd %>% group_by(country, survey, year, file) %>% distinct(Item_Name) %>% summarise(n = n())

# surveys with malaria parasitemia results
dd %>% mutate( malaria = 
                 grepl("malaria", Item_Label, fixed = TRUE) &
                 !grepl("NA", Item_Label, fixed = TRUE)
               ) %>% 
  group_by(country, survey, year) %>%  summarise( malaria = max(malaria)) %>% 
  arrange( country, year) %>% View()  # 126 of 164 mention malaria

## selection of some vars
vars = c('HV253', 'V461', 
         'HML1', 'ML101', 'ML0', 'V459', 'HV227' , # no. nets, etc.
         'HV009', 'HV012', 'HV013', # number of persons sleeping (ratio with # nets)
         'H47', 'H37F', 'H37G' , 'H37H', 
         'ML13F', 'ML13G', 'MLH', 'M49A', 
         'S307A',
         'V455', 'HA55', 'HB55', 'HC55', 'HW55' , # hemoglobin
         'HML32', 'HML33', 'HML34', 'HML35' # malaria results
)


# list malaria variables and associated values

malaria_vars = dd %>% mutate( malaria = 
                 (grepl("malaria", Item_Label, fixed = TRUE) | Item_Name %in% vars ) &
                 !grepl("NA", Item_Label, fixed = TRUE) 
               ) %>% filter( malaria == TRUE , 
                             !grepl("^S", Item_Name) # exclude name of 'special' vars
                             ) %>% 
  group_by(Item_Name, Item_Label, file) %>% 
  summarise( values = paste(unique(value), collapse = "; "),
             surveys = n_distinct(country, survey, year)) %>%
  mutate( of_interest = Item_Name %in% vars)

View(malaria_vars)


dd %>% filter( Item_Name %in% vars) %>% distinct(Item_Name, Item_Label, file) %>% 
  arrange(Item_Name, file, Item_Label) %>% View()




