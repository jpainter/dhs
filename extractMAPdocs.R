
# extract info from MAP files and create a data dictionary


source("parseMAPfile.R")

# get list of surveys and files ####

require(dplyr)
require(readr)
require(tidyr)
require(tibble)

source("file_list.r")
files = as_data_frame(files)

isMAP = grepl(".MAP", files$file)

mapFiles = files[ isMAP, ] %>% mutate( 
  name =  paste0(folder, "/", file),
  name = gsub( "../DHS/" , "" , name ),
  file = sapply( file, FUN = function(x) unlist(strsplit(x, ".MAP"))[1] )
  ) 

# View(mapFiles)

# loop through map files and convert to .rda files

dictionary = NA
n = nrow(mapFiles)
progress =  progress_estimated(n, 0)

for (i in 1:n){ # n
 
  progress$tick()$print()
  
  d =  try(  parseMAPfile( mapFile = unlist(mapFiles[i, "name"]) ) )
  
  if ( is.null(d) ) next() 
  
  if ( class(d) == "try-error" ) next() 
  
  d = d %>%
    mutate( country = unlist(mapFiles[i, "country"] )  ,
            survey = unlist(mapFiles[i, "survey"])  ,
            year = unlist(mapFiles[i, "year"]) ,
            file = unlist(mapFiles[i, "file"])
            )
  
  if ( is.null(nrow(dictionary))  ){ 
    dictionary = d 
  } else {
      dictionary = bind_rows(dictionary, d)
    }
}

View(dictionary)
save(dictionary, file = "dictionary.rda")
dictionary = load("dictionary.rda") # about 5 sec

library(feather)
write_feather( dictionary, "dictionary.feather")
dd = read_feather("dictionary.feather") # about 2 sec

View(dd)

#### summaries ####
# countries
unique(dd$country) #46

#surveys
dd %>% count(country, survey, year) %>% select(country, survey, year) #164

# Mean number of variables
dd %>% group_by(country, survey, year, file) %>% distinct(Name) %>% summarise(n = n())

# surveys with malaria parasitemia results
dd %>% mutate( malaria = 
                 grepl("malaria", Item_Label, fixed = TRUE) &
                 !grepl("NA", Item_Label, fixed = TRUE)
               ) %>% 
  group_by(country, survey, year) %>%  summarise( malaria = max(malaria)) %>% 
  arrange( country, year) %>% View()  # 101 of 164 mention malaria

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




