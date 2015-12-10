

# run this one time to get data set

source("file_list.r")  

# get all slide/rdts 
map_files = files %>% 
  mutate( rda = grepl('map.rda', file)  ) %>%
  filter(rda)

View(map_files)

map_files %>% count()
map_files %>% count(country)
map_files %>% count(country, survey_year)

# function to read map data
map_data = function(
  folder = "../DHS/Benin/Standard DHS 2006", 
  file = "Individual Recode.map.rda"
  ){

  f = paste0(folder, "/", file) 
  print(f); flush.console()
  load( f ) # file will be loaded as 'map'
  return(map)
}

# initialize data dictionary
data_dictionary = data.frame()

# iterate through 1:nrow(household_member_file)
for ( i in 1:nrow(map_files) ){
    s = try(
      map_data(
        folder = map_files[i, "folder"],
        file = map_files[i, "file"])
    )
    
    s$Len =  as.integer(s$Len)
    
    s$country = map_files[i, "country"]
    s$survey_year = map_files[i, "survey_year"]
    s$file = head(strsplit(map_files[i, "file"], ".map.rda")[[1]])

    
    if (i == 1)  {data_dictionary = s 
    } else {
      data_dictionary = bind_rows(
        data_dictionary, s
      )
        
    }
    
}

View(data_dictionary)

save(data_dictionary, file = "data_dictionary.rda")

# create meata data of number of vaariables in each file
data_dictionary_metadata = data_dictionary %>% count(country, survey_year, file)

View(data_dictionary_metadata)

save(data_dictionary_metadata, file = "data_dictionary_metadata.rda")

#  surveys with malaria...
malaria_survey = data_dictionary %>% 
  filter( grepl("malaria", value, ignore.case = TRUE),
          !grepl("NA - ", value, ignore.case = FALSE)) 

View(malaria_survey)
# 7,724 question

malaria_survey %>% count(country) %>% as.data.frame()
# 43 countries have malaria question

malaria_survey %>% count(country, survey_year) %>% as.data.frame()
# 136 surveys; an average of 3.2 per country
  