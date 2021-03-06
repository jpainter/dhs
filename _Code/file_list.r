# loop through dhs data folders

survey_files = function( dir = '../DHS/_Surveys' ){
library(tidyverse)
library(stringr)
library(data.table)

# populated by three subfolders
folders <- list.dirs( dir , full.names = TRUE)

files = list()

for ( i in seq_along(folders) )  {
  
  x = folders[i]
  
  if ( folders[i] == "../DHS" ) next()
  
  # ignore .git and _DHS folders
  if ( substring( strsplit( x, "/")[[1]][4] , 1, 1) %in% c(".", "_") ) next() 
  
  # get list of files in folder
  number = length(list.files(x) )
  x.files = data.frame( folder = rep(x, number),
                        file = list.files(x),
                        stringsAsFactors = FALSE
  )
  
  files[[i]] = x.files

}

files = rbindlist(files, fill = TRUE) 

nrow(files)

# test if file is directory or file..


files = files %>% filter( !dir.exists( paste0(folder, "/", file)) )

files$country = unlist( lapply( str_split( files$folder, "/"), function(x) x[4]) )

files$survey_year = unlist( lapply( str_split( files$folder, "/"), function(x) x[5]) )

files$gis = sapply( str_split( files$folder, "/"), function(x) x[5]) %in% "Supplemental"

files$survey = sapply( str_split( files$survey_year, " "), function(x) paste( x[-length(x)], collapse = " ") )
                       
files$year  = sapply( str_split( files$survey_year, " "), function(x) tail(x, 1) ) 


# fix survey and year for newfile, which are in different format (year_survey)
newfiles = which(files$year == files$survey_year)

years =  sapply( strsplit( 
  sapply( files[newfiles, "survey_year"], as.character) , "_", fixed = TRUE) ,  
  function(x) head(x, 1)
  )

new_years = which( grepl("\\d", years)) 

files[ newfiles[new_years] , "year"] = years[new_years]

survey = sapply( strsplit( 
  sapply( files[newfiles, "survey_year"], as.character) , "_", fixed = TRUE) , 
  function(x) tail(x, 1))

files[ newfiles[new_years] , "survey"] = survey[new_years]

return( files )

}

# TEST
# files = survey_files(); View(files)
