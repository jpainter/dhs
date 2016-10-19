# loop through dhs data folders

# populated by three subfolders
folders <- list.dirs("../DHS", full.names = TRUE)

files = NA

for ( i in 2:length(folders) )  {
  x = folders[i]
  
  # ignore .git and _DHS folders
  if ( substring( strsplit( x, "/")[[1]][3] , 1, 1) %in% c(".", "_") ) next() 
  
  # get list of files in folder
  number = length(list.files(x) )
  x.files = data.frame( folder = rep(x, number),
                        file = list.files(x),
                        stringsAsFactors = FALSE
  )
  
  if (!is.data.frame(files)) {
        files = x.files 
  } else {
        files = rbind( files, x.files)
  }

}

nrow(files)

# test if file is directory or file..
library(dplyr)

files = files %>%
  filter( !dir.exists( paste0(folder, "/", file)) ) %>%
  mutate( 
    country = unlist( sapply( strsplit( folder, "/", fixed = TRUE), function(x) x[3]) ),
    survey_year = unlist( lapply(strsplit( folder, "/", fixed = TRUE), function(x) x[4] ) ),
    gis = lapply( strsplit( folder, "/", fixed = TRUE), function(x) x[4] ) %in% "Supplemental"
)  %>%
  mutate(
        survey = sapply( strsplit(survey_year, " ", fixed = TRUE), function(x) paste( x[-length(x)], collapse = " ") ) ,
        year = sapply( strsplit(survey_year, " ", fixed = TRUE), function(x) tail(x, 1))
  )

# fix survey and year for newfile, which are in different format (year_survey)
newfiles = which(files$year == files$survey_year)

years =  sapply( strsplit( files[newfiles, "survey_year"], "_", fixed = TRUE), function(x) head(x, 1))
new_years = which( grepl("\\d", years)) 
files[ newfiles[new_years] , "year"] = years[new_years]

survey = sapply( strsplit( files[newfiles, "survey_year"], "_", fixed = TRUE), function(x) tail(x, 1))
files[ newfiles[new_years] , "survey"] = survey[new_years]
 


# nrow(files)

# View(files)

