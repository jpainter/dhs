# loop through dhs data folders

# populated by three subfolders
folders <- list.dirs(full.names = TRUE)

files = NA

for ( i in 2:length(folders) )  {
  x = folders[i]
  if ( substring(x, 3, 3) %in% c(".", "_") ) next()
  
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
  filter( !dir.exists( paste0(folder, "/", file))) %>%
  mutate( 
    country = unlist( lapply( strsplit( folder, "/", fixed = TRUE), function(x) x[2]) ),
    survey_year = unlist( lapply(strsplit( folder, "/", fixed = TRUE), function(x) x[3] ) ),
    gis = lapply( strsplit( folder, "/", fixed = TRUE), function(x) x[4] ) %in% "Supplemental"
)  %>%
  mutate(
        survey = sapply( strsplit(survey_year, " ", fixed = TRUE), function(x) x[-length(x)]),
        year = lapply( strsplit(survey_year, " ", fixed = TRUE), function(x) tail(x, 1))
  )

nrow(files)

View(files)

