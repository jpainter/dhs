

library(rgdal)
library(stringr)
library(dplyr)

# load survey ####

survey_GIS_data = function(
  country = NA , # "DRC"
  survey_year = NA
  )
  {

# test if GIS data:
  folder = paste0("../DHS/", 
                ifelse( country %in% "DRC", "Congo Democratic Republic", country),
                "/", survey_year) 
  
  # print(folder); flush.console()
  
  subfolders <- list.dirs(path =  folder, full.names = TRUE)
  
  gis = grepl("Supplemental/flat ascii (.dat)", subfolders, fixed = TRUE)
  
  if (sum(gis, na.rm = TRUE) == 0){ return(NA)}
  
  gisfolder =  subfolders[gis]
  
  files = list.files(gisfolder)
  shp.file = files[ grepl(".shp", files, fixed = TRUE)][1]
  shp.name = strsplit(shp.file, ".", fixed = TRUE)[[1]][1]
  
  shapefile = readOGR( gisfolder, shp.name, verbose = FALSE )

  # geo file variable DHSID links with Household Member Recode variable hv001 (cluster number) 
  # NB: shapefile has longe character string for cluster (AO200600000116), 
  # where as household cluster is numeric (e.g. 116)
  
  cluster = shapefile %>% 
    as.data.frame() %>%
    select( DHSID, LATNUM, LONGNUM) %>% 
    mutate( dhsid = as.numeric(str_sub(DHSID, start= -4)),
            latitude = LATNUM,
            longitude = LONGNUM) %>%
    select( dhsid, latitude, longitude) %>%
    filter( !(latitude == 0 & longitude == 0))
  
  return(cluster)
  }


# Test / Example
# g =  survey_GIS_data("Angola", "MIS 2011")  
  
        
