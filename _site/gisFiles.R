

# find folders with GIS data:
folders <- list.dirs(full.names = TRUE)
gis = grepl("Supplemental/flat ascii (.dat)", folders, fixed = TRUE)
folders[gis]

folder = folders[gis][1]
files = list.files(folder)
shp.file = files[ grepl(".shp", files, fixed = TRUE)][1]
shp.name = strsplit(shp.file, ".", fixed = TRUE)[[1]][1]

library(rgdal)
shapefile = readOGR( folder, shp.name )

# geo file variable DHSID links with Household Member Recode variable hv001 (cluster number)
library(dplyr)
library(stringr)
# NB: shapefile has longe character string for cluster (AO200600000116), 
# where as household cluster is numeric (e.g. 116)

cluster = shapefile %>% 
  as.data.frame() %>%
  select( DHSID, LATNUM, LONGNUM) %>% 
  mutate( hv001 = as.numeric(str_sub(DHSID, start= -4)),
          lat = LATNUM,
          long = LONGNUM) %>%
  select( hv001, lat, long)

# Read household_member_file as x (click on file)
new_x = x %>% inner_join(cluster, by = "hv001")
  
