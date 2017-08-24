

library(rgdal)
library(stringr)
library( dplyr )

# load survey ####

survey_GIS_data = function(
  .country = NA , # "DRC"
  .survey = NA,
  .year = NA,
  dhs_surveys = "../DHS/_Surveys/",
  dhs_code = "../DHS/_Code/"
  )
  {

# test if GIS data:
  folder = paste0( dhs_surveys ,
                ifelse( .country %in% "DRC", "Congo Democratic Republic", .country),
                "/", .survey, " ", .year)

  if (!dir.exists(folder)){
      folder =
          paste0(  dhs_surveys , .country, "/", .year, "_", .survey, "/"  )
  }

  if (!dir.exists(folder)) return()

  # print(folder); flush.console()

  subfolders <- list.dirs(path =  folder, full.names = TRUE, recursive = TRUE)
  subfolder_files = list.files( subfolders, recursive = TRUE )
  has.shp = grepl( ".shp", subfolder_files) & !grepl( ".shp.xml", subfolder_files)

  if (sum(has.shp) == 0) return(NULL)
  which.shp.file = which( nchar( subfolder_files[has.shp] ) == max(nchar(subfolder_files[has.shp] )) )
  shp.file = subfolder_files[has.shp][which.shp.file]


  folder.names = unlist( strsplit( shp.file, "/") )
  gisfolder = paste0(folder.names[ 1:( length((folder.names) )-1)], collapse = "/")

  shp.file = tail( unlist( strsplit( shp.file, "/") ) , 1 )
  shp.name = head( unlist( strsplit( shp.file, ".", fixed = TRUE) ) , 1)

  shapefile = readOGR( paste0(folder, "/", gisfolder), shp.name , verbose = FALSE )

  # gis_old = grepl("Supplemental/flat ascii (.dat)", subfolders, fixed = TRUE)
  # gis_old_alt = grepl("Supplemental Household Recode/stataset (.dta)", subfolders, fixed = TRUE)
  # gis_new = grepl("afl", subfolders, fixed = TRUE)
  #  gis = gis_old | gis_old_alt | gis_new
  #
  # if ( sum(gis, na.rm = TRUE) == 0 ){ return(NULL)}
  #
  # gisfolder =  subfolders[ max( which(gis) ) ]
  #
  # files = list.files(gisfolder)
  # shp.file = files[ grepl(".shp", files, fixed = TRUE)][1]

  # if ( is.na( shp.file)  ){ return(NULL)}

  # shp.name = strsplit( shp.file, ".", fixed = TRUE )[[1]][1]

  # shapefile = readOGR( gisfolder, shp.name, verbose = FALSE )


  # geo file variable DHSID links with Household Member Recode variable hv001 (cluster number)
  # NB: shapefile has longe character string for cluster (AO200600000116),
  # where as household cluster is numeric (e.g. 116)

  cluster = shapefile %>%
    as.data.frame() %>%
    select( DHSID, LATNUM, LONGNUM) %>%
    mutate( dhsid = as.numeric(str_sub(DHSID, start= -4)),
            latitude = LATNUM,
            longitude = LONGNUM) %>%
    # select( dhsid, latitude, longitude, URBAN_RURA ) %>%
    filter( !(latitude == 0 & longitude == 0))

  return(cluster)
  }


# Test / Example
# g =  survey_GIS_data("Angola", "MIS", "2011")
# g =  survey_GIS_data("Uganda", "MIS", "2014-15")
# g =  survey_GIS_data("Mali", "MIS", "2015")



country = "Angola"
survey = "MIS"
year = "2011"

country = "Zambia"
survey = "Standard DHS"
year = "2013-14"


