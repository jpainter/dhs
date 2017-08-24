# make data file for malaria_atlas from DHS datasets
# version history
## dhsForMAP.R  Nov 16, 2015
## dhs_malaria.R  Nov 16, 2015
## dhs_data.R May 16, 2016  incomplete replacement with early/late classification.
###   has code for some sub alanyses and summaries
###   made.d.rda, which was used in slideVersusRDT.rmd

# TODO : fix namespace conflict where files that load surveys invalidate dplyr verbs

# libraries ####

library(tidyverse, quietly = TRUE)
library(data.table)
library(scales, quietly = TRUE)
library(lubridate)
# library(dtplyr)
library(countrycode)
library(knitr)

library(survey)
# library(srvyr) # https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html
library( broom )



# Set location of survey datasets and survey code

## location of DHS survey and code folders.
## include DHS files as: paste0( dhs_surveys, <dhs file here>)
dhs_surveys = "../DHS/_Surveys/"
dhs_code = "../DHS/_Code/"
dhs_dataset = "../DHS/_Datasets/"


# 2. Get list of available survey files by calling the script, file_list.r, then filter to those in subsahara since 2000.  Print list of surveys by country and year. ####


source( paste0( dhs_code, "file_list.r") )

subsahara = countrycode_data %>%
     filter( region %in% c("Middle Africa", "Western Africa", "Eastern Africa", "Southern Africa")) %>%
     rename( country.name = country.name.en) %>%
     select( country.name, iso3c ) %>%
     filter( !country.name %in% "Lesotho")


files = files %>%
     filter( year >= 2000 ) %>%
     # convert to standard country names in countrycode package, which
     # avoids having 'two' countries like Tanzania and United Republic of Tanzania
     mutate( country.name = countrycode(country, "country.name.en", "country.name.en")) %>%
     rename( Country = country.name ) %>%
     filter(Country %in% subsahara$country.name)

# define eras for surveys: most recent = w/in 3 years, etc.
this_year = year( Sys.Date() )
pmi_start_year = 2006

main_year = function(x, part ){
     if ( is.character(x)){
          as.numeric( unlist( strsplit(x, "-") )[part]  )
     } else { return(NA)}
}

sla = files %>%
     mutate( country.name = countrycode(country, "country.name.en", "country.name.en")) %>%
     filter( survey!="", year >= 2000 , country.name %in% subsahara$country.name) %>%
     mutate( iso3 = countrycode( country, 'country.name', "iso3c" ) )

# run these outside of dplyr::mutate because they throw an error otherwise
sla$year1 = sapply( sla$year , function(x) main_year(x, 1) )
sla$year1b = sapply( sla$year , function(x) main_year(x, 2) )
sla$year2 = ifelse( !is.na( sla$year1b ), 2000 +  sla$year1b, sla$year1)

slb = sla  %>%
     dplyr::count( country, country.name, iso3,  year, year1, year2, survey ) %>%
     mutate(
          pre_2006 = year1 %in% 2000:(pmi_start_year-1)    ,
          earliest_from_2006 = year1 %in% pmi_start_year:(pmi_start_year+4)    ,
          latest_within_4_years = year2 %in% (this_year-4):this_year
     )

slb$all_in_era = ifelse( slb$pre_2006 == TRUE, 'earliest_pre_2006' ,
                         ifelse( slb$earliest_from_2006 == TRUE, 'earliest_from_2006',
                                 ifelse( slb$latest_within_4_years == TRUE,  'latest_within_4_years',
                                         NA) ) )

latest = slb %>%  ungroup() %>%
     group_by( iso3, all_in_era ) %>%
     arrange( -year2 ) %>%
     slice(1) %>%
     mutate(
          era = all_in_era
     ) %>% ungroup()

survey_list = slb %>%
     left_join( select( latest, era, iso3, survey, year, era ) ,
                by = c("iso3", "year", "survey")
     )

nsurveys = nrow(survey_list)
ncountries = length( unique( files$Country ) )

cat( "There are", nsurveys, "surveys since 2000 from",  ncountries ,"countries in Subsaharan Africa.")

kable( survey_list %>% arrange( country, year ) %>% select( country, year, survey) )


# Variables ####  

# TODO: Get variables from excel sheet.

library(readxl)

variable.doc = read_excel( paste0( dhs_code, "dhs_variable_definitions.xlsx") )

# cluster.summary.nse = filter(variable.doc, !is.na(Code) , 
#                              Applies_to == "cluster.summary"
# ) %>%
#   dplyr::select(Variable, Code)  %>% as.list()
# 
# cluster.mutate.nse = filter(variable.doc, !is.na(Code) , 
#                             Applies_to == "cluster.mutate"
# ) %>%
#   dplyr::select(Variable, Code)  %>% as.list()


individual_vars  = c(
   'hml32', 'hml35', # RDT and micrscopy
   'hml1', # number of mosquito bednets
   "hml4", # months ago net obtained.  (96 = more than 3 years ago; )
   "hml5", # net treated with insecticide when bought (e.g. ITN)
   "hml6", # net treatment status
   "hml7", # brand of bednet
   "hml8",  # net treated since receiving
   "hml9", # time since last retreatment
   "hml10" , # ITN
   "hml11" , # number of persons who slept under this net (5 = 5+, 9 = missing)
   "hml12", # type of net person slept under
   "hml19" , # person slept under an ever-treated net
   'hml20', # person slept under an llin

   'hv227', # has a bednet for sleeping
   'hv228', # children under 5 slept under bednet (e.g. all, some, none; 9 = missing )
   "hv253", # dwelling sprayed (some multiple responses)

   'hv102', # de jure member of household
   'hv103', # de facto memeber of household (slept there last night)
   'hml16', # corrected age in years (0:96)
   'hml16a' , # for children, corrected age in months

   "hv000", # country and phase
   "hv001", # cluster number
   "hv002", # household number
   'HV003', # line number of respondent answering questions
   'hv004' ,  # ultimate area unit ????
   'HV005', # household sample wieght (6 decimals )

   "HV021", # psu: enumeration area or sample cluster
   "HV005", # spample weight (divide by 1E6)
   "HV022", # Sample stratum number
   "hv023", # STRATA
   "hv024", # region
   "HV025" , # type of residence : urban rural

   'HHID', # case identification ????
   'HVIDX', # line number (1 to 90)
   "HV008", # date of interview
   "HV270", # wealth index
   "latitude", "longitude"
)

variables <- unique( toupper( individual_vars) )
variables = variables[ order(variables) ]




## survey line list: X ####
### NB: this is household member files only
### TODO: option for other survey files, e.g. childrens for ACCM

source( paste0( dhs_code, "getSurveyGIS.R") )
source( paste0( dhs_code, 'load_survey_file.R' ) )

nsurveys = nrow(survey_list)

index = row.names(survey_list)

p <- progress_estimated(nsurveys)

x <- list()

translate = FALSE  # optional translation of values to label.
# NB: Does not work well for all variables

###TODO:  add option to append only--skip if already exists--rewrite

for (i in seq_along( index )  ){

   p$pause(0.1)$tick()$print()

   .country = survey_list[i, ]$country
   .iso3 = countrycode( survey_list[i, ]$country, "country.name", "iso3c")
   .survey = survey_list[i, ]$survey
   .year = survey_list[i, ]$year
   .year1 = survey_list[i, ]$year1
   .year2 = survey_list[i, ]$year2
   .period = survey_list[i, ]$year2
   .file = "Household Member Recode"
   .survey_year = paste(.survey, .year)
   .era = survey_list$era

   print( paste(.country, .survey, .year, .file) )

   x[[i]] <- try( silent = TRUE,
                  load_survey_file(
                     # printout = TRUE ,  # un comment when testing
                     vars = variables ,
                     .country = .country ,
                     .survey = .survey ,
                     .year = .year ,
                     .file = .file ,
                     design = FALSE ,
                     geo = TRUE  )
   )

   if ( class(x[[i]]) == "try-error" ){
      x[[i]] = NULL
      next()
   }


   x[[i]]$country = rep( .country, nrow(x[[i]]) )
   x[[i]]$iso3 = rep( .iso3, nrow(x[[i]]) )
   x[[i]]$year = rep( .year, nrow(x[[i]]) )
   x[[i]]$survey = rep( .survey, nrow(x[[i]]) )

   # TODO: optional- relate dictionary to survey                    )

   # # TODO: only for columns with >4 values (others easier to keep as numeric)
   # # Deriving this table should happen once, before loop
   # v = dd %>% filter( tolower(Item_Name) %in% variables) %>% group_by( Item_Name, label ) %>%
   #   summarise( n = n()) %>% group_by( Item_Name) %>% count(Item_Name) %>% filter(nn>4)

   if (translate){

      # refine dictionary for this country-survey
      dict = dd %>% filter_( ~country == .country , ~survey == .survey, ~year == .year,
                             ~Item_Name %in% toupper(names(has_vars))
      )

      y = x[[i]]

      for (col in 1:length(colnames(y))){
         column = tolower( colnames(y)[col] )

         dict_var = dict %>%
            mutate( item_name = tolower(Item_Name)) %>%
            filter_( ~item_name == column ) %>% # , ~file == "Household Member Recode"
            count( value, label) %>% select(-n)

         old_value = y[, col]
         new_value = dict_var[ match( old_value, dict_var$value ), ]$label
         y[, col] = ifelse( is.na( new_value), old_value, new_value)
      }
      x[[i]] = y
   }

   # if (is.null(X) ){ X <- x; next()}
   # X = rbindlist( list(X, x), use.names = TRUE, fill = TRUE)

}

# X = dplyr:: bind_rows( x )  # will throw error is column types dont match
X = rbindlist(x, fill = TRUE)

# check X for GPS data
# identify rows without gps and those with gps that did not match adm
gps = X %>% group_by(country, survey, year) %>% 
  summarize( nogps = sum( is.na(longitude)|is.na(latitude) ) ,
             gps = sum( !is.na(longitude) & !is.na(latitude) )
  )
View(gps)

# Save/load X ####

save( X , file = paste0( dhs_dataset, "X.rda") )


load( paste0( dhs_dataset, "X.rda") )

## Individual dataset ####

library(readxl)

variable.doc = read_excel( paste0( dhs_code, "dhs_variable_definitions.xlsx") )

# NB: all references to survey variable should be lower case (eg hv227, not HV227)
individual.mutate.nse = filter(
     variable.doc,
     !is.na(Code) ,
     Applies_to == "indiv",
     !(exclude %in% 1)
     ) %>%
     select(Variable, Code)  %>% as.list()


indiv = X %>%

     mutate_( .dots = setNames(
          individual.mutate.nse$Code ,
          individual.mutate.nse$Variable
     )  
     ) %>%
   mutate( rowid = row_number()) %>%  # use rowid when merging over results back into indiv
   ungroup()

saveRDS( indiv, file = paste0( dhs_dataset, "indiv.rds"))

indiv = readRDS( paste0( dhs_dataset, "indiv.rds") )

## Load Maps ####

suppressMessages( library(sp) )

gadm = "../malaria_atlas/gadm/"

load( paste0( gadm, "adm0.africa") )
load( paste0( gadm, "adm1.africa") )
load( paste0( gadm, "adm2.africa") )
load( paste0( gadm, "adm0.africa.s") ) # simplified map to reduce edges and speed up over function
load( paste0( gadm, "adm0.africa.centroids") )
load( paste0( gadm, "adm1.africa.centroids") )
load( paste0( gadm, "adm2.africa.centroids") )

proj_string = proj4string(adm1.africa)


# individual spatial data frame
indiv.spdf = SpatialPointsDataFrame( indiv %>% 
                                       filter(!is.na(longitude)|!is.na(latitude) ) %>% 
                                       dplyr::select(longitude, latitude ) , 
                                     
                                     indiv %>% 
                                       filter(!is.na(longitude)|!is.na(latitude) ) %>% 
                                       dplyr::select( rowid ), 
                                     
                                     proj4string=CRS(proj_string) 
)

# Over function to associate cluster with adminstrative areas ####
over_adm2 = over( indiv.spdf, adm2.africa)   %>% dplyr::select(-iso3)

over_adm2$rowid = indiv.spdf@data$rowid

indiv.over = left_join( indiv, over_adm2 , na_matches = "never"  ) 

saveRDS(indiv.over, file = paste0( dhs_dataset, 'indiv.over.rds'))

indiv.over = readRDS(paste0( dhs_dataset, 'indiv.over.rds'))

# identify rows without gps and those with gps that did not match adm ####
ADM = indiv.over %>% group_by(country, survey, year) %>% 
  summarize( 
    N = n() ,
    NAME_0 = sum( !is.na(NAME_0) ) ,
    NAME_1 = sum( !is.na(NAME_1) ) ,
    NAME_2 = sum( !is.na(NAME_2) ) ,
    gps = sum( !is.na(longitude) & !is.na(latitude) ) ,
    nogps = sum( is.na(longitude) | is.na(latitude) ),
    gpsNoName = gps - NAME_2
             )
View(ADM)




## Cluster dataset ####

library(readxl)

variable.doc = read_excel( paste0( dhs_code, "dhs_variable_definitions.xlsx") )

cluster.summary.nse = filter(variable.doc, !is.na(Code) , 
                             Applies_to == "cluster.summary"
                             ) %>%
     dplyr::select(Variable, Code)  %>% as.list()

cluster.mutate.nse = filter(variable.doc, !is.na(Code) , 
                            Applies_to == "cluster.mutate"
                            ) %>%
     dplyr::select(Variable, Code)  %>% as.list()


cluster = indiv %>%

   # group by household
   group_by( country, iso3, survey, year,  hv024, hv023, hv021, hv001, hv002) %>%

   dplyr::mutate(
      persons.h =  n()
      )  %>%

   # group by household
   group_by( country, iso3, survey, year, hv024, hv023, hv021, hv001) %>%

   summarise_( .dots = setNames(
      as.list( cluster.summary.nse$Code ) ,
      cluster.summary.nse$Variable
   )
   )  %>%

   mutate_( .dots = setNames(
      as.list( cluster.mutate.nse$Code ) ,
      cluster.mutate.nse$Variable
   )
   )  %>%

   ungroup

### TODO:  Do we need this cluster spdf????
cluster.coords =  indiv %>%
  filter(!is.na(longitude)) %>%
  dplyr::count(iso3, survey, year, hv001, longitude, latitude) %>%
  ungroup

# split into 2 dataset, one with just lng/lat, for creating SpatialPoints object
coords = cluster.coords %>%  dplyr::select(longitude, latitude)

data = cluster.coords %>% dplyr::select(-longitude, -latitude)

# use same proj string as gadmin data (e.g. see last line of str(x0))
cluster.spdf = SpatialPointsDataFrame( coords, data, proj4string=CRS(proj_string) )

# adm0 (National)  #####

#--------------------------

svy_mean = function(x, var, by ){
  
  # if no non-missing values, end 
  if ( sum( complete.cases( x[, var ] ) ) == 0){
    # df = data_frame( NA )
    # names(df) = var
    # return(  df  )
    return( NA )
  } 

  strataformula.hm = as.formula("~hv024 + hv025")

  sd = svydesign( 
    ~hv021 , # psu 
    strata = strataformula.hm , 
    data =  x, 
    weights = ~ weight  # weights for household member file
  )  
  
  if ( any(is.na(by)) |  length(by) != sum( by %in% names( x ) )  ){
    sd = update( sd, one = 1 ) #  create variable for by expression that gives unstratified est
    by = "one"
  } else {
    if ( 
         sum( complete.cases( x[, by ] ) ) == 0  
         ) {
      df = data_frame( NA )
      names(df) = by
      return(  df  )
    }
  }
  
  .var = as.formula( paste( "~", var ))
  .by = as.formula( paste( "~", paste( by, collapse = "+") ) )
  
  svyby( .var, .by, sd , svymean, na.rm = TRUE )
} 

# list of surveys.  $data contains each surveys dataset as a data.frame
adm0.nest =
  indiv.over %>%
  # nest by survey
  nest( -country, -iso3, -survey, -year ) 


country.surveys = adm0.nest  # %>% filter( country %in% c("Angola", "Benin") )

cols = c("slide" , "RDT", "hv227")
by = 'hv000' # 'NAME_0' # 'hv024' c('hv024', 'hv025')  # 
n_surveys = nrow(country.surveys)
p <- progress_estimated(n_surveys)
x <- list()
options(survey.lonely.psu = "certainty")

for (i in seq_along( 1:n_surveys )  ){
  
  p$pause(0.1)$tick()$print()
  cat("\n" ,  country.surveys[i, ]$country, country.surveys[i, ]$year, country.surveys[i, ]$survey, "\n")
  
  survey_var_list = lapply(cols, function(col){ 
    country.surveys[i, ] %>% 
      mutate( est = map(data, var = col , by = by , svy_mean ) ) %>%
      select(-data) 
  } )  
  
  xx = list()
  n_survey_var_list = length( survey_var_list )
  
  for (ii in seq_along( 1:n_survey_var_list )  ){
    
    if ( is.na( survey_var_list[[ii]]$est ) ){ next }
    
    # .by = quo(by)
    df =  survey_var_list[[ii]] %>% unnest(est) %>% 
      mutate( var = cols[ii] ) %>%
      rename_( est = cols[ii] )
    
    # gather_( 'est', 'val', c( cols[ii], 'se') ) 
  
    # df[ df$est == cols[ii], ]$est = "point"
    
    xx[[ii]] = df
  }
  
  x[[i]]  = rbindlist( xx )
}

adm.by.estimates = rbindlist( x , fill = TRUE)

saveRDS( adm.by.estimates, file = paste0( dhs_dataset, "adm.by.estimates.rds"))

adm.by.estimates = readRDS( paste0( dhs_dataset, "adm.by.estimates.rds") )

View(adm.by.estimates)


# TODO:: summarise each var with survey statement.  purrr???
  summarise_at( vars(hml20, hv227),  survey_by( ))

## cluster version....
over_adm0 = over( cluster.spdf, adm0.africa)   %>% dplyr::select(-iso3)

admin0 = bind_cols( cluster.coords, over_adm0 ) %>% dplyr::select(-latitude, -longitude)

adm0.household =
  indiv %>%
  left_join( admin0, by = c("iso3", "survey", "year", "hv001" )) %>%
  
  # group by household
  group_by( country, iso3, survey, year, hv001, hv002) %>%
  
  dplyr::mutate(
    persons.h = n()
  )  %>%
  
  ungroup

adm0 = adm0.household %>%
  
  # group by admin
  group_by( country, iso3, survey, year, NAME_1) %>%
  
  summarise_( .dots = setNames(
    as.list( cluster.summary.nse$Code ) ,
    cluster.summary.nse$Variable
  )
  )  %>%
  
  mutate_( .dots = setNames(
    as.list( cluster.mutate.nse$Code ) ,
    cluster.mutate.nse$Variable
  )
  )  %>%
  
ungroup %>%
  
  left_join( adm0.africa.centroids, by = c( "iso3", "NAME_0") )


# adm1  #####

over_adm1 = over( cluster.spdf, adm1.africa)   %>% dplyr::select(-iso3)

admin1 = bind_cols( cluster.coords, over_adm1 ) %>% dplyr::select(-latitude, -longitude)

adm1.household =
     indiv %>%
     left_join( admin1, by = c("iso3", "survey", "year", "hv001" )) %>%

     # group by household
     group_by( country, iso3, survey, year, hv001, hv002) %>%

     dplyr::mutate(
          persons.h = n()
     )  %>%

     ungroup

adm1 = adm1.household %>%

   # group by admin
   group_by( country, iso3, survey, year, NAME_1) %>%

   summarise_( .dots = setNames(
      as.list( cluster.summary.nse$Code ) ,
      cluster.summary.nse$Variable
   )
   )  %>%

   mutate_( .dots = setNames(
      as.list( cluster.mutate.nse$Code ) ,
      cluster.mutate.nse$Variable
   )
   )  %>%

   # TODO: incorporate ITN definitions below, specific to adm1

   # summarise(
   #    households = n_distinct( hv002 ),
   #    persons = n(), # number of persons in household
   #    children_u6 = sum( hml16 %in% 0:6 ),
   #
   #    n_RDT = sum( RDT %in% 0:1 ) ,
   #    RDT = sum( RDT %in% 1 ) ,
   #    n_slide =  sum( slide %in% 0:1) ,
   #    slide= sum( slide %in% 1 ),
   #    n_parasit = sum( parasit %in% 0:1 ),
   #    parasit = sum( parasit %in% 1 ),
   #
   #    nets_per_household = sum( nets_per_person_household  , na.rm = TRUE ) / households, # No. of nets per household
   #    nets_per_person = sum( nets_per_person_household  , na.rm = TRUE ) / persons , # No. of nets per household
   #    mean_nets_per_persons_household = mean( nets_per_person_household , na.rm =TRUE ),
   #
   #    slept = sum( hml19 %in% 1 ),  # if unknown, then assume did not. # number sleeping under net
   #    slept.itn =  sum( hml12 %in% 1) # if unknown, then assume did not. # number sleeping under ITN
   #
   # ) %>%
   #
   # mutate(
   #
   #    survey_year = paste( survey, year),
   #
   #    pct.RDT =  RDT / n_RDT ,
   #    pct.slide = slide / n_slide ,
   #    pct.parasit = parasit / n_parasit ,
   #
   #    pct.net.itn = slept.itn / slept,
   #    pct.slept = slept / persons
   #
   # )  %>%

   ungroup %>%

   left_join( adm1.africa.centroids, by = c( "iso3", "NAME_1") )

# %>%
#
#    mutate(
#       pf_level = cut(
#          ifelse( is.na(percent_positive),
#                  9, percent_positive),
#          breaks = c(0,.1,.2,.4,.6, 1),
#          labels = c("0-9%", "10%-19%", "20%-39%", "40%-59%", "60+%"),
#          right = FALSE,
#          ordered_result = TRUE,
#          include.lowest = TRUE ) ,
#
#       net_level = cut(
#          ifelse( is.na(percent_has_net),
#                  9, percent_has_net),
#          breaks = c(0,.25,.5,.75,.85, 1),
#          labels = c("0-24%", "25%-49%", "50%-74%", "75%-84%", "85+%"),
#          right = FALSE,
#          ordered_result = TRUE,
#          include.lowest = TRUE ),
#
#       fever_level = cut(
#          ifelse( is.na(percent_had_fever),
#                  9, percent_had_fever),
#          breaks = c(0,.01,.02,.04,.08, 1),
#          labels = c("0-1%", "1%-2%", "2%-4%", "4%-8%", "8+%"),
#          right = FALSE,
#          ordered_result = TRUE,
#          include.lowest = TRUE )
#
#    )



# adm2  #####

over_adm2 = over( cluster.spdf, adm2.africa)   %>% dplyr::select(-iso3)

admin2 = bind_cols( cluster.coords, over_adm2 ) %>% dplyr::select(-latitude, -longitude)

adm2.indiv =
     indiv %>%
     left_join( admin2, by = c("iso3", "survey", "year", "hv001" )) %>%

     # group by household
     group_by( country, iso3, survey, year, hv001, hv002) %>%

     dplyr::mutate(
          persons.h = n()
     )  %>%

     ungroup

adm2 = adm2.indiv %>%

     # group by admin
     group_by( country, iso3, survey, year, NAME_1, NAME_2) %>%

     summarise_( .dots = setNames(
          as.list( cluster.summary.nse$Code ) ,
          cluster.summary.nse$Variable
     )
     )  %>%


     mutate_( .dots = setNames(
          as.list( cluster.mutate.nse$Code ) ,
          cluster.mutate.nse$Variable
     )
     )  %>%

     ungroup %>%

     left_join( adm2.africa.centroids, by = c( "iso3", "NAME_1", "NAME_2") )


# save / Tweak ####

save( survey_list, variable.doc, cluster, adm1, adm2 , 
      file = paste0( dhs_dataset, "malaria_atlas.rda") 
      )




