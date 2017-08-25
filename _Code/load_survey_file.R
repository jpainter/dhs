
library(dplyr)
library(survey)


openSurveyFile = function(
  .country = NA ,
  # survey_year = NA,
  .survey = NA,
  .year = NA,
  .file = NA,
  dhs_surveys = "../DHS/_Surveys/",
  dhs_code = "../DHS/_Code/"

)
{

# functions for loading gis data files ####
  if (!existsFunction( "survey_GIS_data" ) ) source( paste0( dhs_code, "getSurveyGIS.R") )

  x = NULL

  file = paste0( dhs_surveys ,
                 ifelse( .country %in% "DRC", "Congo Democratic Republic", .country),
                 "/", .survey, " ", .year,  "/", .file, ".rda")

  if ( file.exists( file ) ){
    load( file ) # file will be loaded as 'x'

  } else {

    filecodes = data_frame( abrev  = c("kr", "br", "ir", "pr", "hr", "hr" ),
                            full = c("Children's Recode","Supplemental Births Recode","Individual Recode",
                                     "Household Member Recode", "Household Recode", "Supplemental Household Recode"))

    dir =
      paste0(  dhs_surveys , .country, "/", .year, "_", .survey, "/"  )

    files = list.files( dir )

    prefix = paste0(  tolower( countrycode( .country, "country.name", "iso2c") ),
                      filecodes$abrev[ match( tolower( .file ), tolower( filecodes$full ) )] )

    middle = substr( files[grep(".MAP", files, fixed=T)][1], 5, 8 )

    suffix = ".rds"

    file = paste0( dir, prefix, middle, suffix)

    if ( file.exists( file ) ){
      x = readRDS( file ) # file will be loaded as 'x'
    }

    ## TODO: error message when file not found

  }

  return(x)
}

# TODO: get printout working.  R does not normally print from inside a function.
load_survey_file = function(
  .country = "Angola",
  # .survey_year = "MIS 2011",
  .year = 2011 ,
  .survey = "DHS",
  .file = "Household Member Recode" ,
  dhs_surveys = "../DHS/_Surveys/",
  dhs_code = "../DHS/_Code/" ,
  design = TRUE, # return survey design object; return dataset if false
  geo = FALSE,
  printout = FALSE,
  vars = NULL  # if not specified, will select variables from vars() [dhs_variable_selection.R]
){

  # no vars given, get basic list of variables
  if ( is.null(vars) ){
    source( paste0( dhs_code, "dhs_variable_selection.R") )
    vars = some_variables()
  } else {
    vars = tolower(vars)
  }

  if (printout){ cat(vars) }

  x = try(
    openSurveyFile( .country = .country ,
                    .survey = .survey ,
                    .year = .year ,
                    .file = .file ,
                    dhs_surveys = dhs_surveys,
                    dhs_code = dhs_code 
                    )
  )

  if (
    ( class(x) == "try-error" | class(x) == "logical" | is.null(x) ) ){
    x = try(
      openSurveyFile(.country = .country,  .survey = .survey, .year = .year,
                     .file = paste("Supplemental", .file )
      )
    )
  }

  if (
    (class(x) == "try-error" | class(x) == "logical" | is.null(x) ) ){
    x = try(
      openSurveyFile(.country = .country,  .survey = .survey, .year = .year,
                     .file = .file
      )
    )
  }


  if (geo){
    g = try(
      survey_GIS_data( .country = .country,  .survey = .survey, .year = .year
                       )

    )

    if ( class(g) == "data.frame" ) x = x %>% left_join(g, by=c("hv001"="dhsid") )

  } else {  g = NULL }



# VARS  #### ensure all variables are in file

  vars_in_x = sapply(vars, function(y)  y %in% names(x))  # check if variable is in dataset
  missing_vars = vars[!vars_in_x]
  for (i in seq_along(missing_vars)){
    x[[missing_vars[i]]] = NA 
  }
  x = x %>% select_( .dots = vars )
  


#  DESIGN ####
  if (design){

    # test if strata exists; some surveys have no strata (e.g. madagascar mis 2011)
    has.strata.022h = nrow( as.data.frame.table( table(x$hv022) ) ) > 1
    has.strata.023h = nrow( as.data.frame.table( table(x$hv023) ) ) > 1
    has.strata.024h = nrow( as.data.frame.table( table(x$hv024) ) ) > 1
    has.strata.025h = nrow( as.data.frame.table( table(x$hv025) ) ) > 1

    has.strata.022c = nrow( as.data.frame.table( table(x$v022) ) ) > 1
    has.strata.023c = nrow( as.data.frame.table( table(x$v023) ) ) > 1
    has.strata.024c = nrow( as.data.frame.table( table(x$v024) ) ) > 1
    has.strata.025c = nrow( as.data.frame.table( table(x$v025) ) ) > 1


    # survey design for housenold member file only
##### TODO : Add survey designs for other files


    # household member
    
    if ( tolower(.file) == tolower("Household Member Recode")){
      x = x %>%
        mutate( weight.hm = as.numeric( hv005 / 1000000 ),
                strata = do.call( paste , x[ , c( 'hv024' , 'hv025' ) ] )
                ) %>%
        filter( !is.na(weight.hm), !is.na(hv021) )
      
      strataformula.hm = as.formula("~hv024 + hv025")
      
      svy <-
        svydesign(
          ~hv021  , # psu
          strata = strataformula.hm ,
          data =  x ,
          weights = ~ weight.hm
        )

    } else {
      svy = NULL
      }


  } # end if (design)


  if (printout){
    cat( "The", .file, "file has", comma(nrow(x)), "rows and ", ncol(x), "columns", "\n"
    )
  }

  if ( design ){
    return(  svy )
  }  else {
    return(  x )
  }


}

