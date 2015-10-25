library(survey) 	# load survey package (analyzes complex design surveys)
library(foreign) 	# load foreign package (converts data files into R)
library(Hmisc)
library(ggplot2)
library(scales)
library(dplyr)
library(knitr)

# load survey ####

# tabs = c("Individual Recode", "Children's Recode", "Household Member Recode",
         # "Household Recode", "Supplemental Births Recode") 

survey_data = function(
  country = "Angola" , # "DRC"
  year =  2011 , # "2013-14"
  survey ="MIS",  # "Standard DHS"
  survey_year = NA,
  tab = "Household Member Recode",
  design = TRUE # to return survey design or not
){
  if ( is.na(survey_year) ){
    survey_year = paste(survey, year)
  }
  file = paste0("./", 
                ifelse( country %in% "DRC", "Congo Democratic Republic", country),
                "/", survey_year, "/", tab, ".rda") 
  print(file); flush.console()
  load( file ) # file will be loaded as 'x'
  
  if (tab %in% "Household Member Recode"){
      x$weight <- as.numeric( x$hv005 )
      x$strata <- x$hv023 ## Nigeria (JP)
      x$psu = x$hv021 # (JP)
      
  } else {
    # convert the weight column to a numeric type
    x$weight <- as.numeric( x$v005 )
    x$strata <- x$v023 ## Nigeria (JP)
    x$psu = x$v021 # (JP)
  }
  
  # create a survey design object (svy) with DHS design information
  if (design){
      if (sum(!is.na(x$strata)) > 0){ # some surveys have no strata (e.g. madagascar mis 2011)
          svy <- 
            svydesign( 
              ~psu , 
              strata = ~strata , 
              data = x , 
              weights = ~weight
            )
        } else {
          svy <- 
          svydesign( 
            ~psu , 
            data = x , 
            weights = ~weight
          )
        }
    
    # add a new variable 'one' that simply has the number 1 for each record 
    svy <-
      update( 
        one = 1 ,
        svy
      )
  } else { svy = NA}
    
    return( list(data = x, design = svy))
     
#      if (i == 1){
#        survey_samples = survey_sample
#      } else {
#        survey_samples = bind_rows(survey_samples, survey_sample)
#      }
        
#     assign( shortname[i], x)
#     assign( paste0(shortname[i], "svy") , svy)
}

# Test / Example
# by default, R will crash if a primary sampling unit (psu) has a single observation
# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
# by uncommenting this line:
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN

#   s = survey_data()
#  
#   # print size
#   svytotal( ~one , s$design ) # ????
#   
#   svyby( ~one , ~one , s$design , unwtd.count ) 
#   
#   # by urban area
#    svyby( ~one , ~hv025 ,  s$design , unwtd.count ) 
#    
#   # compare raw versus survey 
#    s$design <-
#     update( 
#       hml6 = factor( hml6 ) ,
#       s$design
#   )
#   
#   table( s$data$hml6) 
#   prop.table( table( s$data$hml6) )
#   
#   svyby( ~hml6 , ~one ,  s$design , svymean ,  na.rm = TRUE) 
#   
  
        
