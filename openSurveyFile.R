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

openSurveyFile = function(
  country = "Angola" , # "DRC"
  year =  2011 , # "2013-14"
  survey ="MIS",  # "Standard DHS"
  survey_year = NA,
  tab = "Household Member Recode"
){
  
  if ( is.na(survey_year) ){
    survey_year = paste(survey, year)
  }
  
  file = paste0("../DHS/",
                ifelse( country %in% "DRC", "Congo Democratic Republic", country),
                "/", survey_year, "/", tab, ".rda") 
  print(file); flush.console()
  
  load( file ) # file will be loaded as 'x'
  
  if (tab %in% c("Household Member Recode", "Household Recode") ){
    
    # Sample weights are calculated to six decimals but are presented in the standard recode files without the
    # decimal point. They need to be divided by 1,000,000 before use to approximate the number of cases
    # see Guide_to_DHS_Statistics_29Oct2012_DHSG1.pdf page 12

      x$weight <- as.numeric( x$hv005/1000000 )
      x$strata <- x$hv023 ## Nigeria (JP)
      x$psu = x$hv021 # (JP)
      
  } else {
    # convert the weight column to a numeric type
    x$weight <- as.numeric( x$v005/1000000 )
    x$strata <- x$v023 ## Nigeria (JP)
    x$psu = x$v021 # (JP)
  }
  
  return(x)

}

# from loading and merging DHS (D. Vanderelst, N. Speybroeck, 2014)
# Matching variables
# V001 Cluster number Children data set
# V002 Household number in cluster Children data set
# HV001 Cluster number Household data set
# HV002 Household number in cluster Household data set

# Design variables
# V021 Primary sampling unit Children data set
# V023  stratification used in some surveys, V023 is blank or is set to 'National'
# V005 Sample weight Children data set
# V025 Type of place of residence Children data set

# Analysis variables
# V190 Wealth index (quintile) Children data set
# SH108A Time to get to health facility Household data set
# B5 Child alive or dead Children data set

# Example Descriptive statistics
# Variable Range Mean (SD) Value counts
# V021 1-300 NA NA
# V025 1–2, 1: urban, 2: rural NA 1: 3,575, 2: 5,417
# V190 1–5, 1: poorest, 5: richest 02.87 (1.40) 1: 2,038, 5: 1,483
# SH108A 0–900, in minutes 61.36 (83.29) NA
# B5 0–1, 0: no, 1: yes 00.88 (0.31) 0: 1,005, 1: 7,987


