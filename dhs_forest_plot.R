
# packages ####
library(survey)  
options(survey.lonely.psu="adjust")

library(stringr)
library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(dplyr)
library(sp)
library(rworldmap) # this pkg has waaaay better world shapefiles
library(ggthemes)
library(ggmap); library(BH)

library(countrycode)

# get list of surveys and files ####
source("file_list.r")

f = files %>%
  filter( !is.na( countrycode(country, "country.name", "country.name") )) %>%
  filter( grepl( paste(2000:2020, collapse="|"), year) ) %>%
  mutate( year = sapply(survey_year,
                        FUN = function(x) tail(unlist(strsplit(x, " ", fixed = TRUE)), 1) )
  ) %>%
  count(country, survey_year, year)

View(f)

# PMI
pmi = c( "Angola", "Benin", "DRC",
         "Ethiopia", "Ghana", "Guinea", "Kenya", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique",
         "Nigeria", "Rwanda", "Senegal", "Tanzania", "Uganda", "Zambia", "Zimbabwe" )

pmi_iso3 = countrycode(pmi, "country.name", "iso3c")


# PLOTS ####

# select survey ####
# .country = f[11,1]  # 11 = burkina mis2014
# .survey_year = f[11,2]

# source("dhs_variable_selection.R")
source( "dhs_load_survey.R" )

# svy = load_survey_object( .country = .country,
#                           .survey_year = .survey_year ,
#                           geo = FALSE
# )

# svy.h =  svy[[1]] 
# svy.c =  svy[[2]] 
# names(svy[[3]])  # list of variables

# df = data.frame(Data = svy[[3]], row.names = names(svy[[3]]))  # survey structure

load('dictionary.rda') # loads 'data_dictionary'

# Forest plot by region ####
svy_forest_by = function(.var = "hml32", 
                         .by = "hv024", 
                         .file = "household", 
                         .country = "Burkina Faso",
                         .survey = "MIS",
                         .year = "2014",
                         .dictionary = dictionary,
                         .subtitle = "" ){
  
  svy = load_survey_object( .country = .country, dataset = FALSE, 
                            .survey_year = .survey_year ,
                            geo = FALSE
  )
  
  if (.file == "household"){ survey = svy[[1]] 
  } else if ( .file == "children") { survey = svy[[2]] 
  } else {
    return()
  }

  names(svy[[3]])  # list of variables
  
  # df = data.frame(Data = svy[[3]], row.names = names(svy[[3]]))  # survey structure
  
  
  sb =svyby( as.formula(paste("~", tolower(.var))) , 
             by = as.formula(paste("~", .by)), 
             survey, 
             svymean, na.rm = TRUE )
  
  sb$upper = sb[, names(sb)[2]] + 1.96*sb[, names(sb)[3]]
  sb$lower = sb[, names(sb)[2]] - 1.96*sb[, names(sb)[3]]
  
  # add labels for by variable
  dictionary = .dictionary %>% filter(country %in% .country, 
                                          survey %in% .survey,
                                      year %in% .year)
  
  # TODO:  why is description field missing?  
  # - any way to speed up loading of survey?

  # categories/regions and axis label dexcription
  d = dictionary %>% 
    filter( grepl( "member", file, ignore.case = TRUE) , 
            toupper(Name) == toupper("hv024") ) 
  
  values = factor(d$value) 
  label = paste( unique(d$Name), ":" , unique(d$description) )
  
  # plot
  g =  ggplot(data = sb, aes_string(x = values, y =  .var ) ) +
    geom_point() + 
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .25) +
    xlab(NULL) +
    ylab(label) +
    coord_flip() +
    ggtitle( paste(.country, .survey_year)) 

  
  if ( nchar(.subtitle)>0 ){
    g = g + labs(subtitle = .subtitle)
  }
  
  return(g)
}

# tests
svy_forest_by(.var="hml32", .by = "hv024", .file = "household",
              .country = "Burkina Faso",
              .survey = "MIS",
              .year = "2014" )
# 
# svy_forest_by("hml35", .subtitle = "results by regions", .survey = "household",
#               .country = "Benin", .survey_year = "Standard DHS 2011-12")
# 
# svy_forest_by("hml32")
# svy_forest_by("hml35")
# 
# svy_forest_by("s309")  # number of fansidar: 'ml1', but for Burkina, S307A
# svy_forest_by("s307a")  # took fansidar: 'm49a', but for Burkina, S307A
# 
# svy_forest_by( .var = "ml1", .country = "Benin", .survey_year = "Standard DHS 2011-12")  # number of fansidar: 'ml1', but for Burkina, S307A
# svy_forest_by("m49a", .country = "Benin", .survey_year = "Standard DHS 2011-12")  # took fansidar: 'm49a', but for Burkina, S307A






# Histogram ####

svy_histogram = function(.var = "hml32", 
                         .file = "household", 
                         .country = "Burkina Faso",
                         .survey = "MIS",
                         .year = "2014",
                         .dictionary = data_dictionary,
                         .subtitle = "" ){
  
  svy = load_survey_object( .country = .country, dataset = FALSE, 
                            .survey_year = .survey_year ,
                            geo = FALSE
  )
  
  if (.file == "household"){ survey = svy[[1]] 
  } else if ( .file == "children") { survey = svy[[2]] 
  } else {
    return()
  }

    
  sb =svyby( ~ one,
             as.formula(paste("~", tolower(.var))) ,
             survey,
             unwtd.count, na.rm = TRUE )  # USE 'unwtd.count'
  
  # sb =svyby( as.formula(paste("~", tolower(.var))) , 
  #            by = as.formula(paste("~", .by)), 
  #            survey, 
  #            unwtd.count, na.rm = TRUE )
  
  # add labels for by variable
    # add labels for by variable
  dictionary = .dictionary %>% filter(country %in% .country, 
                                          survey %in% .survey,
                                      year %in% .year)
  
  # TODO:  why is description field missing?  
  # - any way to speed up loading of survey?

  # categories/regions and axis label dexcription
  d = dictionary %>% 
    filter( grepl( "member", file, ignore.case = TRUE) , 
            toupper(Name) == toupper("hv024") ) 
  
  values = factor(d$value) 
  label = paste( unique(d$Name), ":" , unique(d$description) )
  
  # plot
  g =  ggplot(data = sb , aes_string( x =  .var ) ) +
    geom_bar( aes(y = counts ), stat = 'identity' ) + 
    # geom_errorbar(aes(ymin = lower, ymax = upper), width = .25) +
    # coord_flip() +
    ggtitle( paste(.country, .survey_year)) +
    xlab(label)
  
  if ( nchar(.subtitle)>0 ){
    g = g + labs(subtitle = .subtitle)
  }
  
  return(g)
}

# svy_histogram("hml35")
# svy_histogram("hml32")
# 
# svy_histogram("s309")  # number of fansidar: 'ml1', but for Burkina, S307A
# svy_histogram("s307a")  # took fansidar: 'm49a', but for Burkina, S307A


