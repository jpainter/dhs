
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

load('data_dictionary.rda') # loads 'data_dictionary'

# Forest plot by region ####
svy_forest_by = function(.var = "hml32", .by = "hv024", .survey = "household", 
                         .country = "Burkina Faso",
                         .survey_year = "MIS 2014",
                         .dictionary = data_dictionary,
                         .subtitle = "" ){
  
  svy = load_survey_object( .country = .country, dataset = FALSE, 
                            .survey_year = .survey_year ,
                            geo = FALSE
  )
  
  if (.survey == "household"){ survey = svy[[1]] 
  } else if ( .survey == "children") { survey = svy[[2]] 
  } else {
    return()
  }

  names(svy[[3]])  # list of variables
  
  df = data.frame(Data = svy[[3]], row.names = names(svy[[3]]))  # survey structure
  
  
  sb =svyby( as.formula(paste("~", tolower(.var))) , 
             by = as.formula(paste("~", .by)), 
             survey, 
             svymean, na.rm = TRUE )
  
  sb$upper = sb[, names(sb)[2]] + 1.96*sb[, names(sb)[3]]
  sb$lower = sb[, names(sb)[2]] - 1.96*sb[, names(sb)[3]]
  
  # add labels for by variable
  dictionary = .dictionary %>% filter(country %in% .country, 
                                          survey_year %in% .survey_year)
  #regions
  r = dictionary %>% 
    filter( grepl( "member", file, ignore.case = TRUE) , toupper(var) == toupper("hv024") ) %>% 
    select(value, label)
  r.name = str_trim(r[ is.na(r$label), ]$value) 
  sb[, r.name] = sapply( unname(r[match(sb$hv024, r$value), "label"]), str_trim )
  r.name = r[ is.na(r$label), ]$value  
  r.label = r[ !is.na(r$label), ] 
  
  # variable
    member_file = grepl( "member", dictionary$file, ignore.case = TRUE)
    individual_file = grepl( "individual", dictionary$file, ignore.case = TRUE)
    childrens_file = grepl( "child", dictionary$file, ignore.case = TRUE)
    
  v = dictionary %>% 
    filter( member_file | individual_file , toupper(var) == toupper(.var) ) %>% 
    select(value, label)
  
  v.name = str_trim(v[ is.na(v$label), ]$value)[1]    

  v.label = v[ !is.na(v$label), ] 

  # plot
  g =  ggplot(data = sb, aes_string(x = r.name, y =  .var ) ) +
    geom_point() + 
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .25) +
    coord_flip() +
    ggtitle( paste(.country, .survey_year)) +
    ylab(v.name)
  
  if ( nchar(.subtitle)>0 ){
    g = g + labs(subtitle = .subtitle)
  }
  
  return(g)
}

# tests
svy_forest_by(.var="hml32", .by = "hv024", .survey = "household", 
                  .country = "Burkina Faso",
                  .survey_year = "MIS 2014",
                  .dictionary = data_dictionary,
                  .subtitle = "" )

svy_forest_by("hml35", .subtitle = "results by regions", .survey = "household",
              .country = "Benin", .survey_year = "Standard DHS 2011-12")

svy_forest_by("hml32")
svy_forest_by("hml35")

svy_forest_by("s309")  # number of fansidar: 'ml1', but for Burkina, S307A
svy_forest_by("s307a")  # took fansidar: 'm49a', but for Burkina, S307A

svy_forest_by( .var = "ml1", .country = "Benin", .survey_year = "Standard DHS 2011-12")  # number of fansidar: 'ml1', but for Burkina, S307A
svy_forest_by("m49a", .country = "Benin", .survey_year = "Standard DHS 2011-12")  # took fansidar: 'm49a', but for Burkina, S307A






# Histogram ####

svy_histogram = function(.var = "hml32", 
                         .survey = "household", 
                         .country = "Burkina Faso",
                         .survey_year = "MIS 2014",
                         .dictionary = data_dictionary,
                         .subtitle = "" ){
  
  svy = load_survey_object( .country = .country, dataset = FALSE, 
                            .survey_year = .survey_year ,
                            geo = FALSE
  )
  
  if (.survey == "household"){ survey = svy[[1]] 
  } else if ( .survey == "children") { survey = svy[[2]] 
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
  dictionary = .dictionary %>% filter(country %in% .country, 
                                      survey_year %in% .survey_year)
  #regions
  r = dictionary %>% 
    filter( grepl( "member", file, ignore.case = TRUE) , toupper(var) == toupper("hv024") ) %>% 
    select(value, label)
  r.name = str_trim(r[ is.na(r$label), ]$value) 
  sb[, r.name] = sapply( unname(r[match(sb$hv024, r$value), "label"]), str_trim )
  r.name = r[ is.na(r$label), ]$value  
  r.label = r[ !is.na(r$label), ] 
  
  # variable
    member_file = grepl( "member", dictionary$file, ignore.case = TRUE)
    individual_file = grepl( "individual", dictionary$file, ignore.case = TRUE)
    childrens_file = grepl( "child", dictionary$file, ignore.case = TRUE)
    
  v = dictionary %>% 
    filter( member_file | individual_file , toupper(var) == toupper(.var) ) %>% 
    select(value, label)
  
  v.name = str_trim(v[ is.na(v$label), ]$value)[1]    
  
  # plot
  g =  ggplot(data = sb , aes_string( x =  .var ) ) +
    geom_bar( aes(y = counts ), stat = 'identity' ) + 
    # geom_errorbar(aes(ymin = lower, ymax = upper), width = .25) +
    # coord_flip() +
    ggtitle( paste(.country, .survey_year)) +
    xlab(v.name)
  
  if ( nchar(.subtitle)>0 ){
    g = g + labs(subtitle = .subtitle)
  }
  
  return(g)
}

svy_histogram("hml35")
svy_histogram("hml32")

svy_histogram("s309")  # number of fansidar: 'ml1', but for Burkina, S307A
svy_histogram("s307a")  # took fansidar: 'm49a', but for Burkina, S307A


