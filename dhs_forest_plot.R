
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

variables =  c('v000', 
       'v001', 'v002', 'v003', 'v005', 'v021', 'v023', 'v024', 'v025', 
       'weight.c', 'weight.hm', 'weight.w', 'weight.h', 
       'hv001',  'hv002', 'hv003',  'hv005', 'hv007', 'hv021', 'hv023', 'hv024', 'hv025',
       'hvidx', 'b16',
       'h22', 'hv105', 'h47', 
       'hml16', # age in years
       'hml32', 'hml35',
      'h32a', 'h32b', 'h32c', 'h32d', 'h32e', 'h32f', 'h32g', 'h32h', 'h32i', 'h32s', #public/govt
      'h32j', 'h32k', 'h32l', 'h32m', 'h32n', 'h32o', 'h32p', 'h32q', 'h32r', #private
      'h32t', 'h32u', 'h32v', 'h32w', 'h32x', # informal privage/other
      'h32y', # no medical rx
      'h32z' # medical rx.  what's that mean?
      )

# source("dhs_variable_selection.R")
source( "dhs_load_survey.R" )

svy = load_survey_object( .country = "Rwanda",
                          .survey = "Standard DHS" ,
                          .year = "2010",
                          design = TRUE,
                          geo = FALSE
)

# svy.h =  svy[[1]] 
# svy.c =  svy[[2]] 
survey = svy[[4]] # household member
# names(svy[[3]])  # list of variables

survey = update(survey,  rdt = ifelse( hml35 %in% 0, 0, 
                                       ifelse( hml35 %in% 1, 1, 
                                               NA)) )

dd <-readRDS("dictionaryNew.rds")  # about 6 sec

# Forest plot by region ####
svy_forest_by = function(.var = "hml35",
                         .by = "hv024", 
                         survey = survey, 
                         .dictionary = dd,
                         .subtitle = "" ){
 

  sb = svyby( as.formula(paste("~", tolower(.var))) , 
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
  d = dictionary[dictionary$Item_Name %in% toupper(.by), c(1, 3, 4)] %>% unique
  
  values = factor(d$label) 
  label = paste( unique(d$Item_Name), ":" , unique(d$Item_Label) )
  
  # plot
  g =  ggplot(data = sb, aes_string(x = values, y =  .var ) ) +
    geom_point() + 
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .25) +
    xlab(NULL) +
    ylab(.var) +
    scale_y_continuous( labels = scales::percent ) +
    coord_flip() +
    ggtitle( paste(.country, .survey, .year), subtitle = .subtitle) 


  return(g)
}

# tests ####



svy_forest_by(.var="hml32", .by = "hv000", .subtitle = "Parasitemia (microscopy)", survey = survey)
svy_forest_by(.var="hml32", .by = "hv024", .subtitle = "Parasitemia (microscopy)", survey = survey)

svy_forest_by(.var="rdt", .by = "hv024", .subtitle = "Parasitemia (RDT)", survey = survey)
svy_forest_by(.var="rdt", .by = "hv000",  .subtitle = "Parasitemia (RDT)", survey = survey)


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
                         .dictionary = dd,
                         .subtitle = "" ){
  
  svy <- load_survey_object( 
                        # printout = TRUE ,  # un comment when testing
                        vars = variables, 
                        .country = .country, 
                        .survey = .survey,
                        .year = .year)
  
  
  if (.file == "household"){ survey = svy[[1]] 
  } else if ( .file == "children") { survey = svy[[2]] 
  } else {
    return()
  }

  # add column, 'one', to serve as a counter
  survey = update(survey, one = 1 )
  
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
            toupper(Item_Name) == toupper(.var) ) 
  
  values = factor(d$label) 
  label = paste( unique(d$Item_Name), ":" , unique(d$Item_Label) )
  
  # plot
  g =  ggplot(data = sb , aes_string( x =  .var ) ) +
    geom_bar( aes(y = counts ), stat = 'identity' ) + 
    # geom_errorbar(aes(ymin = lower, ymax = upper), width = .25) +
    # coord_flip() +
    ggtitle( paste(.country, .survey, .year)) +
    xlab(label)
  
  if ( nchar(.subtitle)>0 ){
    g = g + labs(subtitle = .subtitle)
  }
  
  return(g)
}

svy_histogram("hml35")
svy_histogram("hml32")
# 
# svy_histogram("s309")  # number of fansidar: 'ml1', but for Burkina, S307A
# svy_histogram("s307a")  # took fansidar: 'm49a', but for Burkina, S307A


