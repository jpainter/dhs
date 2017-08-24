
# packages ####
library(survey)  
options(survey.lonely.psu="adjust")

library(stringr)
library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(dplyr)
# library(sp)
# library(rworldmap) # this pkg has waaaay better world shapefiles
library(ggthemes)
library(ggmap); library(BH)


# Forest plot by region ####
svy_forest_by = function(.var = "hml35",
                         .by = "hv024", 
                         survey = NA, 
                         .country = NA , 
                         .survey = NA ,
                         .year = NA,
                         .dictionary = NA,
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

# examples ####

# svy_forest_by(.var="hml32", .by = "hv000", .subtitle = "Parasitemia (microscopy)", survey = survey)
# svy_forest_by(.var="hml32", .by = "hv024", .subtitle = "Parasitemia (microscopy)", survey = survey)
# 
# svy_forest_by(.var="rdt", .by = "hv024", .subtitle = "Parasitemia (RDT)", survey = survey)
# svy_forest_by(.var="rdt", .by = "hv000",  .subtitle = "Parasitemia (RDT)", survey = survey)





# Histogram ####

svy_histogram = function(.var = "hml32", 
                         survey = NA, 
                         .country = NA , 
                         .survey = NA ,
                         .year = NA,
                         .dictionary = dd,
                         .subtitle = "" ){
  

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

# svy_histogram("hml35")
# svy_histogram("hml32")
# 
# svy_histogram("s309")  # number of fansidar: 'ml1', but for Burkina, S307A
# svy_histogram("s307a")  # took fansidar: 'm49a', but for Burkina, S307A


