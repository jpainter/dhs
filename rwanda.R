# RWANDA
library(gridExtra)

source("svy_result.R")
# source("dhs_forest_plot.R")

#  parasitemia
svy_forest_by("hml35", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "Standard DHS 2010")


# household has mosquito net for sleeping: v459 ####
svy_histogram("v459", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "Standard DHS 2010")

a = svy_forest_by("v459", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "Standard DHS 2010")

b = svy_forest_by("v459", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "MIS 2013")

grid.arrange( a, b)

# child slept under net: V460 ####
svy_histogram("v460a", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "MIS 2013")

a = svy_forest_by("v460a", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "Standard DHS 2010")

b = svy_forest_by("v460a", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "MIS 2013")

a = svy_forest_by("v460b", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "Standard DHS 2010")

b = svy_forest_by("v460b", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "MIS 2013")

grid.arrange( a, b)

# number of bednets: hml1 ####
svy_histogram("hml1", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "Standard DHS 2010")

a = svy_forest_by("hml1", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "Standard DHS 2010")

b = svy_forest_by("hml1", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "MIS 2013")

grid.arrange( a, b)

####  TABLES ####
# for country total use .by = 'hv000' (childrens) or 'v000' (household)
# person has mosquito net for sleeping: v459 ####

svy_var("v459" , .survey = "women", .country = "Rwanda", .survey_year = "MIS 2013")
svy_histogram("v459" , .survey = "women", .country = "Rwanda", .survey_year = "Standard DHS 2010")
svy_table( "v459", .by = "hv000" , .survey = "women", .country = "Rwanda", .survey_year = "Standard DHS 2010")
svy_table("v459" , .survey = "women", .country = "Rwanda", .survey_year = "Standard DHS 2010")
svy_table("v459" , .survey = "women", .country = "Rwanda", .survey_year = "MIS 2013")

# household has mosquito net for sleeping: hv227 ####

svy_var("hv227" , .survey = "household", .country = "Rwanda", .survey_year = "MIS 2013")
svy_histogram("hv227" , .survey = "household", .country = "Rwanda", .survey_year = "Standard DHS 2010")
svy_table( "hv227", .by = "hv000" , .survey = "household", .country = "Rwanda", .survey_year = "Standard DHS 2010")
svy_table("hv227" , .survey = "household", .country = "Rwanda", .survey_year = "Standard DHS 2010")
svy_table("hv227" , .survey = "household", .country = "Rwanda", .survey_year = "MIS 2013")

# TODO holes in the net: SH133B ####

a = svy_histogram("sh133b", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "Standard DHS 2010")

b = svy_histogram("sh133b", .subtitle = "results by regions", .survey = "household",
              .country = "Rwanda", .survey_year = "MIS 2013")




# proposed hmis data to eval surv artifact hypothesis ######

d = data.frame(
  district = rep(letters[1:10], 24),
  year = rep( c( rep(2012, 6), rep(2013, 6), rep(2014, 6), rep(2015, 6)) , 10) ,
  source = rep( c(rep('HCF', 2), rep('CHW', 2), rep('Private',2)), 40),
  age = rep( c("<5", ">5"), 120),
  count = rpois(240, 100)
) 

View(d)  

d.sources = d %>%
  group_by( source, year, age) %>%
  summarise( count = sum(count))

ggplot( d.sources, aes(x = year, y = count, color = age, group = age)) +
  geom_line() +
  facet_grid( source ~ .)
  
  
