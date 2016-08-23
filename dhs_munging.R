# DHS munging workflow:

# reformat code as needed.  copy code to clipboard, then run the
# following line..  library(formatR); formatR::tidy_app()

# open survey file ####

# list variables: see 'dhs_variable-selection.r' or
# 'bednet_heard_effect_MattsVars.R'

source("dhs_load_survey.R")
source("dhs_variable_selection.R")
variables <- vars()

# select country survey year ####

.country = "Angola"
.survey = "MIS"
.year = 2011
.survey_year = paste(survey, year)

svy <- load_survey_object(printout = T, vars = variables, 
                          .country = .country, 
                          .survey_year = .survey_year)

# returns: svy.h, svy.c, svy.w, svy.hm, vars_x, x
svy.h <- svy[[1]]
svy.c <- svy[[2]]
svy.w <- svy[[3]]
svy.hm <- svy[[4]]
has_vars = svy[[5]]
x = svy[[6]]

# test basic stats with survey pkg ####

svytotal(~one, svy.h)
svytotal(~one, svy.c)
svytotal(~one, svy.w)
svytotal(~one, svy.hm)

# slide and rdt result
svymean(~hml32 + hml35, svy.hm, na.rm = TRUE)
svymean(~hml32 + hml35, svy.w, na.rm = TRUE)
svymean(~hml32 + hml35, svy.c, na.rm = TRUE)
svymean(~hml32 + hml35, svy.h, na.rm = TRUE)

# age group
svymean(~hv105.grp, svy.hm, na.rm = TRUE)
svymean(~hv105.grp + hml32, svy.hm, na.rm = TRUE)  # this gives a weird result!?!

# result by age group/age
svyby(~hml32, ~hv105.grp, svy.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)
svyby(~hml32, ~hv105, svy.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)
svyby(~hml35, ~hv105, svy.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)


# open dictionary ####

library(feather)
dd <- read_feather("dictionary.feather")  # about 2 sec

dict = dd %>% filter_( ~country == .country , ~survey == .survey, ~year == .year, 
                      ~Item_Name %in% toupper(names(has_vars))
                      )
View(dict)

# transform data to long form 

library(tidyr)
svh.hm_l = gather( svy.hm$variables[1:10,], variable, value)

# relate dictionary to survey ####

# for each column, substitute dictionary label for value
svy.hm_dict = svy.hm
y = svy.hm_dict$variables
for (col in 1:length(colnames(y))){
  x = toupper( colnames(y)[col] )
  
  dict_var = dict %>% filter_( ~Item_Name == x, ~file == "Household Member Recode" ) %>% 
  select( value, label) 
  
  old_value = y[, col]
  new_value = dict_var[ match( old_value, dict_var$value ), ]$label
  y[, col] = ifelse( is.na( new_value), old_value, new_value)
}
colnames(y) = paste0(colnames(y), "_")
svy.hm_dict$variables = y

# test: 
# svymean(~hml32_, svy.hm_dict, na.rm = TRUE)

# save ####

# define new variables ####

# for each survey, calculate survey-freq with CI, by country and region
# ####

# putting results into long format table:country, survey, year, var,
# mean, std, beta distribution parameters alpha and beta ####
