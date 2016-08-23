# bednet herd effect

# Mathews variable list:
#HV253-Has dwelling been sprayed against mosquito


#V461-Respondent slept under mosquito bed net


# -Type of mosquito bed net(s) slept under last ni
# ML101 (any household member?)
# ML0 (children only?)

# H47-Blood taken from child's finger/heel for testin


# -CS antimalarial taken for fever/cough
# H37F
# H37G
# H37H
# ML13F
# ML13G
# MLH
# (not sure why there are so many)

# -During pregnancy took: SP/fansidar for malaria
# M49A
# S307A (just for one survey: Burkina Faso MIS 2014)

vars = c('HV253', 'V461', 
         'HML1', 'ML101', 'ML0', 'V459', 'HV227' , # no. nets, etc.
         'HV009', 'HV012', 'HV013', # number of persons sleeping (ratio with # nets)
         'H47', 'H37F', 'H37G' , 'H37H', 
         'ML13F', 'ML13G', 'MLH', 'M49A', 
         'S307A',
         'V455', 'HA55', 'HB55', 'HC55', 'HW55' , # hemoglobin
         'HML32', 'HML33', 'HML34', 'HML35' # malaria results
         
         # TODO: include presence of species..HML32A, HML32B etc
)
  
library(dplyr)
library(feather)
# load('data_dictionary.rda') # loads 'data_dictionary'
# write_feather( data_dictionary, "data_dictionary.feather")
dd = read_feather("data_dictionary.feather")

# no. vars
length(unique(vars))

found_malaria_vars =  dd %>% mutate( malaria = 
                 grepl("malaria", Item_Label, fixed = TRUE) &
                !grepl("^S", Item_Name)  # exclude name of 'special' vars
                ) %>% 
  filter( malaria == TRUE )  %>%
  select(Item_Name)  %>% unlist() %>% as.character() %>% unique()

# missing vars; found_malaria_vars not included in 'vars'
setdiff(found_malaria_vars, vars)

dd %>% 
  filter( Item_Name %in% setdiff(found_malaria_vars, vars) &
          !grepl("^NA", Item_Label) # exclude those that are not in survey
          ) %>%
  group_by(Item_Name, Item_Label, file) %>% 
  summarise( values = paste(unique(value), collapse = "; "),
             surveys = n_distinct(country, survey, year)) %>%
  View

# HIV?
dd %>% filter( grepl("HIV", Item_Label, ignore.case = TRUE)) %>% 
  group_by(Item_Name, Item_Label) %>% 
  summarise( values = paste(unique(value), collapse = "; "),
             surveys = n_distinct(country, survey, year)) %>%
  View

