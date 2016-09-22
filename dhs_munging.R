# DHS munging workflow:

# reformat code as needed.  copy code to clipboard, then run the
# following line..  library(formatR); formatR::tidy_app()

library(data.table)
library(dplyr)
library(tibble)

# clear memory
rm( list = ls()); gc()

# create Variable list ####

# list variables: see 'dhs_variable-selection.r' or
# 'bednet_heard_effect_MattsVars.R'

  source("dhs_variable_selection.R") # for standard list of ~80 variables
  
  vars = c( some_variables(), 
            'HV253', 'V461', 
           'HML1', 'ML101', 'ML0', 'V459', 'HV227' , # no. nets, etc.
           'HV009', 'HV012', 'HV013', # number of persons sleeping (ratio with # nets)
           'H47', 'H37F', 'H37G' , 'H37H', 
           'ML13F', 'ML13G', 'ML13H',  
           'S307A',
           'V455', 'HA55', 'HB55', 'HC55', 'HW55' , # hemoglobin
           'HML32', 'HML33', 'HML34', 'HML35' # malaria results
           
           # TODO: include presence of species..HML32A, HML32B etc
  )
  
variables <- unique( tolower( vars) )
variables = variables[ order(variables) ]

# open dictionary ####

dd <- readRDS("dictionary.rds")  

# list variables and variable names
lv = dd %>% mutate( item_name = tolower(Item_Name) ) %>% 
  filter( item_name %in% variables ) %>% 
  group_by( item_name ) %>%
  count( Item_Label ) %>% 
    arrange( item_name,  -n ) %>% as.data.frame()
View(lv)
variable_names = lv; save(variable_names, file = "variable_names.rda")

# open survey list ####

source("file_list.r")

# select country survey year ####

library(countrycode)
subsahara = countrycode_data %>%
  filter( region %in% c("Middle Africa", "Western Africa", "Eastern Africa", "Southern Africa")) %>% 
  select( country.name, iso3c ) 

files = files %>% 
  mutate( country.name = countrycode(country, "country.name", "country.name")) %>%
  filter( survey!="", year >= 2000 , country.name %in% subsahara$country.name)

# not_needed = c('Bangladesh', 'Nepal', 'Pakistan', 'Philippines', 'Comoros')

survey_list = files %>% count(country, survey, year) 

View(survey_list)

# Create master survey file with selected variables ####

source("dhs_load_survey.R")

nsurveys = nrow(survey_list)
p <- progress_estimated(nsurveys)
X = NULL

for (survey_index in 1:nsurveys){
  
  p$pause(0.1)$tick()$print()
  
  .country = survey_list[survey_index, ]$country
  .iso3c = countrycode( survey_list[survey_index, ]$country, "country.name", "iso3c")
  .survey = survey_list[survey_index, ]$survey
  .year = survey_list[survey_index, ]$year
  .survey_year = paste(.survey, .year)

  print( paste(.country, .survey, .year))
  
  svy <- try( silent = TRUE,
    load_survey_object( printout = FALSE, 
                            vars = variables, 
                            .country = .country, 
                            .survey_year = .survey_year)
  )

  if ( class(svy) == "try-error" ){ next()}

  # returns: svy.h, svy.c, svy.w, svy.hm, vars_x, x
  # svy.h <- svy[[1]]
  # svy.c <- svy[[2]]
  # svy.w <- svy[[3]]
  # svy.hm <- svy[[4]]
  has_vars = svy[[5]]
  x = svy[[6]] 
  
  x$country = rep( .country, nrow(x) )
  x$iso3c = rep( .iso3c, nrow(x) )
  x$year = rep( .year, nrow(x) )
  x$survey = rep( .survey, nrow(x) )


  # refine dictionary for this country-survey 
  dict = dd %>% filter_( ~country == .country , ~survey == .survey, ~year == .year, 
                        ~Item_Name %in% toupper(names(has_vars))
                        )
  
  # relate dictionary to survey 
  
  translate = FALSE  # optional translation of values to label.  
  # NB: Does not work well for all variables

  # only for columns with >4 values (others easier to keep as numeric)
  v = dd %>% filter( tolower(Item_Name) %in% variables) %>% group_by( Item_Name, label ) %>% 
    summarise( n = n()) %>% group_by( Item_Name) %>% count(Item_Name) %>% filter(nn>4) 

  if (translate){
  y = x
for (col in 1:length(colnames(y))){
  column = tolower( colnames(y)[col] )
  
  dict_var = dict %>% 
    mutate( item_name = tolower(Item_Name)) %>%
    filter_( ~item_name == column ) %>% # , ~file == "Household Member Recode"
    count( value, label) %>% select(-n)
  
  old_value = y[, col]
  new_value = dict_var[ match( old_value, dict_var$value ), ]$label
  y[, col] = ifelse( is.na( new_value), old_value, new_value)
}
  x = y
}

  # add _ after var name to (optional)
  # colnames(y) = paste0(colnames(y), "_")
  
  # test: 
  # svymean(~hml32, sv, na.rm = TRUE)
  
  if (is.null(X) ){ X <- x; next()}
  X = rbindlist( list(X, x), use.names = TRUE, fill = TRUE)
}


# Save master file #####
object.size(X)

saveRDS(X, file = "svyX.rds")
# library(feather); write_feather(X, "svyX.feather") # too large--3.5GB


#  Load master file ####
X = loadRDS("svyX.rds")

# reduce to those surveys with testing
comma(nrow(X))

X %>% group_by(country, year, survey) %>% 
  select(country, year, survey) %>% 
  filter(row_number()==1)

has.para = X %>% group_by(country, year, survey) %>% 
  summarise( notest = sum( is.na(hml32) & is.na(hml35)) ,
             rows = n()) %>%
  filter( notest != rows)

X.para = inner_join(X, has.para) %>% select(-notest, -rows)
comma(nrow(X.para))

saveRDS(X.para, file = 'X.para.rds')

X.para = loadRDS("X.para.rda")

# subset:

xhm = X.para %>% 
  filter( country %in% 'Burkina Faso', survey %in% 'Standard DHS' , year %in% '2010' ,
         !is.na(hv021)  # survey design object, below, requires non missing value for psu (hv021)
         )  

# check dictionary
dict_x = dd %>% filter_( ~country == 'Burkina Faso' , ~survey == 'Standard DHS', ~year == '2010', 
                      ~Item_Name %in% toupper( variables )
                      )

# create survey design object
svy.x.hm  <- 
            svydesign( 
              ~hv021  , # psu 
              strata = ~hv023 , 
              data =  xhm , 
              weights = ~ weight.hm  # weights for household member file
            )  


# test basic stats with survey pkg ####

svytotal(~one, svy.h)
svytotal(~one, svy.c)
svytotal(~one, svy.w)
svytotal(~one, svy.hm)

# slide and rdt result
svymean(~hml32 + hml35, svy.x.hm, na.rm = TRUE)
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

# mean number of nets by regions
svyby(~hml1, ~hv024, svy.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)
svyby(~v459, ~hv024, svy.hm, svymean, na.rm.all = TRUE, na.rm = TRUE) #household has net


# can we use X file to generate survey designs/stats? ####
  
  # get single survey file and calculate parasitemia 
 
  svy = load_survey_object( .country = 'Burkina Faso', 
                .survey_year = 'Standard DHS 2010',
                dataset = TRUE, # returns dataset (x) along with survey design objects
                geo = FALSE, 
                printout = TRUE,
                vars = NULL  # if not specified, will select variables from vars() [dhs_variable_selection.R]
                )
    
  svy.hm = svy[[4]]
  svy.h = svy[[1]]
    
  svymean(~hml32 + hml35, svy.hm, na.rm = TRUE)
  
  svymean(~hv106, svy.hm, na.rm = TRUE) # education level
  svymean(~hv106, svy.h, na.rm = TRUE) # education level
  
  # subset master file
  load("X.para.rda")
  x = X.para %>% 
    filter( country %in% 'Burkina Faso', survey %in% 'Standard DHS' , year %in% '2010' ,
           !is.na(hv021)  # survey design object, below, requires non missing value for psu (hv021)
           )  
  
  unique(with(x, paste(country, survey, year)))
  
  # test if strata exists; some surveys have no strata (e.g. madagascar mis 2011)
  has.strata.022 = nrow( as.data.frame.table( table(x$hv022) ) ) > 1
  has.strata.023 = nrow( as.data.frame.table( table(x$hv023) ) ) > 1
  has.strata.025 = nrow( as.data.frame.table( table(x$hv025) ) ) > 1
 
  if (has.strata.022) { # urban/rural
    strataformula.h = as.formula("~hv022 ")
    strataformula.c = as.formula("~v022 ")
    strataformula.w = as.formula("~v022 ")
    strataformula.hm = as.formula("~hv022 ")
  } else {
      strataformula.h = NULL
      strataformula.c = NULL
      strataformula.w = NULL
      strataformula.hm = NULL
  }
  
  svy.x.hm  <- 
            svydesign( 
              ~hv021  , # psu 
              strata = strataformula.hm , 
              data =  xhm , 
              weights = ~ weight.hm
            )  
  
  svymean(~hml32 + hml35, svy.x.hm, na.rm = TRUE)
  svyby(~hml32 + hml35, ~hv025, svy.x.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)
  
  svymean(~hv106, svy.x.hm, na.rm = TRUE)

  svy.x.h <- 
            svydesign( 
              ~hv021  , # psu 
              strata = strataformula.h , 
              data =  xhm , 
              weights = ~ weight.h
            )  
  svymean(~hv106, svy.x.h, na.rm = TRUE)
  
  # YES!
  

  

# can we use STAN/rethinking file to generate survey stats? ####

  xhm = X.para[!is.na(x$hv021),]
  svy.x.hm  <- 
            svydesign( 
              ~hv021  , # psu 
              strata = NULL , 
              data =  xhm , 
              weights = ~ weight.hm
            )  
  
  svymean(~hml32 + hml35, svy.x.hm, na.rm = TRUE)
  svyby(~hml32, ~b4, svy.x.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)
  svyby(~hml32, ~hv024, svy.x.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)
  svyby(~hml32, ~b4 + hv024, svy.x.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)

## rethingking example to use as template ####
  
  library(rethinking)
  
    data(UCBadmit)
    d = UCBadmit
    d$male = ifelse( d$applicant.gender == "male", 1, 0)
    d$dept_id = coerce_index( d$dept )
  
  library(dplyr)
    d = d %>% select(applications, male, admit, dept_id)
  
  m = map2stan(
    alist(
      admit ~ dbinom( applications, p ),
      logit(p) <- a_dept[dept_id] + bm*male,
      a_dept[dept_id] ~ dnorm( a , sigma_dept),
      a ~ dnorm( 0, 10),
      bm ~ dnorm(0,1),
      sigma_dept ~ dcauchy(0,2)
    ),
    data =  d, warmup =500, iter = 4500, chains = 3 )
  
  precis(m, depth = 2)
  
## survey example/ single intercept  ####
  d = x[!is.na(x$hv021),]
  d$region_id = coerce_index( d$hv024) # region
  d = d %>% select(region_id, hml32) %>% filter( !is.na(hml32), !is.na(region_id) )

    m = map(
    alist(
      hml32 ~ dbinom( 1, p ),
      logit(p) <- a_psu ,
      a_psu ~ dnorm( 0, 10)
    ),
    data =  d)
    precis(m); logistic(precis(m)@output)

## map2stan ####
    
    d = X.para[!is.na(X.para$hv021),]
    d$region_id = coerce_index( d$hv024) # region
    d = d %>% select(region_id, hml32) %>% filter( !is.na(hml32), !is.na(region_id) )
    
  m = map2stan(
    
    alist(
      hml32 ~ dbinom( 1, p ),
      logit(p) <- a_region[region_id] , 
      a_region[region_id] ~ dnorm( 0, 10)
      
    ), data =  d)
  
  precis(m)

  # multiple intercept--for each region  
  m = map2stan(
    alist(
      hml32 ~ dbinom( 1, p ),
      logit(p) <- a_psu[region_id] ,  # adding sex as covariate
      a_psu[region_id] ~ dnorm( a, sigma),
      a ~ dnorm( 0, 10),
      sigma ~ dcauchy(0,2)
    ),
    data =  d)
  precis(m)
  pairs(m)
  post = extract.samples(m) 
  
  # adjusting for sex
  d = xhm
  d$region_id = coerce_index( d$hv024) # region
  d = d %>% select(region_id, hml32, b4) %>% filter( !is.na(hml32), !is.na(region_id), !is.na(b4) )

    m = map2stan(
    alist(
      hml32 ~ dbinom( 1, p ),
      logit(p) <- a_psu + bm*b4,
      a_psu[region_id] ~ dnorm( a, sigma),
      a ~ dnorm( 0, 10),
      sigma ~ dcauchy(0,2),
      bm ~ dnorm(0,10)
    ),
    data =  d)
    precis(m, depth = 2); 
    pairs(m)
    
  # MAYBE!-- it takes some...time
  

# define new variables ####

# for each survey, calculate survey-freq with CI, by country and region ####


# mean, std, beta distribution parameters alpha and beta ####
