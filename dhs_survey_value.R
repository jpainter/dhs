# load dhs survey

library(survey)  
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)

library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(dplyr)

source("dhs_variable_selection.R")
vars = c( vars(), c('c', 'hm', 'w', 'h') )

# functions for loading data files ####
source( "getSurveyGIS.R")

openSurveyFile = function(
  country = NA ,
  survey_year = NA,
  tab = NA
  )
  {
  
  file = paste0(
                ifelse( country %in% "DRC", "Congo Democratic Republic", country),
                "/", survey_year, "/", tab, ".rda")
  
  if ( !file.exists( file ) ) return(NA)

  load( file ) # file will be loaded as 'x'
  return(x)
}

# TODO: get printout working.  R does not normally print from inside a function.
load_survey_object = function( .country = "Angola", 
                .survey_year = "MIS 2011",
                dataset = TRUE, # returns dataset (x) along with survey design objects
                geo = TRUE, 
                printout = FALSE,
                vars = NA  # if not specified, will select variables from vars() [dhs_variable_selection.R]
                ){
  
  if (is.na(vars)){ 
    source("dhs_variable_selection.R")
    vars = vars()
    }
  
  c = try(
    openSurveyFile(country = .country, survey_year = .survey_year, 
                   tab = "Children's Recode")
  )
  
  s = try(
    openSurveyFile(country = .country, survey_year = .survey_year,
                   tab = "Supplemental Births Recode")
  )
  
  
  w = try(
    openSurveyFile(country = .country, survey_year = .survey_year, 
                   tab = "Individual Recode")
  )
  
  
  hm = try(
    openSurveyFile(country = .country, survey_year = .survey_year, 
                   tab = "Household Member Recode")
  )
  
  
  h = try(
    openSurveyFile(country = .country, survey_year = .survey_year, 
                   tab = "Household Recode")
  )
  
  if (geo){
  g = try(
    survey_GIS_data( country = .country, survey_year = .survey_year)
  )
  }
  
  if (printout){
    paste( "the household file has", nrow(h), "rows and ", ncol(h), "columns")
    paste( "the household member file has", nrow(hm), "rows and ", ncol(hm), "columns")
    paste( "the women's file has", nrow(w), "rows and ", ncol(w), "columns")
    paste( "the childrens file has", nrow(c), "rows and ", ncol(c), "columns")
    paste( "the GIS file has", nrow(g), "rows and ", ncol(g), "columns")
  } 
  
  c = c %>% mutate( c = 1 )
  w = w %>% mutate( w = 1 )
  hm = hm %>% mutate( hm = 1 )

  
  vars_c = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(c))) ) 
  vars_w = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(w))) ) 
  vars_hm = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(hm))) ) 
  vars_h = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(h))) ) 
  
 
  # merge 1.:
  ## household member with children, excluding variables already in hm

  if ( !class(c) == "try-error" && !is.na(c) &&
       sapply( "b16", function(x) any(grepl(paste0("\\b", x, "\\b"), names(c))) ) == TRUE)
    {
    
  c_vars_not_in_hm = setdiff( names(vars_c[vars_c == TRUE]), names(vars_hm[vars_hm == TRUE] ) )

  # full join to get both children of inteviewed women and children of women not interviewed but in house
  hmc = hm[, names(vars_hm[vars_hm == TRUE]) ] %>%
    full_join( c[, c(c_vars_not_in_hm )],
                         by = c("hv001"="v001", "hv002"="v002",  "hvidx" = "b16") )
  } else
    { hmc = hm }  

  if (printout){  
    paste( "the merged childrens-womens file has", nrow(hmc), "rows and ", ncol(hmc), "columns")
  }
    
  vars_hmc = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(hmc))) )  
   
  # merge 2.:
  ## household member-children with W, excluding variables already in hmc
  
  if ( !class(w) == "try-error" && !is.na(w) )
    {

    # rename womens weight variable
    w = w %>% rename(v005w = v005)
    
    vars_w = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(w))) ) 
    w_vars_not_in_hmc = setdiff( names(vars_w[vars_w == TRUE]), 
                                 names(vars_hmc[vars_hmc == TRUE] ) )
    
    # join all interviewed women in household
    hmcw = hmc %>%
      left_join( w[, c(w_vars_not_in_hmc, "v003", "v005w") ],
                         by = c("hv001"="v001", "hv002"="v002", "hv003" = "v003") )
  } else
      { hmcw = hmc } 
  
  if (printout){
      paste( "the merged household member-children-womens file has", nrow(hmcw), "rows and ", ncol(hmcw), "columns")
  }
  
  vars_hmcw = sapply( c(vars, "v005w"), function(x) any(grepl(paste0("\\b", x, "\\b"), names(hmcw))) )
  
  # merge 3.:
  ## merged file with household (if needed)
  
  #Are there any variable to add?
    if ( !class(h) == "try-error" && !is.na(h) )
      { 
      
        h = h %>% mutate( h = 1 )
        
        # rename household weight variable
        h = h %>% rename(hv005h = hv005)
        
        vars_h = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(h))) ) 
        h_vars_not_in_hmcw = setdiff(names(vars_h[vars_h == TRUE]), names(vars_hmcw[vars_hmcw == TRUE]) )
  
      if (length(h_vars_not_in_hmcw)>0 ) 
        {
        hmcwh = hmcw %>% 
        left_join( h[, c(h_vars_not_in_hmcw, "hv001", "hv002")], 
                  by=c("hv001"="hv001", "hv002"="hv002"))
        } else 
        { hmcwh = hmcw }
    } else 
          { hmcwh = hmcw }

  ##  join geo file 
  # geo file variable DHSID links with Household Member Recode variable hv001 (cluster number)
  if ( geo && is.data.frame(hmcwh) && !class(g) == "try-error" && !is.na(g) )
    {
    hmcwh = hmcwh %>% left_join(g, by=c("hv001"="dhsid") )
  }

  if (printout){
    paste( "the merged household member-children-womens-houshold  file has", 
             nrow(hmcwh), "rows and ", ncol(hmcwh), "columns")
  } 
  
  x = hmcwh %>% mutate(
    weight.h = as.numeric( hv005 / 1000000 ),
    weight.hm = as.numeric( hv005 / 1000000 ),
    weight.w = as.numeric( v005 / 1000000 ),
    weight.c = as.numeric( v005 / 1000000 ),
    
    hml32 = if( exists('hml32', where = hmcwh)) { ifelse(hml32>1 , NA, hml32) } else { NA},
    hml35 = if( exists('hml35', where = hmcwh)) { ifelse(hml35>1 , NA, hml35)  } else { NA},
    
    hml1.n =  if( exists('hml1', where = hmcwh)) { ifelse(hml1 %in% 0:97 , hml1, NA)  } else { NA},
    hml1.own =  if( exists('hml1', where = hmcwh)) { ifelse(hml1 > 0 , 1, 0)  } else { NA},
    hml12.anynet = if( exists('hml12', where = hmcwh)) { ifelse( hml12 %in% 1:3, 1, 0)} else { NA},
    hml12.itn = if( exists('hml12', where = hmcwh)) { ifelse( hml12 %in% 1:2, 1, 0)} else { NA}, 
    
    hv105.grp = if( exists('hv105', where = hmcwh)) { cut( hv105, breaks = c(0, 1, 5,15,25,55,Inf), include.lowest = TRUE )} else { NA},
    hv105.grp.c = if( exists('hv105', where = hmcwh)) { cut( hv105, breaks = c(0, 1, 2, 3, 4, 5 ), include.lowest = TRUE )} else { NA},
    
    b78 = if( exists('b8', where = hmcwh)) { ifelse( is.na(b8), b7, b8)} else { NA}, # age(ys) of live and dead
    b78 = if( exists('b78', where = hmcwh)) { ifelse( b78>4, NA, b78)} else { NA},
    
    # bednet usage
    # children under 5 slept under net last night: v460
    ## if have bednet

    v460a = if( exists('v460', where = hmcwh)) { ifelse(v460 == 2 , 1, v460) } else { NA},
    v460a = if( exists('v460', where = hmcwh)) {  ifelse(v460a > 1 , NA, v460a) } else { NA},
    
    # ## all children
    v460b = if( exists('v460', where = hmcwh)) { ifelse(v460 == 2 , 1, v460) } else { NA},
    v460b = if( exists('v460', where = hmcwh)) { ifelse(v460b > 1 , 0, v460b) } else { NA},
    
    # holes in the net: sh133b.  Remove DK = 8
    sh133b = if( exists('sh133b', where = hmcwh)) { ifelse(sh133b == 8 , NA, sh133b) } else { NA},
    
    
    one = 1
)
        
 if (printout){
      paste( "the completed merged file has", nrow(x), "rows and ", ncol(x), "columns")
   
  }
  
  
          
#### alternative  
# test if strata exists; some surveys have no strata (e.g. madagascar mis 2011)
has.strata.022 = nrow( as.data.frame.table( table(hmcwh$hv022) ) ) > 1
has.strata.023 = nrow( as.data.frame.table( table(hmcwh$hv023) ) ) > 1
has.strata.025 = nrow( as.data.frame.table( table(hmcwh$hv025) ) ) > 1

# sum(!(hmcwh$hv023 == hmcwh$v023), na.rm = TRUE)
# sum(!(hmcwh$hv025 == hmcwh$v025), na.rm = TRUE)

# if (has.strata.025) { # urban/rural
#   if (has.strata.023)  { 
#     strataformula.h = as.formula("~hv025 + hv023")
#     strataformula.c = as.formula("~v025 + v023")
#   } else {
#     strataformula.h = as.formula("~hv025")
#     strataformula.c = as.formula("~v025")
#   }
#   } else { 
#     if (has.strata.023) {
#       strataformula.h = as.formula("~hv023")
#       strataformula.c = as.formula("~v023")
#     } else { 
#       strataformula.h = NULL
#       strataformula.c = NULL
#       }
#   }

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

# see Vanderelst/Speybroeck (different from Damico); to include household? 
 x.h = x %>% filter(h == 1, is.na(hv021) )
 
 # household
  if ( nrow( x.h) > 0 )
      { 
         svy.h <- 
                    svydesign( 
                      ~hv021 , # psu 
                      strata = strataformula.h , 
                      data = x.h , 
                      weights = ~ weight.h
                    )
  }
        
  # svy.h <- update( one = 1 , svy.h ) # deprecated because 'one' previously defined in dataset x
  
  # svytotal( ~one , svy.h ) # ????
  # svyby( ~one , ~one , svy.h , unwtd.count )
  # svyby( ~one , ~hv025 , svy.h , unwtd.count )
  
  # childrens...
  x.c = x %>% filter( c == 1, is.na(hv021) )
  svy.c <- 
            svydesign( 
              ~v021  , # psu 
              strata = strataformula.c , 
              data =  x.c , 
              weights = ~ weight.c
            )   
  
  # womens
  x.w = x %>% filter( w == 1 , is.na(hv021) )
  svy.w <- 
            svydesign( 
              ~v021  , # psu 
              strata = strataformula.w , 
              data =  x.w , 
              weights = ~ weight.w
            )  
  
  # household member
  x.hm = x %>% filter( hm == 1, is.na(hv021) ) # weight.hm > 0
  svy.hm <- 
            svydesign( 
              ~v021  , # psu 
              strata = strataformula.hm , 
              data =  x.hm , 
              weights = ~ weight.hm
            )  
  
  vars_x = sapply( names(x), function(x) any(grepl(paste0("\\b", x, "\\b"), x)) )
  
  if (dataset){
    return( list(svy.h, svy.c, svy.w, svy.hm, vars_x, x))
  }  else {
    return( list(svy.h, svy.c, svy.w, svy.hm, vars_x))
  }    
  
  
}

# test / not run
  # svy = load_survey_object(printout = T)
  # svy.h = svy[[1]]
  # svy.c = svy[[2]]
  # varlist = svy[[3]]
  # x = svy[[4]]

  # svytotal( ~one , svy.c ) # ????
  # svyby( ~one , ~one , svy.c , unwtd.count )

  # svy = load_survey_object()
  # svy.h = svy[[1]]
  # svymean( ~ hml32 , svy.h , na.rm = TRUE )
  # svyby( ~ hml32 , ~ hv105.grp.c, svy.h, svymean, na.rm = TRUE )
  
  # svytotal( ~one , svy.h ) # ????
  # svyby( ~one , ~one , svy.h , unwtd.count )
  
  # Mortality (b5 is actually survived, mortality is 1- result below)
  # svyby( ~ one , ~ b5 ,  svy.c , unwtd.count )
  # svymean(~ b5, svy.c, na.rm = TRUE)
  # svytable(~ b5 + v025, svy.c, round=TRUE) 
  # svyby( ~ b5 ,  ~ v025, svy.c , svymean ,  na.rm = TRUE  )
  # svyby( ~ b5 ,  ~ b78, svy.c , svymean ,  na.rm = TRUE  )