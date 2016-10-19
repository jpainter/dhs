# load dhs survey

library(survey)  
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)

library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(dplyr)


# functions for loading data files ####
source( "getSurveyGIS.R")

openSurveyFile = function(
  country = NA ,
  # survey_year = NA,
  survey = NA,
  year = NA,
  tab = NA
  )
  {
 
  x = NULL
  
  file = paste0(
                ifelse( country %in% "DRC", "Congo Democratic Republic", country),
                "/", survey, " ", year,  "/", tab, ".rda")
  
  if ( file.exists( file ) ){
      load( file ) # file will be loaded as 'x'
      
  } else {
   
    filecodes = data_frame( abrev  = c("kr", "br", "ir", "pr", "hr", "hr" ), 
                            full = c("Children's Recode","Supplemental Births Recode","Individual Recode",
                                     "Household Member Recode", "Household Recode", "Supplemental Household Recode")) 
    
    dir = 
      paste0(  country, "/", year, "_", survey, "/"  )
    
    files = list.files(dir)
    
    prefix = paste0(  tolower( countrycode( country, "country.name", "iso2c") ),
                      filecodes$abrev[ match(tab, filecodes$full) ] )
    
    middle = substr( files[-grep(prefix, files, fixed=T)][1], 5, 8 )
    
    suffix = ".rds"
    
    file = paste0( dir, prefix, middle, suffix) 
    
    if ( file.exists( file ) ){
      x = readRDS( file ) # file will be loaded as 'x'
    }
    
  }
  
  return(x)
}

# TODO: get printout working.  R does not normally print from inside a function.
load_survey_object = function( 
                .country = "Angola", 
                # .survey_year = "MIS 2011",
                .year = 2011 ,
                .survey = "DHS",
                dataset = TRUE, # returns dataset (x) along with survey design objects
                geo = FALSE, 
                printout = FALSE,
                vars = NULL  # if not specified, will select variables from vars() [dhs_variable_selection.R]
                ){
  
  # no vars given, get basic list of variables
  if ( is.null(vars) ){ 
    source("dhs_variable_selection.R")
    vars = some_variables()
  } 
  
  linking_vars = c("hv001", "v001", "hv002", "v002", "hvidx", "b16", "hv003" , "v003", "hv021", "v021" )
  weight_vars =  c("v005", "hv005", 'weight.c', 'weight.hm', 'weight.w', 'weight.h')
  vars = unique( c( vars, linking_vars, weight_vars ) ) %>% tolower
  vars = vars[order(vars)]
  
  if (printout){ cat(vars) }
  
  c = try(
    openSurveyFile(country = .country, survey = .survey, year = .year,
                   tab = "Children's Recode")
  )
  
  s = try(
    openSurveyFile(country = .country,  survey = .survey, year = .year,
                   tab = "Supplemental Births Recode")
  )
  
  
  w = try(
    openSurveyFile(country = .country,  survey = .survey, year = .year,
                   tab = "Individual Recode")
  )
  
  
  hm = try(
    openSurveyFile(country = .country, survey = .survey, year = .year,
                   tab = "Household Member Recode")
  )
  
  
  h = try(
    openSurveyFile(country = .country,  survey = .survey, year = .year,
                   tab = "Household Recode")
  )
  
  if ( class(h) == "try-error" | class(h) == "logical" | is.null(h) ){
    h = try(
      openSurveyFile(country = .country,  survey = .survey, year = .year,
                   tab = "Supplemental Household Recode")
    )
  }
  
  
  if (geo){
  g = try(
    survey_GIS_data( country = .country,  survey = .survey, year = .year)
  )
  } else {  g = NULL }
  
  if (printout){
    cat(paste( 
          "the household file has", nrow(h), "rows and ", ncol(h), "columns", "\n",
           "the household member file has", nrow(hm), "rows and ", ncol(hm), "columns", "\n",
           "the women's file has", nrow(w), "rows and ", ncol(w), "columns", "\n",
           "the childrens file has", nrow(c), "rows and ", ncol(c), "columns", "\n",
           "the GIS file has", nrow(g), "rows and ", ncol(g), "columns")
    )
  } 

  # to avoid confusion/conflict, create file specific weight variables
  c = c %>% rename( weight.c = v005 ) 
  w = w %>% rename( weight.w = v005 )
  hm = hm %>% rename( weight.hm = hv005 )
  h = h %>% rename( weight.h = hv005 )

  
  vars_c = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(c))) ) 
  vars_w = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(w))) ) 
  vars_hm = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(hm))) ) 
  vars_h = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(h))) ) 
  
  # start with household member file: hm
  
  # merge 1.:
  ## household member with children, excluding variables already in hm

  if ( !class(c) == "try-error" && !is.null(c) &&
       sapply( "b16", function(x) any(grepl(paste0("\\b", x, "\\b"), names(c))) ) == TRUE
       )
    {
    
  c_vars_not_in_hm = setdiff( names(vars_c[vars_c == TRUE]), names(vars_hm[vars_hm == TRUE] ) )

  # full join to get both children of inteviewed women and children of women not interviewed but in house
  hmc = hm[, names(vars_hm[vars_hm == TRUE]) ] %>%
    full_join( c[, c_vars_not_in_hm ],
                         by = c("hv001"="v001", "hv002"="v002",  "hvidx" = "b16") )
  } else
  { hmc = hm }  
  
  rm( hm); rm(c) 

  if (printout){  
    cat(paste( "the merged childrens-womens file has", nrow(hmc), "rows and ", ncol(hmc), "columns")
    )
  }
    
  vars_hmc = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(hmc))) )  
   
  # merge 2.:
  ## household member-children with W, excluding variables already in hmc
  
  if ( !class(w) == "try-error" && !is.na(w) )
    {
    
    vars_w = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(w))) ) 
    w_vars_not_in_hmc = setdiff( names(vars_w[vars_w == TRUE]), 
                                 names(vars_hmc[vars_hmc == TRUE] ) )
    
    # join all interviewed women in household
    hmcw = hmc %>%
      left_join( w[, c(w_vars_not_in_hmc , "v003" )],
                         by = c("hv001"="v001", "hv002"="v002", "hv003" = "v003") )
  } else
  { hmcw = hmc } 
  
  rm( hmc ) 
  
  if (printout){
      cat(paste( "the merged household member-children-womens file has", nrow(hmcw), "rows and ", ncol(hmcw), "columns"))
  }
  
  vars_hmcw = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(hmcw))) )
  
  # merge 3.:
  ## merged file with household (if needed)
  
  #Are there any variable to add?
    if ( !class(h) == "try-error" && !is.na(h) )
      { 
        
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
  
  rm( hmcw )
  
  vars_hmcwh = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(hmcwh))) )


  ##  join geo file 
  # geo file variable DHSID links with Household Member Recode variable hv001 (cluster number)
  if ( geo && is.data.frame(hmcwh) && !class(g) == "try-error" && !is.null(g) )
    {
    hmcwhg = hmcwh %>% left_join(g, by=c("hv001"="dhsid") )
  } else {
    hmcwhg = hmcwh
  }
  
  rm(  hmcwh ) 

    
  vars_hmcwhg = sapply( c(vars, "v005w"), function(x) any(grepl(paste0("\\b", x, "\\b"), names(hmcwhg))) )

  if (printout){
    cat(paste( "the merged household member-children-womens-houshold  file has", 
             nrow(hmcwhg), "rows and ", ncol(hmcwhg), "columns")
    )
  } 
  
  # remove hv005 and v005 (the original weight variables), if they are still present
  if ("v005" %in% names(hmcwhg)){ hmcwhg = hmcwhg %>% select( -v005 )}
  if ("hv005" %in% names(hmcwhg)){ hmcwhg = hmcwhg %>% select( -hv005 )}
  
  hmcwhg = hmcwhg %>% mutate(
 
    # hml32 = if( exists('hml32', where = hmcwh)) { ifelse(hml32>1 , NA, hml32) } else { NA},
    # hml35 = if( exists('hml35', where = hmcwh)) { ifelse(hml35>1 , NA, hml35)  } else { NA},
    
    # hml1.n =  if( exists('hml1', where = hmcwh)) { ifelse(hml1 %in% 0:97 , hml1, NA)  } else { NA},
    # hml1.own =  if( exists('hml1', where = hmcwh)) { ifelse(hml1 > 0 , 1, 0)  } else { NA},
    # hml12.anynet = if( exists('hml12', where = hmcwh)) { ifelse( hml12 %in% 1:3, 1, 0)} else { NA},
    # hml12.itn = if( exists('hml12', where = hmcwh)) { ifelse( hml12 %in% 1:2, 1, 0)} else { NA}, 
    
    hv105.grp = if( exists('hv105', where = hmcwhg)) { cut( hv105, breaks = c(0, 1, 5,15,25,55,Inf), include.lowest = TRUE )} else { NA}
    # hv105.grp.c = if( exists('hv105', where = hmcwh)) { cut( hv105, breaks = c(0, 1, 2, 3, 4, 5 ), include.lowest = TRUE )} else { NA},
    
    # b78 = if( exists('b8', where = hmcwh)) { ifelse( is.na(b8), b7, b8)} else { NA}, # age(ys) of live and dead
    # b78 = if( exists('b78', where = hmcwh)) { ifelse( b78>4, NA, b78)} else { NA},
    
    # bednet usage
    # children under 5 slept under net last night: v460
    ## if have bednet

    # v460a = if( exists('v460', where = hmcwh)) { ifelse(v460 == 2 , 1, v460) } else { NA},
    # v460a = if( exists('v460', where = hmcwh)) {  ifelse(v460a > 1 , NA, v460a) } else { NA},
    
    # ## all children
    # v460b = if( exists('v460', where = hmcwh)) { ifelse(v460 == 2 , 1, v460) } else { NA},
    # v460b = if( exists('v460', where = hmcwh)) { ifelse(v460b > 1 , 0, v460b) } else { NA},
    
    # holes in the net: sh133b.  Remove DK = 8
    # sh133b = if( exists('sh133b', where = hmcwh)) { ifelse(sh133b == 8 , NA, sh133b) } else { NA},
    
    # one = 1
)
      
  x = hmcwhg; rm(hmcwhg)
   
 if (printout){
      cat(paste( "the completed merged file has", nrow(x), "rows and ", ncol(x), "columns"))
  }
          
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

# see Vanderelst/Speybroeck (different from Damico); to include household? 
 x.h = x %>% filter( !is.na(weight.h), !is.na(hv021) )
 
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
  } else {
    svy.h = NULL
  }
        
  # svy.h <- update( one = 1 , svy.h ) # deprecated because 'one' previously defined in dataset x
  
  # svytotal( ~one , svy.h ) # ????
  # svyby( ~one , ~one , svy.h , unwtd.count )
  # svyby( ~one , ~hv025 , svy.h , unwtd.count )
  
  # childrens...
  x.c = x %>% filter( !is.na(weight.c), !is.na(v021) )
  svy.c <- 
            svydesign( 
              ~v021  , # psu 
              strata = strataformula.c , 
              data =  x.c , 
              weights = ~ weight.c 
            )   
  
  # womens
  x.w = x %>% filter( !is.na(weight.w) , !is.na(v021))
  svy.w <- 
            svydesign( 
              ~v021  , # psu 
              strata = strataformula.w , 
              data =  x.w , 
              weights = ~ weight.w
            )  
  
  # household member
  x.hm = x %>% filter( !is.na(weight.hm), !is.na(hv021) )
  svy.hm <- 
            svydesign( 
              ~hv021  , # psu 
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