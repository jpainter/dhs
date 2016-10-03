
# setup #####
library(feather)
library(data.table)
library(dplyr)
library(tibble)
library(survey)

# clear memory
rm( list = ls()); gc()

# list variables and variable names
load("variable_names.rda"); View(variable_names)


# Main data ####
load("X.para.rda")

# subset: ####
  .country = 'Burkina Faso'
  .survey = 'Standard DHS'
  .year = '2010'
  .survey_year =  paste(.survey, .year)
  
x = X.para %>% 
  filter( country %in% .country, survey %in% .survey , year %in% .year ,
         !is.na(hv021)  # survey design object, below, requires non missing value for psu (hv021)
         )  

# data dictionary #####

variables = colnames(x)  # variables in dataset

load("dictionary.rda") # from extractMAPdocs.R

dict_x = dictionary %>% 
  filter_( ~country == .country , ~survey == .survey, ~year == .year, 
                      ~Item_Name %in% toupper( variables )
                      )
View(dict_x)

# Survey design object ####
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
              data =  x, 
              weights = ~ weight.hm
            )  

# survey eSTIMATES ####  
  svymean(~hml32 + hml35, svy.x.hm, na.rm = TRUE)
  svyby(~hml32 + hml35, ~hv025, svy.x.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)
  
  svymean(~hv106, svy.x.hm, na.rm = TRUE)
  
  # convert svymean to df
  a = svymean(~hml32 + hml35, svy.x.hm, na.rm = TRUE)
  adf = as.data.frame(a)
  
  t = svytable(~hml32 + hml35, svy.x.hm)
  tdf = as.data.frame(t)
  
  t1 = svytable(~hml32, svy.x.hm)
  t1df = as.data.frame( t(t(t1)) )
  
  # convert svyciprop to df
  p = svyciprop(~hml32, svy.x.hm, method="lo")
  pp = cbind( as.numeric(p), as.data.frame( t(attributes(p)$ci) ) )
  names(pp)[1] = "Est."
  pp$var = names(p)
  pp
  
  # convert svyby to df
  .byvar = c("hv025", "hv024")
  .byvar = c("hv024")
  .by = as.formula( paste0("~", paste(.byvar, collapse = "+") ) )
  b = svyby(~hml32 + hml35, .by, svy.x.hm, svymean, na.rm.all = TRUE, na.rm = TRUE)
  bdf = as.data.frame( b )
  bdf
  
  ## add dictionary
  dict_xv = dict_x %>% filter( tolower(Item_Name) %in% .byvar, grepl("Member", file)) 
  
  ### joining dictionary with one variable
  bdf %>% mutate_( "value" = paste("as.character(", .byvar, ")" ) ) %>% 
                     left_join( dict_xv[,1:4] )
  ##3 joining dictionary with 2 variables
  bdf %>% mutate( by1 = as.character(hv024),
                  by2 = as.character(hv025) ) %>% 
                     left_join( dict_xv[,1:4], by = c("by1" = "value") )
  
  ### using match
  dict_xv1 = dict_x %>% filter( tolower(Item_Name) %in% .byvar[1], grepl("Member", file))
  dict_xv2 = dict_x %>% filter( tolower(Item_Name) %in% .byvar[2], grepl("Member", file))
  bdf[, .byvar[1]] = dict_xv1[ match(bdf[, .byvar[1]], dict_xv1$value), "label"]
  bdf[, .byvar[2]] = dict_xv2[ match(bdf[, .byvar[2]], dict_xv2$value), "label"]
  bdf
  
  #### plot...
  library(ggplot2)
  ggplot( bdf, aes(y = hv024, x = hml35, color = hv025)) + geom_point()
  
  
  # TODO  Figure out way to translate label and var names on the go...
  .variables = rownames(adf)
  dict_x = dictionary %>% 
    filter_( ~country == .country , ~survey == .survey, ~year == .year, 
                      ~Item_Name %in% toupper( .variables )
                      ) %>%
    mutate( Item_Name = tolower( Item_Name ))
  
# TODO: Rethinking DHS ####
  
  library(rethinking)
  
  d = x
  d$region_id = coerce_index( d$hv024) # region
  d$psu_id = coerce_index( d$hv021) # psu
  d = d %>% 
    filter( !is.na(hml32), !is.na(psu_id))
  
  d = d %>% 
    group_by(psu_id) %>%
    filter( !is.na(weight.hm ) ) %>%
    select(region_id, psu_id, hml32 ) %>% 
    as.data.frame()

    m = map2stan(
        alist(
          hml32 ~ dbinom( 1, p ),
          logit(p) <- a_region ,
          a_region[region_id] ~ dnorm( a, sigma),
          a ~ dnorm( 0, 10),
          sigma ~ dcauchy(0, 2)
          
        ), data =  d, warmup =500, iter = 4500, chains = 1)
    
    # same by map
    ## crude
    mm = map(
        alist(
          hml32 ~ dbinom( 1, p ),
          logit(p) <- a ,
          a ~ dnorm( 0, 10)
        ), data =  d)
    
    mmp = precis(mm) 
    unlist( logistic( mmp@output ) )
    
    ## by regions
    mm = map(
        alist(
          hml32 ~ dbinom( 1, p ),
          logit(p) <- a_region[region_id] ,
          a_region[region_id] ~ dnorm( 0, 10)
        ), data =  d)
    
    mmp = precis(mm, depth = 2) 
    logistic(mmp@output$Mean)
    
    ### Global mean???
    mu = link(mm, data = data.frame(region_id = 1:13)) # link returns original units
    apply( mu, 2, mean) # crude
    apply( mu, 2, HPDI, prob = .95)
    
    ### Weighted mean using relative weights for each region
    w = x %>% 
      select(  hv024, weight.hm ) %>%
      group_by( hv024 ) %>% 
      summarise( w = sum(as.numeric(weight.hm), na.rm = TRUE) /
                  sum(as.numeric(x$weight.hm), na.rm = TRUE) 
                 )
    
    mean( mu %*% w$w )
    HPDI( mu %*% w$w )
    
    ### compared with survey estimate:
    sm = svymean(~hml32 , svy.x.hm, na.rm = TRUE) 
    c( sm[1] - 1.96*(attributes(sm)$var)^.5, sm[1] + 1.96*(attributes(sm)$var)^.5 )
    
    ### compare regions pairwise
    post = extract.samples(mm) # post samples need logistic()
    
    compare1_2 = post$a_region[,2] - post$a_region[,1]
    hist(compare1_2)
    HPDI(compare1_2)
    prob1gtr2 = sum(compare1_2>0) / length(compare1_2)
    prob3gtr2 = sum(compare1_2<0) / length(compare1_2)
    prob1gtr2; prob3gtr2
    
    compare2_3 = post$a_region[,2] - post$a_region[,3]
    HPDI(compare2_3)
    prob2gtr3 = sum(compare2_3>0) / length(compare2_3)
    prob3gtr2 = sum(compare2_3<0) / length(compare2_3)
    prob2gtr3; prob3gtr2 
    
    # pairs(post)
    
    # TODO region and psu --- #####
    
    
  