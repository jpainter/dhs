
# function calculates u5 mortalitly
  u5m = function(survey){
  age_breaks = c(0, 1, 3, 6, 12, 24, 36, 48, 60 )
  
  cohort = function( i = 1, interview_date = v008, birth_date = b3  ){
    t1 = interview_date - 60  # date of interview(CMC) - 60
    tu = interview_date
    a1 = age_breaks[ i ] 
    au = age_breaks[ i + 1 ] - 1
    t1au = t1 - au
    t1a1 = t1 - a1
    tuau = tu - au
    tua1 = tu - a1
    
    cohort = ifelse( birth_date >= t1au & birth_date <= t1a1, "a",
                     ifelse( birth_date >= t1a1 & birth_date <= tuau, "b",
                             ifelse( birth_date >= tuau & birth_date <= tua1, "c",  NA )
                     )
    )
  }
    
  u5numerator = function( i, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ){
    
    a1 = age_breaks[ i ] 
    au = age_breaks[ i + 1 ] - 1
    
    cohort = cohort( i, interview_date, birth_date )
    
    # for most recent 5 year period--the only data we have available, no children in cohort A
    # assume that 'special' case applies for most recent 5yr period
    numer = 
      ifelse( cohort %in% c("b") & (alive == 0) & death_age %in% a1:au , 1 ,
              ifelse( cohort %in% c("a", "c") & (alive == 0) & death_age %in% a1:au , 0.5 , 0
              ))
    
    return(numer)
  }
  
  u5denominator = function( i, interview_date = v008, birth_date = b3, alive = b5, death_age = b7  ){

    cohort = cohort( i, interview_date, birth_date )
    
    denom = 
      ifelse( cohort %in% c("b"), 1,
              ifelse( cohort %in% c("a", "c"), 0.5, 0 
              ))
    
    return(denom)
  }
  

  s <- update( 
    age_cmc = v008 - b3 , # measured age (CMC). interview data - dob. If dead, what current age would be.
    age_segment = cut( age_cmc, breaks = age_breaks , 
                       labels  = c( "0", "1-2", "3-5", "6-11", "12-23", "24-35", "36-47", "48-59" ),
                       include.lowest = TRUE,  right = FALSE),
    
    # cohort =   cohort(i = 5, interview_date = v008, birth_date = b3, alive = b5, death_age = b7),
    cohort1n =  u5numerator(i = 1, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) ,
    cohort1d =  u5denominator(i = 1, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) , 
    
    cohort2n =  u5numerator(i = 2, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) ,
    cohort2d =  u5denominator(i = 2, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) , 
    
    cohort3n =  u5numerator(i = 3, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) ,
    cohort3d =  u5denominator(i = 3, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) , 
    
    cohort4n =  u5numerator(i = 4, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) ,
    cohort4d =  u5denominator(i = 4, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) , 
    
    cohort5n =  u5numerator(i = 5, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) ,
    cohort5d =  u5denominator(i = 5, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) , 
    
    cohort6n =  u5numerator(i = 6, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) ,
    cohort6d =  u5denominator(i = 6, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) , 
    
    cohort7n =  u5numerator(i = 7, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) ,
    cohort7d =  u5denominator(i = 7, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) , 
    
    cohort8n =  u5numerator(i = 8, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 ) ,
    cohort8d =  u5denominator(i = 8, interview_date = v008, birth_date = b3, alive = b5, death_age = b7 )  

    , survey ) 
  
  # table(s$variables$cohort1)
  # svytotal( ~cohort1 , s, na.rm = TRUE)  
  # svytotal( ~cohort1n , s , na.rm = TRUE)
  # svyratio( ~cohort1, ~cohort1n, s, na.rm = TRUE)
  # 
  # table(s$variables$cohort3)
  # svytotal( ~cohort3 , s, na.rm = TRUE)  
  # svytotal( ~cohort3n , s , na.rm = TRUE)
  # svyratio( ~cohort3, ~cohort3n, s, na.rm = TRUE)
  # 
  # table(s$variables$cohort8)
  # svytotal( ~cohort8 , s, na.rm = TRUE)  
  # svytotal( ~cohort8n , s , na.rm = TRUE)
  # svyratio( ~cohort8, ~cohort8n, s, na.rm = TRUE)
  
  samples = 100000
        
  for ( i in 1:8 ){
    
    x = svyratio( as.formula(paste0("~cohort", i, "n")) , 
                  as.formula(paste0("~cohort", i, "d")), 
                  s , na.rm = TRUE)
    
      x1 = unlist(x[1]) # component mortality rate
      x2 = unlist(x[2]) # component mortality rate standard error
      
      # n = svytotal( as.formula(paste0("~cohort", i, "n")), s , na.rm = TRUE)
      # d = svytotal( as.formula(paste0("~cohort", i, "d")), s , na.rm = TRUE)
      # n / d
                
      # print(i); print(x1); print(x2)
    
    if ( i ==1 ){ 
        ps =  1 - x1 # probability of survival
        post_ps = 1 - rnorm( samples , x1, sqrt(x2) ) # posterior of ps
        
      } else {  # multiple by subsequent ps to get total ps
        ps = ps * ( 1 - x1 )
        post_ps = post_ps * ( 1 - rnorm( samples , x1, sqrt(x2) ) )
      }
  }
  
  (1 - ps) * 1000  # u5 mortality rate
  
  # summarise posterior distribution of U5 mortality
  # subtract from one to represent mortality
  u5m = mean( (1-post_ps) * 1000 )
  u5sd = sd( (1-post_ps) * 1000 )
  
  if ( is.na(u5m) ) return()
  return( data.frame( u5m, u5sd) )
}

# Get u5 from each file...

# get list of surveys and files ####
  source("file_list.r")
  library(countrycode)
  
  f = files %>%
    filter( !is.na( countrycode(country, "country.name", "country.name") )) %>%
    filter( grepl( paste(2000:2020, collapse="|"), year) ) %>%
    mutate( year = sapply(survey_year,
                          FUN = function(x) tail(unlist(strsplit(x, " ", fixed = TRUE)), 1) )
    ) %>%
    count(country, survey_year, year)
  
  View(f)
  
  source( "dhs_load_survey.R" )
  
for ( i in 1:nrow(f) ){
  
  .survey = "childrens"  
  .country = f$country[i]
  .survey_year = f$survey_year[i]
  
  print( paste( .country, .survey_year ) )
  
    svy =  try(
      load_survey_object( .country = .country, 
                            dataset = FALSE, 
                            .survey_year = .survey_year ,
                            geo = FALSE
  )
    )
    
  if ( class(svy) == "try-error" ){ 
    df = data_frame( country = .country,
                       survey_year = .survey_year,
                       u5m = NA,
                       u5sd = NA)
  } else {
  
  if ( .survey == "household" ){ survey = svy[[1]] 
  } else if ( .survey == "childrens" ) { survey = svy[[2]] }
    
  u5 = u5m(survey)  
  
  df = data_frame( country = .country,
                       survey_year = .survey_year,
                       u5m = u5$u5m,
                       u5sd = u5$u5sd)
  }
    
  if ( i==1 ){
     
    accm = df
    
  } else {
    
    accm = bind_rows( accm, df)
  }
                       
}
  
  View(accm)
  save( accm, file = "accm.rda")


  
