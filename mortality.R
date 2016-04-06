# U5 mortality

load('data_dictionary.rda') # loads 'data_dictionary'
dic = data_dictionary %>% filter(country %in% "Burkina Faso")
View(dic)

source( "dhs_load_survey.R" )

                        .survey = "childrens"  
                        .country = "Benin" 
                        .survey_year = "Standard DHS 2001" #   "Standard DHS 2006" 
                       
  
  svy = load_survey_object( .country = .country, 
                            dataset = FALSE, 
                            .survey_year = .survey_year ,
                            geo = FALSE
  )
  
  
  if (.survey == "household"){ survey = svy[[1]] 
  } else if ( .survey == "childrens") { survey = svy[[2]] }
 
  names(svy[[3]])  # list of variables
  
  df = data.frame(Data = svy[[3]], row.names = names(svy[[3]]))  # survey structure
  
  str(survey)
  
  # U% mortality direct rate
  # See pgs 90-94, http://dhsprogram.com/pubs/pdf/DHSG1/Guide_to_DHS_Statistics_29Oct2012_DHSG1.pdf
  
  age_breaks = c(0, 1, 3, 6, 12, 24, 36, 48, 60 )
  
  cohort = function( i = 1, interview_date, birth_date, alive, death_age  ){
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
    
  component_survival = function( i = 1, interview_date, birth_date, alive, death_age  ){
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
    
    # for most recent 5 year period--the only data we have available, no children in cohort A
    component_survival = 
      ifelse( cohort %in% c("b") &  alive == 1, 1,
              ifelse( cohort %in% c("a", "c") & alive == 1, 0.5,
                      ifelse( cohort %in% c("b", "c") & !(alive == 1) & death_age %in% a1:au , 0,
                              NA 
              )))
    
    return(component_survival)
  }
  

  s <- update( 
    age_cmc = v008 - b3 , # measured age (CMC). interview data - dob. If dead, what current age would be.
    age_segment = cut( age_cmc, breaks = age_breaks , 
                       labels  = c( "0", "1-2", "3-5", "6-11", "12-23", "24-35", "36-47", "48-59" ),
                       include.lowest = TRUE,  right = FALSE),
    
    # cohort =   cohort(i = 5, interview_date = v008, birth_date = b3, alive = b5, death_age = b7),
    cohort1 =  component_survival(i = 1, interview_date = v008, birth_date = b3, alive = b5, death_age = b7) ,
    cohort1n =  ifelse(cohort1 == 0 | cohort1 == 1, 1, .5) , 
    
    cohort2 =  component_survival(i = 2, interview_date = v008, birth_date = b3, alive = b5, death_age = b7) ,
    cohort2n =  ifelse(cohort2 == 0 | cohort1 ==1, 1, .5) , 
    
    cohort3 =  component_survival(i = 3, interview_date = v008, birth_date = b3, alive = b5, death_age = b7) ,
    cohort3n =  ifelse(cohort3 == 0 | cohort3 ==1, 1, .5) ,  
    
    cohort4 =  component_survival(i = 4, interview_date = v008, birth_date = b3, alive = b5, death_age = b7) ,
    cohort4n =  ifelse(cohort4 == 0 | cohort4 ==1, 1, .5) , 
    
    cohort5 =  component_survival(i = 5, interview_date = v008, birth_date = b3, alive = b5, death_age = b7) , 
    cohort5n =  ifelse(cohort5 == 0 | cohort5 ==1, 1, .5) , 
    
    cohort6 =  component_survival(i = 6, interview_date = v008, birth_date = b3, alive = b5, death_age = b7) ,
    cohort6n =  ifelse(cohort6 == 0 | cohort6 ==1, 1, .5) , 
    
    cohort7 =  component_survival(i = 7, interview_date = v008, birth_date = b3, alive = b5, death_age = b7) ,
    cohort7n =  ifelse(cohort7 == 0 | cohort7 ==1, 1, .5) , 
    
    cohort8 =  component_survival(i = 8, interview_date = v008, birth_date = b3, alive = b5, death_age = b7),
    cohort8n = ifelse(cohort8 == 0 | cohort8 ==1, 1, .5) 

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
  
  for ( i in 1:8 ){
    
    x = svyratio( as.formula(paste0("~cohort", i)) , as.formula(paste0("~cohort", i, "n")), s , na.rm = TRUE)
      x1 = unlist(x[1])
      x2 = unlist(x[2])
      
      print(i); print(x1); print(x2)
    
      samples = 100000
    
    if ( i ==1 ){ 
        px =  x1
        pxb = rnorm( samples , x1, sqrt(x2) )
        
      } else {
        px = px * x1
        pxb = pxb * rnorm( samples , x1, sqrt(x2) )
      }
    
  }
  
  # (1 - px) * 1000
  
  # posterior distribution of U5 mortality
  pxb = 1 - pxb # convert from survival to mortality
  mean(pxb * 1000)
  sd(pxb * 1000)
  hist( pxb * 1000 )
  
  # can make similar distribution form mean and sd
  # xx = rnorm( samples ,  mean(pxb * 1000),  sd(pxb * 1000) )
  # hist(xx)
  
  
  

  
