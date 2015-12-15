# This file replaces dhsForMAP
# makes d.rda instead of dhs_malaria.rda
# TODO: determine which surveys have variables

# run this one time to get data set

source("file_list.r")  

source("getSurveyValue.R")

library(survey)

  # percent forumula that respects zero
  my.percent<- function(x) {
    if(length(x)==1) if(x==0) return(paste0(0,"%") )
    return(percent(x) )
  }
  
# fetch each survey and keep data from those that have malaria test data
rdt_slide_table = function(s){ 
  # assign variable to data and to design
  x = s$data
  svy = s$design

  # select key fields
  slide.rdt = 
    x %>% 
    # select( hhid, hvidx, hv005, hv021, hv023, hv024, hml32, hml35 ) %>% 
    rename( 
            psu = hv021,
            strata = hv023
            # ,
#             region = hv024,
#             slide = hml32,
#             rdt = hml35,
#             pf = hml32a,
#             pm = hml32b,
#             po = hml32c,
#             pv = hml32d
            )  %>%
    mutate( 
            weight = as.numeric( hv005 ),
            pf = sum(pf==1),
            notpf = sum(pm==0:1, po==0:1, pv==0:1)
            )
  
  nrow(slide.rdt)
 
# filter complete slide and rdt 
  has.slide = sum(x$hml32 %in% 0:1) > 0
  has.rdt = sum(x$hml35 %in% 0:1) > 0
  
  if ( !(has.slide && has.rdt) ){
    stop("slide and/or rdt values are all missing")
  }
  
  # remove rows with no test  
#   slide.rdt = slide.rdt[has.data,] 
#   nrow(slide.rdt)
  
  
  # summarise by psu
  compare = 
    slide.rdt %>% 
    group_by( region, psu ) %>%
    summarise( n = n(),
               n_slide = sum( slide %in% 0:1 ),
               n_rdt = sum( rdt %in% 0:1 ),
               slide_pos  = sum(slide %in% 1),
               rdt_pos = sum(rdt %in% 1),
               slidepos_rdtneg = sum( ifelse( slide %in% 1 & rdt %in% 0, 1, 0) ),
               slideneg_rdtpos = sum( ifelse( slide %in% 0 & rdt %in% 1, 1, 0) ),
               discordant = sum(slidepos_rdtneg , slideneg_rdtpos, na.rm = TRUE)
               ) %>%
    mutate( `%slide` = my.percent(slide_pos / n_slide),
            `%rdt` = my.percent( rdt_pos / n_rdt),
            `%slidepos_rdtneg` = my.percent( slidepos_rdtneg / n),
            `%slideneg_rdtpos` = my.percent( slideneg_rdtpos / n),
            `%discordant` = my.percent( discordant / n )
            ) 

  return(compare)
}

# rdt_slide_table(s)

# get all slide/rdts 
household_member_file = files %>% filter( file %in% 'Household Member Recode.rda') 

d = NA # initialize variable for data.frame of results
d.temp= NA

# iterate through 1:nrow(household_member_file)
for ( i in 1:nrow(household_member_file) ){

    s = try(
      survey_data(
        country = household_member_file[i, "country"],
        survey_year = household_member_file[i, "survey_year"],
        design = TRUE)
    )
  
  if (sum(class(s) == "try-error")>0){ 
    print( "s try error") 
    next }
    
  result = try( rdt_slide_table(s), silent = TRUE)
  
  if ( sum(class(result) == "try-error")>0 ){ 
        print(paste( household_member_file[i, "country"], 
                     household_member_file[i, "survey_year"], ":", 
                     result[[1]])
              )
        d.temp = NA
        
    } else {
      print(paste( household_member_file[i, "country"],
                   household_member_file[i, "survey_year"]
#                    , 
#                    "has slide or rdt data")
            ))
      
      svy_mean_rdt = svymean(~hml32, s$design, na.rm = TRUE)
      
      d.temp = result %>%
        mutate( country = household_member_file[i, "country"],
                    survey_year = household_member_file[i, "survey_year"],
                mean_rdt = svy_mean_rdt[1],
                se_rdt = SE(svy_mean_rdt)[1]
    )
  }

  if (is.na(d)){
    d = d.temp
    print("d is NA")
  } else {
    if (!is.na(d.temp) ){
      d = rbind(d, d.temp)
      print( paste(nrow(d.temp), nrow(d)) )
    } 
  }
}

View(d)

save(d, file = "d.rda")