# Discordant Malaria tests
# 
# Get survey_design object from getSurveyValue.R
#
# e.g. Angola 2011 household member data as 'member', survey design as 'membersvy'
  # individual as member


# hv001 is cluster
# hv002 is household
# hv003 is member number within household
# hv012 is age

# HML32 Final result of malaria from blood smear test
#           0  Negative
#           1  Positive
#           6  Test undetermined
#           7  Sample not found in lab database
#       (m) 9  Missing
#      (na)    Not applicable

# HML35 Result of malaria rapid test
#     0 Negative
#     1  Positive
#     (m) 9  Missing
#     (na)    Not applicable

# get data
source("getSurveyValue.R")


rdt_slide_table = function(s){ 
  # assign variable to data and to design
  x = s$data
  svy = s$design

  # select key fields
  slide.rdt = 
    x %>% 
    select( hhid, hvidx, hv005, hv021, hv023, hv024, hml32, hml35 ) %>% 
    rename( 
            psu = hv021,
            strata = hv023,
            region = hv024,
            slide = hml32,
            rdt = hml35)  %>%
    mutate( 
            weight = as.numeric( hv005 )
            )
  nrow(slide.rdt)
  
  if ( sum(complete.cases(slide.rdt)) == 0) {
    stop("slide and/or rdt values are all missing")
  }
  
  # remove rows with no test  
  slide.rdt = slide.rdt[complete.cases(slide.rdt),] 
  nrow(slide.rdt)
  
  # summarise by psu
  compare = 
    slide.rdt %>% 
    group_by( region ) %>%
    summarize( n = n(),
               n_slide = sum( slide %in% 0:1 ),
               n_rdt = sum( rdt %in% 0:1 ),
               slide_pos  = sum(slide),
               rdt_pos = sum(rdt),
               discordant = sum( ifelse( slide != rdt, 1, 0) ),
               slidepos_rdtneg = sum( ifelse( slide == 1 & rdt == 0, 1, 0) ),
               slideneg_rdtpos = sum( ifelse( slide == 0 & rdt == 1, 1, 0) )
               ) %>%
    mutate( `%slide` = percent(slide_pos / n_slide),
            `%rdt` = percent( rdt_pos / n_rdt),
            `%discordant` = percent( discordant / n ),
            `%slidepos_rdtneg` = percent( slidepos_rdtneg / n),
            `%slideneg_rdtpos` = percent( slideneg_rdtpos / n)
            ) %>%
    select( -n_slide, -n_rdt)

  return(compare)
}

# test:

# s = survey_data(
#   country = "Angola" , # "DRC"
#   year =  2011 , # "2013-14"
#   survey ="MIS",  # "Standard DHS"
#   tab = "Household Member Recode"
# )
# 
# rdt_slide_table(s)

# get all slide/rdts 
household_member_file = files %>% filter( file %in% 'Household Member Recode.rda') 

d = NA # initialize variable for data.frame of results

for ( i in 1:nrow(household_member_file)){
  s = survey_data(
    country = household_member_file[i, "country"],
    survey_year = household_member_file[i, "survey_year"],
    tab = "Household Member Recode" ,
    design = FALSE
  )
  
  result = try( rdt_slide_table(s), silent = TRUE)
  
  if ( class(result) %in% "try-error" ){ d.temp = NA} else {
      d.temp = data.frame( country = household_member_file[i, "country"],
                    survey_year = household_member_file[i, "survey_year"],
                    result = result,
                    stringsAsFactors = FALSE
    )
  }

  if (is.na(d)){
    d = d.temp
  } else {
    if (!is.na(d.temp) ) d = rbind(d, d.temp)
  }
}

View(d)


