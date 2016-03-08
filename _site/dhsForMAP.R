library(dplyr)
library(countrycode)

source("../DHS/file_list.r")  

source("../DHS/getSurveyValue.R")

source("../DHS/getSurveyGIS.R")

# fetch each survey and keep data from those that have malaria test data
dataForMAP = function(gs){ 

  #TEST if RDT variable exists (hml35).  if not , add dummy 
  if ( sum(names(gs) %in% "hml35") == 0 ){ gs$hml35 = NA }
  
  #TEST if microscopy variable exists (hml32).  if not , add dummy 
  if ( sum(names(gs) %in% "hml32") == 0 ){ gs$hml32 = NA }
  
  #TEST if result variable exists (hml32).  if not , add dummy 
  if ( sum(names(gs) %in% "hml33") == 0 ){ gs$hml33 = NA }
  
  #TEST if has_net variable exists (hv227).  if not , add dummy 
  if ( sum(names(gs) %in% "hv227") == 0 ){ gs$hv227 = NA }
  
  #TEST if used_net variable exists (hv228).  if not , add dummy 
  if ( sum(names(gs) %in% "hv228") == 0 ){ gs$hv228 = NA }
  
  #TEST if age_months variable exists (hc1).  if not , add dummy 
  if ( sum(names(gs) %in% "hc1") == 0 ){ gs$hc1 = NA }
  
  #TEST if fever variable exists (hc22).  if not , add dummy 
  if ( sum(names(gs) %in% "h22") == 0 ){ gs$h22 = NA }
  
  # select key fields
  dhs = 
    gs %>% 
    select( 
            # cluster details
            hhid, hvidx,  
            hv001, hv005, hv021, hv023, hv024, 
            hv006, hv007, hc1, latitude, longitude,
            
            # malaria tests
            hml32, hml33, hml35,
            
            # bed nets
            hv227, hv228,
            
            # fever last 2 weeks..
            h22,
            
            # age, sex...
            hc1
            ) %>% 
    rename( 
            month = hv006,
            year = hv007,
            psu = hv021,
            strata = hv023,
            region = hv024,
            cluster = hv001,
            slide = hml32,
            rdt = hml35,
            age_months = hc1,
            has_net = hv227,
            used_net = hv228,
            fever = h22,
            treatment = h21, 
            age_months = hc1
            )  %>%
    mutate( 
            weight = as.numeric( hv005 )
            )
  
  nrow(dhs)
  
  # test if malaria test data available
  has.test.data = dhs$slide %in% c(0,1) | dhs$rdt %in% c(0,1) 
  has.net.data = dhs$has_net %in% c(0,1) | dhs$used_net %in% c(0,1) 
  has.data = has.test.data | has.net.data
  
  if ( sum(has.data) == 0 ) {
    stop("all slide, rdt, and net data are missing")
  }
  
  # remove rows with no test  
  dhs = dhs[has.data,] 
  nrow(dhs)
  
  # summarise by cluster
  d = 
    dhs %>% 
    group_by( cluster ) %>%
    summarise( persons = n(),
               children = sum(!is.na(age_months)),
               
               n_slide = sum( slide %in% 0:1 ),
               n_rdt = sum( rdt %in% 0:1 ),

               slide_pos  = sum(slide == 1 , na.rm = TRUE ),
               rdt_pos = sum(rdt  == 1 , na.rm = TRUE),
               result_pos = sum( result  == 1 , na.rm = TRUE),
               slide_pos_wt  = sum( (slide  == 1 )* weight, na.rm = TRUE)/1000000,
               rdt_pos_wt = sum( (rdt  == 1 ) * weight, na.rm = TRUE)/1000000,
               result_pos_wt = sum( (result  == 1) * weight, na.rm = TRUE)/1000000,
               
               n_nets = sum( has_net %in% 0:1 ),
               n_nets_used = sum( used_net %in% 0:3),
               
               has_net = sum( has_net == 1, na.rm = TRUE ),
               has_net_wt = sum( (has_net == 1) * weight, na.rm = TRUE)/1000000,
               
               used_net_all = sum( used_net == 1, na.rm = TRUE ),
               used_net_some = sum( used_net == 2, na.rm = TRUE ),
               
               # used_net_wt = sum( (used_net %in% 1 ) * weight)/1000000,
               
               n_fever = sum( fever %in% 0:1 ),
               fever = sum( fever == 1, na.rm = TRUE ),
               fever_wt = sum( (fever == 1 ) * weight, na.rm = TRUE)/1000000,
               
               n_treatment = sum( treatment %in% 0:1 ),
               treatment = sum( treatment == 1, na.rm = TRUE ),
               treatment_wt = sum( (treatment == 1 ) * weight, na.rm = TRUE)/1000000,
               

               `year start` = min(year, na.rm = TRUE),
               `year end` = max(year, na.rm = TRUE),
               `lower age` = min(age_months, na.rm = TRUE),
               `upper age` = max(age_months, na.rm = TRUE),
               latitude = max(latitude, na.rm = TRUE),
               longitude = max(longitude, na.rm = TRUE),
               weight = floor(sum(weight, na.rm = TRUE)/1000000)

               ) %>%
    mutate( 
      `dhs id` = cluster,
      citation1 = "DHS",
      `pf pos` = ifelse(is.na(rdt_pos),
                        ifelse( is.na(slide_pos), NA, slide_pos),
                        ifelse( is.na(slide_pos), rdt_pos,
                        ifelse( rdt_pos > slide_pos, 
                         rdt_pos, 
                         slide_pos)
                        )
                        ),
      n_pf = ifelse(is.na(rdt_pos),
                        ifelse( is.na(slide_pos), NA, n_slide),
                        ifelse( is.na(slide_pos), n_rdt,
                        ifelse( rdt_pos > slide_pos, 
                         n_rdt, 
                         n_slide)
                        )
                        ),
      method = ifelse(is.na(rdt_pos),
                        ifelse( is.na(slide_pos), NA, "Microscopy"),
                        ifelse( is.na(slide_pos), "RDT",
                        ifelse( rdt_pos > "Microscopy", 
                         "RDT", 
                         "Microscopy")
                        )
                        )
    )

  return(d)
}

# rdt_slide_table(s)

# get all slide/rdts 
household_member_file = files %>% filter( file %in% 'Household Member Recode.rda') 
View(household_member_file)

dhs_malaria = NA # initialize variable for data.frame of results

for ( i in 1:nrow(household_member_file)) {

  s = survey_data(
    country = household_member_file[i, "country"],
    survey_year = household_member_file[i, "survey_year"],
    design = FALSE
  )
  s = s$data
  
  g = survey_GIS_data(
    country = household_member_file[i, "country"],
    survey_year = household_member_file[i, "survey_year"]
  )
  
  if ( sum(is.na(g))>0 ){ next }
  
  # merge
  # geo file variable DHSID links with Household Member Recode variable hv001 (cluster number)
  gs = s %>% inner_join(g, by = "hv001")

  
  result = try( dataForMAP(gs), silent = TRUE)
  
  if ( class(result) %in% "try-error" ){ 
        print(paste( household_member_file[i, "country"], 
                     household_member_file[i, "survey_year"], ":", 
                     result[[1]])
              )
        d.temp = NA
    } else {
      print(paste( household_member_file[i, "country"],
                   household_member_file[i, "survey_year"], 
                   "has malaria data")
            )
      
      d.temp = result %>%
        mutate( 
                    country = household_member_file[i, "country"],
                    `country id` = countrycode(household_member_file[i, "country"], 
                                               "country.name", "iso3c"),
                    citation1 = "DHS",
                    survey_year = household_member_file[i, "survey_year"]
                    ) 
  }

  if (is.na(dhs_malaria)){
    dhs_malaria = d.temp
  } else {
    if (!is.na(d.temp) ) dhs_malaria = rbind(dhs_malaria, d.temp)
  }
}

View(dhs_malaria)

save(dhs_malaria, file = "../DHS/dhs_malaria.rda")

load("../DHS/dhs_malaria.rda")

