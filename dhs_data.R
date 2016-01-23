# This file replaces dhsForMAP
# creates "dhs.rda" (formerly "d.rda")
# instead of dhs_malaria.rda
# TODO: determine which surveys have variables

library(dplyr)

# run this one time to get data set

# get list of surveys and filer ####
source("file_list.r")  
nrow(files)

# clean up country names
library(countrycode)
# filter to surveys after 1999

f = files %>% 
  filter( !is.na( countrycode(country, "country.name", "country.name") )) %>%
  filter( grepl( paste(2000:2015, collapse="|"), year) ) %>%
  mutate( year = sapply(survey_year, 
                        FUN = function(x) tail(unlist(strsplit(x, " ", fixed = TRUE)), 1) ) 
          ) %>%
  count(country, survey_year, year)

f %>% count(country, survey_year) # 119 surveys
f %>% count(country) # 43 countries

# Chart of most recent survey ####
load("../malaria_atlas/africa_grid.rda")

# PMI 
pmi = c( "Angola", "Benin", "DRC", 
         "Ethiopia", "Ghana", "Guinea", "Kenya", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique", 
         "Nigeria", "Rwanda", "Senegal", "Tanzania", "Uganda", "Zambia", "Zimbabwe" )

pmi_iso3 = countrycode(pmi, "country.name", "iso3c")

last_survey = f %>% group_by(country) %>%
  summarise(year = max(year)) %>%
  right_join( africa_grid) %>% 
  mutate( pmi = id %in% pmi_iso3,
          `last survey year` = ifelse( year %in% 2012:2015, "2012-2015",
                                       ifelse(year %in% 2008:2011, "2008-2011",
                                              ifelse( !is.na(year), "<2008", NA)
                                              ) )
          )

library(ggplot2)
 ggplot( data = last_survey) +
   geom_polygon(aes(x, y, group = id, fill = `last survey year`, color = pmi), size = 1) +
   scale_color_manual( values = c("grey", "red"), guide = FALSE) +
   coord_fixed(ratio = 1) +
   geom_text(data = africa_grid_labels, aes(label = as.character(id), x= x, y = y), size = 3)

# open survey and merge files ####

# file:   Indexes:
# childrens       v001, v002, v003 (for womens), b16 (for hh member)
# TO 
# individual (womens)   v001, v002, v003 (childrens v003)
# TO
# household member    hv001, hv002, hvidx (childrens b16)
# TO
# household        hv001, hv002

   ## list surveys and variables available with data ####
  
  # from loading and merging DHS (D. Vanderelst, N. Speybroeck, 2014)
  # Matching variables
  # V001 Cluster number Children data set
  # V002 Household number in cluster Children data set
  # HV001 Cluster number Household data set
  # HV002 Household number in cluster Household data set
  
  # Design variables
  # V021 Primary sampling unit Children data set
  # V023  stratification used in some surveys, V023 is blank or is set to 'National'
  # V005 Sample weight Children data set
  # V025 Type of place of residence Children data set
  
  # Analysis variables
  # V190 Wealth index (quintile) Children data set
  # SH108A Time to get to health facility Household data set
  # B5 Child alive or dead Children data set
  
  # Example Descriptive statistics
  # Variable Range Mean (SD) Value counts
  # V021 1-300 NA NA
  # V025 1–2, 1: urban, 2: rural NA 1: 3,575, 2: 5,417
  # V190 1–5, 1: poorest, 5: richest 02.87 (1.40) 1: 2,038, 5: 1,483
  # SH108A 0–900, in minutes 61.36 (83.29) NA
  # B5 0–1, 0: no, 1: yes 00.88 (0.31) 0: 1,005, 1: 7,987

  vars = c("weight", "hv021", "hv024", "hv025", "b16",
           "hv023", # strata
           "hv001", "hv002", "hv003", "midx", "hvidx", "hv111",
            "hv105" , # household member age
           "b5", "b7", "b8", "v190", # living?, age at death, current age, wealth index
           "sh108a",  # only available from CDR 2007
           "hc1", "h22", "h21",
           "hml32", "hml35", 
           "ml0", "ml1", "ml2", "ml11", # from child file: child's type of net, fansidar during preg, has fever/cough, source of anitmal
           "hml32a", "hml32b", "hml32c", "hml32d",
           "hv227", "hv228", "hml1", "hml2", "hml12", "v461", "v459", "hml7")
 
 # initialize data.frame to hold inventory of variables in each survey
 survey_hmch_vars = cbind( data.frame(country = character(), survey_year = character(), stringsAsFactors = FALSE),
                                      as.data.frame(lapply( vars, FUN = function(x) logical()))
 )
 names(survey_hmch_vars)[3:(length(vars)+2)] = vars
 
source("openSurveyFile.R")

 # Create DF for summary
survey_summary = data.frame(
  country = character(),  
  survey_year = character(),
  file = character(), 
  nrow = integer(),
  ncol = integer(),
  stringsAsFactors = FALSE
)

# zero out data files
c = NA; w = NA; h = NA; hm = NA; hmc = NA; hmch = NA

# i = 24 # DRC 2007
# i = 46 # liberia

for (i in 1:nrow(f)){ 
.country = f[i, "country"]
.survey_year = f[i, "survey_year"]


  c = try(
    openSurveyFile(country = .country, survey_year = f[i, "survey_year"], 
                   tab = "Children's Recode")
  )
  
  
  w = try(
    openSurveyFile(country = .country, survey_year = f[i, "survey_year"], 
                   tab = "Individual Recode")
  )
  
  
  hm = try(
    openSurveyFile(country = .country, survey_year = f[i, "survey_year"], 
                   tab = "Household Member Recode")
  )
  
  
  h = try(
    openSurveyFile(country = .country, survey_year = f[i, "survey_year"], 
                   tab = "Household Recode")
  )
  
  
  paste( "the household file has", nrow(h), "rows and ", ncol(h), "columns")
  paste( "the household member file has", nrow(hm), "rows and ", ncol(hm), "columns")
  paste( "the women's file has", nrow(w), "rows and ", ncol(w), "columns")
  paste( "the childrens file has", nrow(c), "rows and ", ncol(c), "columns")
  
  # Use sapply to test if name exists in variables; regex surrounded by "\\b" means exact match
  
  vars_c = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(c))) ) 
  vars_w = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(w))) ) 
  vars_hm = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(hm))) ) 
  vars_h = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(h))) ) 
  
  
  # merge:
  ## household member with children, excluding variables already in hm
  
  c_vars_not_in_hm = setdiff( names(vars_c[vars_c == TRUE]), names(vars_hm[vars_hm == TRUE] ) )
  hm_vars_not_in_c = setdiff(  names(vars_hm[vars_hm == TRUE] ), names(vars_c[vars_c == TRUE]) )
  
  if ( !class(hm) == "try-error" & !class(c) == "try-error"  & 
       sapply( "b16", function(x) any(grepl(paste0("\\b", x, "\\b"), names(c))) ) == TRUE){
  hmc = hm[, names(vars_hm[vars_hm == TRUE]) ] %>%
    inner_join( c[, c(c_vars_not_in_hm, "v001", "v002", "v003") ],
                         by = c("hv001"="v001", "hv002"="v002",  "hvidx" = "b16") )
  
  
  paste( "the merged childrens-houshold member file has", nrow(hmc), "rows and ", ncol(hmc), "columns")
  
  vars_hmc = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(hmc))) )
  
  ## merged file with household (if needed)
  hmc_vars_not_in_h = setdiff( names(vars_h[vars_h == TRUE]), names(vars_hmc[vars_hmc == TRUE] ) )
  
  #Are there any variable to add?
    if(!class(h) == "try-error") {
  
      hmch = hmc %>% left_join(h, by=c("hv001"="hv001", "hv002"="hv002")) 
  
      paste( "the merged childrens-houshold member-household file has", nrow(hmch), "rows and ", ncol(hmch), "columns")
      vars_hmch = sapply( vars, function(x) any(grepl(paste0("\\b", x, "\\b"), names(hmch))) )
      
      # TODO survey_hmch_vars = bind_rows(survey_summary, surv_sum)
      
      } else { hmch = NA}
  } else { 
    hmch = NA
    hmc = NA
    }
  
  # summarize file sizes
  # versions of nrow and ncol that can handle when file not available
  nrow_ = function(x) if(class(x) == "try-error"| !is.data.frame(x)) NA else nrow(x)
  ncol_ = function(x) if(class(x) == "try-error"| !is.data.frame(x)) NA else ncol(x)
  
  surv_sum = data.frame(
    country = .country,  
    survey_year = .survey_year,
    file = c('c', 'w', 'hm', 'h', 'hmc', 'hmch'), 
    nrow = c(nrow_(c),  nrow_(w), nrow_(hm), nrow_(h), nrow_(hmc), nrow_(hmch)),
    ncol = c(ncol_(c),  ncol_(w), ncol_(hm), ncol_(h), ncol_(hmc), ncol_(hmch)),
    stringsAsFactors = FALSE
  )
  
   surv_hmch_vars = cbind(
     data.frame( country = .country, survey_year = .survey_year, stringsAsFactors = FALSE),
     as.data.frame( t(vars_hmc) )
  )
  
  # fill in data.frames that summarise surveys
  if ( nrow(surv_sum)>0 ) survey_summary = bind_rows(survey_summary, surv_sum)
  if ( nrow(surv_hmch_vars)>0 ) survey_hmch_vars = bind_rows(survey_hmch_vars, surv_hmch_vars)
    
}

# summary results ####
survey_summary
save(survey_summary, file = "survey_summary.rda")

load("survey_summary.rda")
library(tidyr)
ss = survey_summary %>% select(-ncol) %>% spread( file, nrow) %>% 
  mutate( `hmc:c` = hmc/c) %>%
  arrange( `hmc:c` )
View(ss)
summary(ss$`hmc:c`)

# surveys with biomarkers
survey_hmch_vars %>% select(country, survey_year, hml32, hml35) %>% 
  filter(hml32 == TRUE | hml35 == TRUE) %>% View()

# countries with >1 surveys with biomarkers
survey_hmch_vars %>% select(country, survey_year, hml32, hml35) %>% 
  filter(hml32 == TRUE | hml35 == TRUE) %>% 
  group_by(country) %>% mutate(n = n()) %>% filter(n>1) %>%
  View()











table(x$hv025, useNA = 'always')

# memory cleanup
rm(c, w, h ); gc()

# calculate weighted and survey values #### 
library(survey)

# test if strata exists; some surveys have no strata (e.g. madagascar mis 2011)
has.strata.023 = nrow( as.data.frame.table( table(x$hv023) ) ) > 1
has.strata.025 = nrow( as.data.frame.table( table(x$hv025) ) ) > 1

if (has.strata.025) {
  if (has.strata.023)  { strataformula = as.formula("~hv025 + hv023")
  } else {
    strataformula = as.formula("~hv025")
  }
  } else { 
    if (has.strata.023) {strataformula = as.formula("~hv023")
    } else { strataformula = NULL }
  }

# create survey design object (svy)
          svy <- 
            svydesign( 
              ~hv021 + hv002 , # see Vanderelst/Speybroeck (this is different from Damico)
              strata = strataformula , 
              data = x , 
              weights = ~weight
            )
    
    # add a new variable 'one' that simply has the number 1 for each record 
    svy <-
      update( 
        one = 1 ,
        svy
      )

  # percent forumula that respects zero
  my.percent<- function(x) {
    if(length(x)==1) if(x==0) return(paste0(0,"%") )
    return(percent(x) )
  }

  
# ANALYSIS  
  childmort = svymean(~b5, design = svy, FUN = svymean, na.rm = TRUE, vartype = c('se', 'ci'))
  childmort
  
  survival = svyby(~b5, by = ~v190, design = svy, FUN = svymean, na.rm = TRUE, vartype = c('se', 'ci'))
  survival

    
# fetch each survey and keep data from those that have malaria test data
rdt_slide_table = function(s){ 

  # select key fields
  slide.rdt = 
    x %>% 
    # select( hhid, hvidx, hv005, hv021, hv023, hv024, hml32, hml35 ) %>% 
    rename( 
            region = hv024,
            slide = hml32,
            rdt = hml35,
            pf = hml32a,
            pm = hml32b,
            po = hml32c,
            pv = hml32d
            )  %>%
    mutate( 
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