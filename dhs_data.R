# This file replaces dhsForMAP
# creates "dhs.rda" (formerly "d.rda")
# instead of dhs_malaria.rda created in dhs_malaria.R
# TODO: determine which surveys have variables

library(dplyr)
suppressPackageStartupMessages(library(ggplot2))

# run this one time to get data set

# get list of surveys and files ####
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
          yr = sapply( year, function(x){ head(unlist(strsplit(x, "-")), 1 )} ),
          `last survey year` = ifelse( yr %in% 2012:2015, "2012-2015",
                                       ifelse(yr %in% 2008:2011, "2008-2011",
                                              ifelse( !is.na(yr), "<2008", NA)
                                              ) )
          )

 ggplot( data = last_survey) +
   geom_polygon(aes(x, y, group = id, fill = `last survey year`, color = pmi), size = 1) +
   scale_color_manual( values = c("grey", "red"), guide = FALSE) +
   coord_fixed(ratio = 1) +
   geom_text(data = africa_grid_labels, aes(label = as.character(id), x= x, y = y), size = 3)

# Variable list ####

  # file:   Indexes:
  # childrens       v001, v002, v003 (for womens), b16 (for hh member)
  # TO 
  # individual (womens)   v001, v002, v003 (childrens v003)
  # TO
  # household member    hv001, hv002, hvidx (childrens b16)
  # TO
  # household        hv001, hv002

  # from loading and merging DHS (D. Vanderelst, N. Speybroeck, 2014)
  # Matching variables
  # V001 Cluster number Children data set
  # V002 Household number in cluster Children data set
  # HV001 Cluster number Household data set
  # HV002 Household number in cluster Household data set
  
  # TODO add month and year of survey
 
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

  vars = c(
           "hv021", "v021", # psu
           "hv023", "v023", # strata
           "hv005", "v005", # weight
           "hv025", "v025", #urban (1), rural(2)
           "hv026", # type of town: large city(0), small city(1), town(2), country(3), missing (9)
           "hv006", "hv007", # month, year of interview
           "b16", # mothers id/line number
           "v001", "v002", "v003", # cluster, household, woman
           "hv001", "hv002", "hv003", "midx", "hvidx", "hv111",
           "hv024", 
           "b1", "b2", "b3", # child month of birth, year of birth, date of birth
           "b4", "hv104", "hc27", # sex
            "v013", # age groups
           "hv105" , # household member age
           "hv106", # place of residence
           "hc1", "hw1", # age in months
           "b5", "b6", "b7", "b8", # living?, age at death,  age at death (imputed), current age
           "v190", #  wealth index
           "S238" , # had a fever
           "sh108a",  # only available from CDR 2007 (distance to clinic)
           "h21", "h21a", # received treatment for fever/cough, recd no treatment
           "h22", # had fever in last 2 weeks
           "h32a", "h32b", "h32c",#  government clinic...more detail in h32d:z
           "hml32", "hml35", # slide; rdt
           "ml13f", #  took coartem...more detail in h32a:z
           "hml32a", "hml32b", "hml32c", "hml32d", # type of malaria on slide
           "hv253", # has dwelling been sprayed
           "sh109b", # months ago was sprayed (irs)
           "hml1", "hml2", "hml9", # number nets, child under net last night,  months ago obtained net
           "hv227",  "hml20", # has net for sleeping, slept under llin,
           "hv228", # child slept under net no(0), all(1), some(2), no net(3), missing(9)
           "ml0", "ml1", "ml2", "ml11", # from child file: child's type of net, fansidar during preg, has fever/cough, source of anitmal
           "hv228", "hml1", "hml2", "hml12", "v461", "v459", "hml7",
           "hc57", # anemia level sever(1), mod(2), mild(3), not(4), missing(9)
           
           "dhsid", "latitude", "longitude"
           )
 

# functions for loading data files ####
source( "getSurveyGIS.R")

openSurveyFile = function(
  country = NA , 
  survey_year = NA,
  tab = NA
  )
  {
  file = paste0("../DHS/",
                ifelse( country %in% "DRC", "Congo Democratic Republic", country),
                "/", survey_year, "/", tab, ".rda") 
  if ( !file.exists( file ) ) return(NA)
  
  load( file ) # file will be loaded as 'x'
  return(x)
}

# initialize data.frames to hold inventory of variables and cluster in each survey ####
 survey_vars = cbind( data.frame(country = character(), survey_year = character(), stringsAsFactors = FALSE),
                                      as.data.frame(lapply( vars, FUN = function(x) logical()))
 )
 names(survey_vars)[3:(length(vars)+2)] = vars
 
 # Create DF for summary
survey_size = data.frame(
  country = character(),  
  survey_year = character(),
  file = character(), 
  nrow = integer(),
  ncol = integer(),
  stringsAsFactors = FALSE
)

# create df for cluster data
  df.vars = setNames(data.frame(matrix( integer() , ncol = length(vars), nrow = 1)), vars)
  
  dhs_clusters = data.frame(
      country = character(),
      survey_year = character()
      ) %>%
    bind_cols( df.vars[0, ] )
  
  # all var columns are integer; need lat and long to be numeric
  dhs_clusters$latitude = as.numeric( dhs_clusters$latitude )
  dhs_clusters$longitude = as.numeric( dhs_clusters$longitude )

# zero out data files ####
c = NA; w = NA; h = NA; hm = NA; hmc = NA; hmch = NA

# iterate through survey files to create a merged file for each survey 

for (i in 1:nrow(f)){ 
  
  # TODO: if survey summary already exists, skip
  
  
  # i = 24 # DRC 2007
  # i = 46 # liberia
  
.country = f[i, "country"]
.survey_year = f[i, "survey_year"]
print( paste(.country, .survey_year) )


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
  
  g = try(
    survey_GIS_data( country = .country, survey_year = f[i, "survey_year"] )
  )
   
  paste( "the household file has", nrow(h), "rows and ", ncol(h), "columns")
  paste( "the household member file has", nrow(hm), "rows and ", ncol(hm), "columns")
  paste( "the women's file has", nrow(w), "rows and ", ncol(w), "columns")
  paste( "the childrens file has", nrow(c), "rows and ", ncol(c), "columns")
  paste( "the GIS file has", nrow(g), "rows and ", ncol(g), "columns")
  
  # Use sapply to test if name exists in variables; regex surrounded by "\\b" means exact match
  
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
  
  paste( "the merged childrens-womens file has", nrow(hmc), "rows and ", ncol(hmc), "columns")
  
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
  
  paste( "the merged household member-children-womens file has", nrow(hmcw), "rows and ", ncol(hmcw), "columns")
  
  vars_hmcw = sapply( c(vars, "v005w"), function(x) any(grepl(paste0("\\b", x, "\\b"), names(hmcw))) )
  
  # merge 3.:
  ## merged file with household (if needed)
  
  #Are there any variable to add?
    if ( !class(h) == "try-error" && !is.na(h) )
      { 
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
  if ( is.data.frame(hmcwh) && !class(g) == "try-error" && !is.na(g) )
    {
    hmcwh = hmcwh %>% inner_join(g, by=c("hv001"="dhsid") )
  }

  paste( "the merged household member-children-womens-houshold  file has", 
             nrow(hmcwh), "rows and ", ncol(hmcwh), "columns")
      
  vars_hmcwh = sapply( c(vars, "hv005h", "v005w"), 
                       function(x) any(grepl(paste0("\\b", x, "\\b"), 
                                             names(hmcwh))) 
                       )
        
  # summarize file sizes ####
  # versions of nrow and ncol that can handle when file not available
  nrow_ = function(x) if(class(x) == "try-error"| !is.data.frame(x)) NA else nrow(x)
  ncol_ = function(x) if(class(x) == "try-error"| !is.data.frame(x)) NA else ncol(x)
  
  surv_size = data.frame(
    country = .country,  
    survey_year = .survey_year,
    file = c('c', 'w', 'hm', 'h', 'hmc', 'hmcw', 'hmcwh'), 
    nrow = c(nrow_(c),  nrow_(w), nrow_(hm), nrow_(h), nrow_(hmc), nrow_(hmcw), nrow_(hmcwh)),
    ncol = c(ncol_(c),  ncol_(w), ncol_(hm), ncol_(h), ncol_(hmc), ncol_(hmcw), ncol_(hmcwh)),
    stringsAsFactors = FALSE
  )
  
   surv_vars = cbind(
     data.frame( country = .country, survey_year = .survey_year, stringsAsFactors = FALSE),
     as.data.frame( t(vars_hmcwh) )
  )
  
  # fill in data.frames that summarise surveys
  if ( nrow(surv_size)>0 ) survey_size = bind_rows(survey_size, surv_size)
  if ( nrow(surv_vars)>0 ) survey_vars = bind_rows(survey_vars, surv_vars)
  
  # compile database of clusters ####
   if ( is.data.frame(hmcwh) && nrow(hmcwh)>0 )
   {
     survey.df = data.frame( country = .country,  survey_year = .survey_year)
     hmcwh = cbind( survey.df , hmcwh)
     cluster_sum =  hmcwh %>% 
         mutate(
           hml32 = if( exists('hml32', where = hmcwh)) { ifelse(hml32>1, NA, hml32) } else { NA},
           hml35 = if( exists('hml35', where = hmcwh)) { ifelse(hml35>1, NA, hml35)  } else { NA}  
           ) %>%
       group_by( country, survey_year, hv001) %>%
       summarise_each( funs(mean(., na.rm = TRUE)) ) %>%
       inner_join(
         hmcwh %>% group_by( country, survey_year, hv001) %>% count(hv001)
       )
     
     dhs_clusters = bind_rows(dhs_clusters, cluster_sum )
   }
}

# save results ####
save(survey_size, survey_vars, dhs_clusters, file = "survey_summaries.rda")

load("survey_summaries.rda")
Sys.time()


# view summary results ####

library(tidyr)
ss = survey_summary %>% select(-ncol) %>% spread( file, nrow) %>% 
  mutate( 
          dcd = hmc - hm,
          dcd_c = dcd / c) %>%
  arrange( -dcd_c )
View(ss)
summary(ss$dcd)

# surveys with biomarkers
survey_vars %>% select(country, survey_year, hml32, hml35) %>% 
  filter(hml32 == TRUE | hml35 == TRUE) %>% View()

# countries with >1 surveys with biomarkers
survey_vars %>% select(country, survey_year, hml32, hml35) %>% 
  filter(hml32 == TRUE | hml35 == TRUE) %>% 
  group_by(country) %>% mutate(n = n()) %>% filter(n>1) %>%
  View()

# function to list countries with non-missing data in a variable
nonMissingData = function( var = NA )
{
  dhs_clusters %>% filter_(paste('!is.na(', var, ')')) %>% count(country, survey_year)
}
nonMissingData("latitude")
nonMissingData("hml32") # slide
nonMissingData("hml32a") # pf
nonMissingData("hml35") # rdt










# calculate weighted and survey values #### 
library(survey)

# test if strata exists; some surveys have no strata (e.g. madagascar mis 2011)
has.strata.023 = nrow( as.data.frame.table( table(hmcwh$hv023) ) ) > 1
has.strata.025 = nrow( as.data.frame.table( table(hmcwh$hv025) ) ) > 1

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
              data = hmcwh , 
              weights = ~hv005
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