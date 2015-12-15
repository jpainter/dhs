# load survey files and document which key variables are available

library(dplyr)
library(countrycode)
library(tidyr)

source("../DHS/file_list.r")  

source("../DHS/getSurveyValue.R")

source("../DHS/getSurveyGIS.R")

# get all slide/rdts 
household_member_file = files %>% filter( file %in% 'Household Member Recode.rda') 
View(household_member_file)

# determine which key variables are available
dhs_variables_exist = function(gs){
  cols = names(gs)
  colsList = paste(cols, sep = ",", collapse = ",")
  dhsV = 
    data.frame( 
            geo = grepl(quote(latitude), colsList),
            slide = grepl(quote(hml32), colsList),
            rdt = grepl(quote(hml35), colsList),
            result = grepl(quote(hml33), colsList),
            age_months = grepl(quote(hc1), colsList),
            has_net = grepl(quote(hv227), colsList),
            used_net = grepl(quote(hv228), colsList),
            fever = grepl(quote(h22), colsList)
            )  
  return(dhsV)
}

#####
vars = data.frame(var_name = c("geo", "slide", "rdt", "has_net", "used_net", "fever", "treatment"), 
                  dhs_name = c("latitude", "hml32", "hml35", "hv227", "hv228", "h22", "h21"),
                  stringsAsFactors = FALSE)

isin = function(var = vars[7,2], survey = gs){
  cols = names(survey)
  has_var = var %in% cols
  has_data = if (has_var){ (sum(survey[, var] != 0, na.rm = TRUE)) != 0 } else {NA}
  return( c(has_var, has_data))
}  

# test isin()
# as.data.frame( 
#   lapply(vars[,2], isin)
# )

dhs_variables = NA # initialize variable for data.frame of results
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

  # merge  
  # geo file variable DHSID links with Household Member Recode variable hv001 (cluster number)
  if ( sum(is.na(g))>0 ){ gs = s } else {
  gs = s %>% inner_join(g, by = "hv001")

  }
  
  result = as.data.frame( 
    lapply(vars[1:7,2], isin)
    )
  names(result) = vars[,1]
  rownames(result) = c("variable", "data")
  r = bind_cols(result[1,], result[2,])
  names(r) = c( paste0(vars[,1],"_variable"), paste0(vars[,1],"_data"))
  r = r[, rev(order(names(r)))]
  
  
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
      
      d.temp = r %>%
        mutate( 
                    country = household_member_file[i, "country"],
                    `country id` = countrycode(household_member_file[i, "country"], 
                                               "country.name", "iso3c"),
                    citation1 = "DHS",
                    survey_year = household_member_file[i, "survey_year"]
                    ) 
  }

  if (is.na(dhs_variables)){
    dhs_variables = d.temp
  } else {
    if (!is.na(d.temp) ) dhs_variables = rbind(dhs_variables, d.temp)
  }
}

View(dhs_variables)

# Summarise dhs_variables
dhs_variables %>%
  summarise(
            surveys = n(),
            geo = sum(geo_variable),
            geo_data = sum(geo_data, na.rm = TRUE),
            slide = sum(slide_variable), 
            slide_data = sum(slide_data, na.rm = TRUE), 
            rdt = sum(rdt_variable),
            rdt_data = sum(rdt_data, na.rm = TRUE),
            has_net = sum(has_net_variable), 
            has_net_data = sum(has_net_data, na.rm = TRUE), 
            used_net = sum(used_net_variable), 
            used_net_data = sum(used_net_data, na.rm = TRUE), 
            fever = sum(fever_variable) ,
            fever_data = sum(fever_data, na.rm = TRUE) ,
            treatment = sum(treatment_variable),
            treatment_data = sum(treatment_data, na.rm = TRUE)
  ) %>% View()

####

save(dhs_variables, file = "../DHS/dhs_variables.rda")

load("../DHS/dhs_variables.rda")

