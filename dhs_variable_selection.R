# dhs data variable selection

# load("data_dictionary.rda")
# str(data_dictionary)
# 
# library(dplyr)
some_variables = function(){
  vars = c(
            "v000", "hv000", # country code and phase
           "hv021", "v021", # psu
           "hv023", "v023", # strata
           "hv005", "v005", # weight
           "hv025", "v025", #urban (1), rural(2)
           "hv026", # type of town: large city(0), small city(1), town(2), country(3), missing (9)
           "hv006", "hv007", # month, year of interview,
           "hv008", # interview date (CMC)
           "v008", # interview date (CMC)
           "b16", # mothers id/line number
           "v001", "v002", "v003", # cluster, household, woman
           "hv001", "hv002", "hv003", "midx", "hvidx", "hv111",
           "hv024", 
            
           "hv106", # place of residence
           
           # sex
           "b4", "hv104", "hc27", # sex
            
           # age
           "b1", "b2", "b3", # child month of birth, year of birth, date of birth
           "v013", # age groups
           "hv105" , # household member age
           "hml16", # years of child according to household/mom (e.g. corrected)
           "hml16a", # months of child according to household/mom (e.g. corrected)
           "hc1", 
           "hw1", # age in months
           
           
           "b5", "b6", "b7", "b8", # living?, age at death,  age at death (imputed), current age(yrs)
           "hw1", # childs age in months
           "v190", #  wealth index
           
           
           # fever
           "h21", "h21a", # received treatment for fever/cough, recd no treatment
           "h22", # had fever in last 2 weeks
           "S238" , # had a fever
           
           # provider
           "sh108a",  # only available from CDR 2007 (distance to clinic)
           "h32a", "h32b", "h32c",#  government clinic...more detail in h32d:z
           "hml32", "hml35", # slide; rdt
           
           # treatment
           "ml13f", #  took coartem...more detail in h32a:z
           "hml32a", "hml32b", "hml32c", "hml32d", # type of malaria on slide
           
           # IRS
           "hv253", # has dwelling been sprayed
           "sh109b", # months ago was sprayed (irs)
           
           # nets
           "hml1", "hml2", "hml9", # number nets, child under net last night,  months ago obtained net
           "hml7", # brand of bednet
           "hml10", # ITN net 
           "hml12", # type of net slept under (recode to get slept under)
           "hv227",  "hml20", # has net for sleeping, slept under llin,
           "hv228", # child slept under net no(0), all(1), some(2), no net(3), missing(9)
           "ml0", # childs type of net
           "v459", # Have mosquito bed net for sleeping (from househ
           "v460", # Children under 5 slept under bednet last night
           "v461", # respondent slept under bednet
           "sh133b", # holes in net
           
           # MIP
           "ml1", "ml2", "ml11", # fansidar during preg, has fever/cough, source of anitmal
           "m491" , # took fansidar
           "s307a", # took fansidar (burkina)
           "s309", # number of times took fansidar (burkina)
           
           # drugs taken during pregnancy
           "M49A" ,"M49B" ,"M49C" ,"M49D"  ,"M49E"  ,"M49F", "M49G" ,"M49X" ,"M49Z", "M49Y" ,     
           
           # anemia
           "hc57", # anemia level sever(1), mod(2), mild(3), not(4), missing(9)
           
           "dhsid", "latitude", "longitude"
)
  return(vars)
}


# v = data_dictionary %>% filter( var %in% toupper(vars())) 
# str(v)
# View(v)
# v %>% filter( !var %in% toupper(variables[1:12]) & !grepl("(na)|NA", value) ) %>% 
#   group_by(var, value, label) %>% summarise(n = n()) %>% View
# 

