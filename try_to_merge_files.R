# try to merge...Angola MIS 2011

library(dplyr)
source("getSurveyValue.R")

# household
h = survey_data(tab = "Household Recode")
names(h$data)[1:20]
nrow(h$data) # 40,600

h_index = hm$data %>% select(hv001, hv002)
nrow(h_index) # 40,600

nrow(unique(hm_index)) # 40,600


# household member
hm = survey_data(tab = "Household Member Recode")
names(hm$data)[1:20]
nrow(hm$data) # 40,600

hm_index = hm$data %>% select(hv001, hv002, hvidx)
nrow(hm_index) # 40,600

nrow(unique(hm_index)) # 40,600


# children
c = survey_data(tab = "Children's Recode")
names(c$data)[1:20]

c_index = c$data %>% select(v001, v002, v003, hidx)
nrow(c_index) # 8,242

nrow(unique(c_index)) #5,331
length(unique(c$data$hidx))# 5
length(unique(c$data$caseid)) #5,331

## relstionship of hidx with v003 --- 'both are'index to birth history' and  'line number'
sum(c$data$v003 == c$data$hidx) # 2,584 

# women
w = survey_data(tab = "Individual Recode")
names(w$data)[1:20]

w_index = w$data %>% select(v001, v002, v003)
nrow(w_index) # 8,589

nrow(unique(w_index)) #8,589
length(unique(w$data$hidx))# 5
length(unique(w$data$caseid)) #8,589

# Join children and household

hc = h$data %>% inner_join(c$data, by=c("hv001"="v001", "hv002"="v002"))

nrow(hc) # 8242 PERFECT 


# Join children and household member

hmc = hm$data %>% inner_join(c$data, by=c("hv001"="v001", "hv002"="v002", "hvidx" = "v003"))

nrow(hmc) # 8242 PERFECT when hvidx = v003


# Join Women and household member

hmw = hm$data %>% inner_join(w$data, by=c("hv001"="v001", "hv002"="v002", "hvidx"="v003"))

nrow(hmw) # 22374 Wow--NOT CLOSE when hv003 = v003
nrow(hmw) # 8589 PERFECT when hv003 = v003

# Join Women and children member

wc = w$data %>% inner_join(c$data, by=c("v001"="v001", "v002"="v002", "v003"="v003"))
nrow(wc) # 8,242 All children
# women without children = 
wc_ = w$data %>% anti_join(c$data, by=c("v001"="v001", "v002"="v002", "v003"="v003"))
nrow(wc_) # 3,258. 

## All women, including children
w_c = w$data %>% left_join(c$data, by=c("v001"="v001", "v002"="v002", "v003"="v003"))
nrow(w_c) # 11500 ok .  should equal # records of inner join plus anti_join: 8242 + 3258 = 11500
