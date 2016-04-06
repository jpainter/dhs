# what is rdt mean?  Depends on which file/svy design...
# for this example I used Benin, 2011-2012  (have not looked at others)

# load files from dhs_data

# svy is design of hmc

# make design for childrens file, c
svy_c <- 
  svydesign( 
    ~psu , 
    strata = ~strata , 
    data = c , 
    weights = ~weight
  )

# unweighted hmc
sum(hmc$hml32 %in% 1) / sum(hmc$hml32 %in% 0:1)

# weighted hmc
sum((hmc$hml32 %in% 1)*hmc$weight) / sum((hmc$hml32 %in% 0:1)*hmc$weight)

# # weighted childrens
sum((hmc$hml32 %in% 1)*hmc$v005, na.rm = TRUE) / sum((hmc$hml32 %in% 0:1)*hmc$v005, na.rm = TRUE)


# survey household member file
svymean(~hml32, svy, na.rm = TRUE)


# how do household weights from childrens file compare with weights from household memeber?
# overall, not a big difference
hmc %>% 
  select(v005, hv005, hml32) %>%
  filter( v005>0 & hv005>0) %>%  # weights in both files
  mutate(
  weight_diff = (v005 - hv005)
) %>%
  summarise(
    hm = sum(hv005/1000000, na.rm = TRUE),
    c = sum(v005/1000000, na.rm = TRUE),
    diff = mean(weight_diff, na.rm = TRUE)/100000
  )

# just a little different when restricting to children with biomarkers...
hmc %>% 
  select(v005, hv005, hml32) %>%
  filter( v005>0 & hv005>0) %>% 
  filter(hml32 %in% 0:1) %>%
  mutate(
    weight_diff = (v005 - hv005)
  ) %>%
  summarise(
    hm = sum(hv005/1000000, na.rm = TRUE),
    c = sum(v005/1000000, na.rm = TRUE),
    diff = mean(weight_diff, na.rm = TRUE)/100000
  )