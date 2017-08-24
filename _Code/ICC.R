
# calculate Intra Cluster Correlations

hm = readRDS( "household_member_bednet.rds"  ) %>% as_tibble()

# filter to those with parsitemia result

has.test.result = hm$hml32 %in% 0:1 | hm$hml35 %in% 0:1
hm.result = hm[ has.test.result ,  ]

cat( "There are", nrow(hm.result), "children with a either a positve or negative parsitemia result from",
     count(hm.result, country, survey, year) %>% nrow, "surveys in ",
     count(hm.result, country) %>% nrow, "countries" 
)

library(tidyverse)
library( Hmisc )

survey_list = count( hm, country, survey, year )

i = 1

.country = survey_list[ i, ]$country
.survey = survey_list[ i, ]$survey
.year = survey_list[ i, ]$year

cat( .country, .survey, .year) 

d = hm.result %>% 
    filter( country %in% .country, survey %in% .survey, year %in% .year) %>%
    mutate( 
        age = plyr::mapvalues( hml16, from= c(1:97, 98, 99 ), to = c( 1:97, NA, NA )),
        rdt = plyr::mapvalues( hml35, from = c( 0, 1, 9), to = c( TRUE, FALSE, NA ) ),
        slept =  plyr::mapvalues(hml19, from = c( 0, 1, 9), to = c( TRUE, FALSE, NA ) )
            )

nrow(d)
count( d, age )


# RDT
count( d, rdt)

## deff from hmisc
icc =  d %>% filter( !is.na(rdt), !is.na(age)) %>% 
    do( 
        icc.rdt = deff( .$rdt, .$hv021 ) 
        ) %>% unlist()

icc

## icc from aod
library(aod)
d = hm.result %>%
    filter( country %in% .country, survey %in% .survey, year %in% .year) %>%
    mutate( 
        age = plyr::mapvalues( hml16, from= c(1:97, 98, 99 ), to = c( 1:97, NA, NA )),
        rdt = plyr::mapvalues( hml35, from = c( 0, 1, 6, 9), to = c( TRUE, FALSE, NA, NA ) ),
        slept =  plyr::mapvalues(hml19, from = c( 0, 1, 9), to = c( TRUE, FALSE, NA ) )
    ) %>%
    group_by( country, year, survey, hv021 ) %>%
    summarise(
        y = sum( rdt %in% TRUE ),
        n= n()
    )

icc.aod = icc( n = nrow)

icc(n, y, rats[rats$group == "CTRL", ])
## Not run:
res <- icc(n, y, rats[rats$group == "TREAT", ], R = 5000)
res
hist(res@rho.MC)
## End(Not run)
by(rats,
   list(group = rats$group),
   function(x) icc(n, y, data = x))

# slept under net
count( d, slept)

icc =  d %>% filter( !is.na(age)) %>% 
    do( 
            icc.rdt = deff( .$rdt, .$hv021) ,
            icc.slept = deff( .$slept, .$hv021 )
        ) %>% unlist
icc

# ALl surveys

icc = hm.result %>%
    # filter( country %in% .country, survey %in% .survey, year %in% .year) %>%
    mutate( 
        age = plyr::mapvalues( hml16, from= c(1:97, 98, 99 ), to = c( 1:97, NA, NA )),
        rdt = plyr::mapvalues( hml35, from = c( 0, 1, 6, 9), to = c( TRUE, FALSE, NA, NA ) ),
        slept =  plyr::mapvalues(hml19, from = c( 0, 1, 9), to = c( TRUE, FALSE, NA ) )
    ) %>%
    # to reduce cluster/bias from household, select first from household
    group_by( country, year, survey, hv021, hv002 ) %>%
    mutate( row.num = row_number()) %>%
    filter( row.num == 1) %>%
    ungroup() %>%
    group_by( country, year, survey) %>%
    do( 
        icc.rdt = deff( .$rdt, .$hv021) ,
        icc.slept = deff( .$slept, .$hv021 ) 
    ) %>% ungroup

View(icc)

# rho
  
summary( t( sapply(icc$icc.rdt, unlist ))[,3] )
summary(t( sapply(icc$icc.slept, unlist ))[,3] )
