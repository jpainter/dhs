
# packages ####
library(survey)  
options(survey.lonely.psu="adjust")

library(stringr)
library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(dplyr)
library(sp)
library(rworldmap) # this pkg has waaaay better world shapefiles
library(ggthemes)
library(ggmap); library(BH)

library(countrycode)

# PMI
pmi_africa = c( "Angola", "Benin", "DRC",
         "Ethiopia", "Ghana", "Guinea", "Kenya", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique",
         "Nigeria", "Rwanda", "Senegal", "Tanzania", "Uganda", "Zambia", "Zimbabwe" )

pmi_iso3 = countrycode(pmi_africa, "country.name", "iso3c")

africa = countrycode_data %>% filter( continent %in% 'Africa')

load("accm.rda")

year_from_survey = function( survey_year){
  
  x = unlist( strsplit( survey_year, " ") )
  x_pieces = length(x)
  y = x[x_pieces]
  y_pieces =  length( year )
  year = unlist( strsplit( y, "-") )[ y_pieces ]
  
  return(year)
}

accm_africa = accm %>% 
  filter( countrycode(country, "country.name", "iso3c") %in% africa$iso3c,
          u5m > 10 # see Cote d'Ivoire Standard AIS 2005 (4.2) and Mali Standard DHS 2001 (8.2)
          ) %>%
  mutate(
    pmi = ifelse( countrycode(country, "country.name", "iso3c") %in% pmi_iso3, "PMI", "Non-PMI" ),
    year = sapply( survey_year, FUN = function(x) year_from_survey(x) )
  ) %>% group_by( country ) %>%
  filter( n() > 1 ) # at least 2 surveys per country

View(accm_africa)

library(Cairo)  # to get anti-aliasing
CairoWin()
accm_pmi_v_NonPmi = 
  ggplot( accm_africa ) +
  geom_line( aes( x = year, y = u5m, group = country, color = country ), size = 1 ) +
  geom_point( aes( x = year, y = u5m, color = country, fill = country ) ) +
  ylab("per 1000 live births\n") +
  xlab("\nYear") +
  guides( color = FALSE, fill = FALSE) +
  theme_few(  ) +
  theme(
    axis.text = element_text(face = 'bold', size = 16),
    axis.title = element_text(face = 'bold', size = 20),
    strip.text = element_text(face = 'bold', size = 20)
  ) +
  facet_grid( pmi ~ . )

ggsave(accm_pmi_v_NonPmi, file = 'accm_pmi_v_NonPmi.jpeg', width = 18, height = 9)

# paired change
library(tidyr)

accm_africa %>% 
  group_by( country ) %>%
  arrange( country, year ) %>%
  mutate( survey_num = row_number()) %>% 
  select( country, u5m, survey_num) %>%
  spread( survey_num, u5m  ) %>% as.data.frame() %>% View()
 
mean(accm_africa$u5m)
accm_africa %>% group_by(pmi) %>% summarise( meam = mean(u5m))

library(rethinking)
d = accm_africa %>% as.data.frame() %>%
  mutate( country = factor( country),
          year = as.numeric(year) - 2000,
          pmi = ifelse(pmi == "PMI", 1, 0)) %>%
  select( country, year, pmi, u5m) 

m = alist(
  u5m ~ dnorm( u, sigma),
  u <- a[country] + by * year + bpmi * pmi ,
  a[country] ~ dnorm( 100, 50),
  by ~ dnorm( 0, 20),
  bpmi ~ dnorm( 20, 20), 
  sigma ~ dunif( 0, 50)
)

mm = map(m, d)

precis(mm, depth = 2)

m2 = alist(
  u5m ~ dnorm( u, sigma),
  u <- a[country] + by * year  ,
  a[country] ~ dnorm( 100, 50),
  by <- by0 + bpmi * pmi,
  by0 ~ dnorm( 0, 10),
  bpmi ~ dnorm( 0, 10), 
  sigma ~ dunif( 0, 50)
)
mm2 = map(m2, d)
precis(mm2, depth = 2)

mm2s = map2stan(m2, d)
precis(mm2s, depth = 2)


# versus lm
mlm = lm( u5m ~ 1 + year + as.factor(pmi) , d) 
mlm
