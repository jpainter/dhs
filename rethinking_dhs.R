library(rethinking)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
library(knitr)
library( xtable)
library(dplyr)

load("survey_summaries.rda")

str(dhs_clusters)

# lm rdt v slide
# be careful not sum or take mean of slide/rdt variables as is --  they include nine!
data =  dhs_clusters %>% filter( hml32>0 & hml35>0) 

data %>% group_by(country, survey_year) %>% summarise( n = n())

# data.country = data %>% group_by(country, survey_year) %>% 
#   summarise( hml32 =  sum(hml))

plot( data$hml32, data$hml35)

model.lm = lm( hml32 ~ hml35 + 1 , data )
model.lm
plot(model.lm)

print( xtable(summary(model.lm)), type = "html" )


# compare 2 surveys
d = data %>% filter( country == "Burkina Faso") %>%
  mutate( latest = survey_year == "MIS 2014") %>% as.data.frame()

d1 =  d %>% filter(survey_year == "Standard DHS 2010") %>% as.data.frame()
d2 =  d %>% filter(survey_year == "MIS 2014") %>% as.data.frame()

  flist = alist(
    hml32 ~ dnorm( mu, sigma),
    mu ~ dnorm(.6, .3),
    sigma ~ dunif( 0, 1)
  )
  
  m1 = map( flist, d1 )
  precis(m1)

  m2 = map( flist, d2 )
  precis(m2)
  
# difference
  dlist = alist(
    hml32 ~ dnorm( mu, sigma),
    mu ~ a + delta*latest,
    a ~ dnorm(.6, .3),
    delta ~ dnorm(0, 1),
    sigma ~ rho + delta_rho*latest,
    rho ~ dunif(0,1), 
    delta_rho ~ dnorm(0,.2)
  )
  md = map( dlist, d )
  precis(md)  
  
  