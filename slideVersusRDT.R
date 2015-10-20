# Discordant Malaria tests
# 
# use code in getSurveyValue.R to get survey_design object from each file
#
source("getSurveyValue.R")

# hv001 is cluster
# hv002 is household
# hv003 is member number within household
# hv012 is age

# HML32 Final result of malaria from blood smear test
#           0  Negative
#           1  Positive
#           6  Test undetermined
#           7  Sample not found in lab database
#       (m) 9  Missing
#      (na)    Not applicable

# HML35 Result of malaria rapid test
#     0 Negative
#     1  Positive
#     (m) 9  Missing
#     (na)    Not applicable

# get list of dhs files available
source("file_list.r")  

# fetch each survey and keep data from those that have malaria test data
rdt_slide_table = function(s){ 
  # assign variable to data and to design
  x = s$data
  svy = s$design

  # select key fields
  slide.rdt = 
    x %>% 
    select( hhid, hvidx, hv005, hv021, hv023, hv024, hml32, hml35 ) %>% 
    rename( 
            psu = hv021,
            strata = hv023,
            region = hv024,
            slide = hml32,
            rdt = hml35)  %>%
    mutate( 
            weight = as.numeric( hv005 )
            )
  nrow(slide.rdt)
  
  if ( sum(complete.cases(slide.rdt)) == 0) {
    stop("slide and/or rdt values are all missing")
  }
  
  # remove rows with no test  
  slide.rdt = slide.rdt[complete.cases(slide.rdt),] 
  nrow(slide.rdt)
  
  # summarise by psu
  compare = 
    slide.rdt %>% 
    group_by( region ) %>%
    summarize( n = n(),
               n_slide = sum( slide %in% 0:1 ),
               n_rdt = sum( rdt %in% 0:1 ),
               slide_pos  = sum(slide),
               rdt_pos = sum(rdt),
               discordant = sum( ifelse( slide != rdt, 1, 0) ),
               slidepos_rdtneg = sum( ifelse( slide == 1 & rdt == 0, 1, 0) ),
               slideneg_rdtpos = sum( ifelse( slide == 0 & rdt == 1, 1, 0) )
               ) %>%
    mutate( `%slide` = percent(slide_pos / n_slide),
            `%rdt` = percent( rdt_pos / n_rdt),
            `%discordant` = percent( discordant / n ),
            `%slidepos_rdtneg` = percent( slidepos_rdtneg / n),
            `%slideneg_rdtpos` = percent( slideneg_rdtpos / n)
            ) %>%
    select( -n_slide, -n_rdt)

  return(compare)
}

# test:

# s = survey_data(
#   country = "Angola" , # "DRC"
#   year =  2011 , # "2013-14"
#   survey ="MIS",  # "Standard DHS"
#   tab = "Household Member Recode"
# )
# 
# rdt_slide_table(s)

# get all slide/rdts 
household_member_file = files %>% filter( file %in% 'Household Member Recode.rda') 
View(household_member_file)

d = NA # initialize variable for data.frame of results

for ( i in 1:nrow(household_member_file)){
  s = survey_data(
    country = household_member_file[i, "country"],
    survey_year = household_member_file[i, "survey_year"],
    design = FALSE
  )
  
  result = try( rdt_slide_table(s), silent = TRUE)
  
  if ( class(result) %in% "try-error" ){ d.temp = NA} else {
      d.temp = data.frame( country = household_member_file[i, "country"],
                    survey_year = household_member_file[i, "survey_year"],
                    result = result,
                    stringsAsFactors = FALSE
    )
  }

  if (is.na(d)){
    d = d.temp
  } else {
    if (!is.na(d.temp) ) d = rbind(d, d.temp)
  }
}

View(d)

# interactive charts
#  Summarize over whole survey  ####

paste( 'There are ', length(unique( d$country)) ,'countries with slide and rdt data')
paste( 'There are ', length(unique( c(d$country, d$survey_year)))  ,'surveys with slide and rdt data')

library(ggvis)

d %>% 
  group_by(country, survey_year) %>%
  summarize(rdt = 100 * sum(result.rdt_pos) / sum(result.n) ,
         slide = 100* sum(result.slide_pos) / sum(result.n) ) %>%
  as.data.frame() %>%
  ggvis(~slide, ~rdt) %>%
  layer_points( fill = ~country) %>%
  add_tooltip(function(df) df$slide) %>%
  layer_model_predictions(formula = rdt ~ slide, model = "lm", se = TRUE)
  
library(ggplot2) 
# lm_eqn function for labelling lm, from 
# http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
    eq <- substitute(italic(y) == a + b %.% italic(x), l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)  
    eq <- substitute(italic(y) == a - b %.% italic(x), l)  
  }
  
  as.character(as.expression(eq));                 
}

df = d %>% 
  group_by(country, survey_year) %>%
  summarize(rdt = 100 * sum(result.rdt_pos) / sum(result.n) ,
            slide = 100* sum(result.slide_pos) / sum(result.n) ) %>%
  as.data.frame()

  ggplot(df, aes(x = slide, y = rdt)) +
  geom_point( aes(color = country), size = 6) +
  geom_smooth(method=lm,  se=TRUE) +
  annotate("text", x = 15, y = 40, label = lm_eqn(lm(rdt ~ slide, df)), parse = TRUE ) +
  geom_abline(intercept = 0)

# summarize by REGION #####
  d %>% 
    mutate(rdt = 100 * result.rdt_pos / result.n ,
              slide = 100* result.slide_pos / result.n ) %>%
    select( country, survey_year, rdt, slide) %>%
    as.data.frame() %>%
    ggvis(~slide, ~rdt) %>%
    layer_points( fill = ~country) %>%
    add_tooltip(function(df) df$slide) %>%
    layer_model_predictions(formula = rdt ~ slide, model = "lm", se = TRUE)
  
  library(ggplot2) 
  # lm_eqn function for labelling lm, from 
  # http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph
  lm_eqn = function(m) {
    
    l <- list(a = format(coef(m)[1], digits = 2),
              b = format(abs(coef(m)[2]), digits = 2),
              r2 = format(summary(m)$r.squared, digits = 3));
    
    if (coef(m)[2] >= 0)  {
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
      eq <- substitute(italic(y) == a + b %.% italic(x), l)
    } else {
      eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)  
      eq <- substitute(italic(y) == a - b %.% italic(x), l)  
    }
    
    as.character(as.expression(eq));                 
  }
  
  df = d %>% 
    mutate(rdt = 100 * result.rdt_pos /result.n ,
           slide = 100* result.slide_pos / result.n ,
           discordant = 100 * result.discordant / result.n ,  
           slide.pos_rdt.neg = 100 * result.slidepos_rdtneg / result.n ,  
           slide.neg_rdt.pos = 100 * result.slideneg_rdtpos / result.n
           ) %>%
    as.data.frame()
  
  ggplot(df, aes(x = slide, y = rdt)) +
    geom_point( aes(color = country), size = 4) +
    geom_smooth(method=lm,  se=TRUE) +
    annotate("text", x = 15, y = 40, label = lm_eqn(lm(rdt ~ slide, df)), parse = TRUE ) +
    geom_abline(intercept = 0)
  
  ggplot(df, aes(x = slide, y = discordant )) +
    geom_point( aes(color = country), size = 4) +
    geom_smooth(method=lm,  se=TRUE) +
    annotate("text", x = 15, y = 40, label = lm_eqn(lm( discordant ~ slide, df)), parse = TRUE ) +
    ggtitle("rdt discordant")
  
  a = ggplot(df, aes(x = slide, y = slide.pos_rdt.neg)) +
    geom_point( aes(color = country), size = 4) +
    geom_smooth(method=lm,  se=TRUE) +
    annotate("text", x = 15, y = 40, label = lm_eqn(lm( slide.pos_rdt.neg ~ slide, df)), parse = TRUE ) +
    ggtitle("rdt discordant: slide.pos_rdt.neg")
  
  b = ggplot(df, aes(x = slide, y = slide.neg_rdt.pos)) +
    geom_point( aes(color = country), size = 4) +
    geom_smooth(method=lm,  se=TRUE) +
    annotate("text", x = 15, y = 40, label = lm_eqn(lm( slide.neg_rdt.pos ~ slide, df)), parse = TRUE ) +
    ggtitle("rdt discordant: slide.neg_rdt.pos")
  
  library(gridExtra)
  grid.arrange(a, b, ncol = 1)
  
