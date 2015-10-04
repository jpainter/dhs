library(survey) 	# load survey package (analyzes complex design surveys)
library(foreign) 	# load foreign package (converts data files into R)
library(Hmisc)
library(ggplot2)
library(scales)
library(dplyr)
library(knitr)

# load survey ####

# by default, R will crash if a primary sampling unit (psu) has a single observation
# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
# by uncommenting this line:
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN


country = "Rwanda"
year = 2013
survey = "MIS"
tab = "Individual Recode"


# load the 2004 malawi individual recodes data.frame object
file = load( paste0("./", country, "/", survey, " ", year, "/", tab, ".rda") )

load( file ) # file will be loaded as 'x'

# convert the weight column to a numeric type
x$weight <- as.numeric( x$v005 )

x$strata <- x$v023 ## Nigeria (JP)

x$psu = x$v021 # (JP)

# create a survey design object (svy) with DHS design information
svy <- 
  svydesign( 
    ~psu , 
    strata = ~strata , 
    data = x , 
    weights = ~weight
  )

# add a new variable 'one' that simply has the number 1 for each record 

  svy <-
    update( 
      one = 1 ,
      svy
    )

# count the total (unweighted) number of records in dhs #####
# broken out by urban/rural #

  svyby(
    ~one ,
    ~v025 ,
    svy ,
    unwtd.count
  )

# calculate the mean of a linear variable #


# calculate the distribution of a categorical variable #

# v101 should be treated as a factor (categorical) variable
# instead of a numeric (linear) variable
# this update statement converts it.
# the svyby command below will not run without this

svy <-
  update( 
    v101 = factor( v101 ) ,
    svy
  )

    
# distribution of malawian 15-49 year old females - nationwide
svymean( 
  ~v101 , 
  design = svy ,
  na.rm = TRUE
)

# mosq nets (HV227)  and children<5 slept under net (HV228)
svymean(
    ~v227 ,
    svy ,
    na.rm = TRUE
  )


# by urban/rural
svyby( 
  ~HV227 , 
  ~v025 ,
  design = svy ,
  svymean , 
  na.rm = TRUE
)

# calculate the median and other percentiles #

# note that a taylor-series survey design
# does not allow calculation of standard errors

# minimum, 25th, 50th, 75th, maximum 
# hours worked in the united states
svyquantile( 
  ~surviving.children , 
  design = svy ,
  c( 0 , .25 , .5 , .75 , 1 ) ,
  na.rm = TRUE
)

# by urban/rural
svyby( 
  ~surviving.children , 
  ~v025 ,
  design = svy ,
  svyquantile ,
  c( 0 , .25 , .5 , .75 , 1 ) ,
  keep.var = F ,
  na.rm = TRUE
)

######################
# subsetting example #
######################

# restrict the svy object to
# 40-49 year old females only
svy.4049 <-
  subset(
    svy ,
    v447a %in% 40:49
  )
# now any of the above commands can be re-run
# using the svy.4049 object
# instead of the svy object
# in order to analyze 40-49 year olds only

# calculate the mean of a linear variable #

# average number of children ever born
# nationwide, restricted to females aged 40-49
svymean( 
  ~v201 , 
  design = svy.4049 ,
  na.rm = TRUE
)

###################
# export examples #
###################

# calculate the distribution of a categorical variable #
# by urban/rural

# store the results into a new object

region.by.urbanrural <-
  svyby( 
    ~v101 , 
    ~v025 ,
    design = svy ,
    svymean ,
    na.rm = TRUE
  )
  # print the results to the screen 
  region.by.urbanrural
  
  # now you have the results saved into a new object of type "svyby"
  class( region.by.urbanrural )
  
  # print only the statistics (coefficients) to the screen 
  coef( region.by.urbanrural )
  
  # print only the standard errors to the screen 
  SE( region.by.urbanrural )
  
  # this object can be coerced (converted) to a data frame.. 
  region.by.urbanrural <- data.frame( region.by.urbanrural )
