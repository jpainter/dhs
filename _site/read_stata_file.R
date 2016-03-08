# read dhs stata file

library(foreign) 

z = "../DHS/sample/ZMHR61FL.DTA"

x <- read.dta( z , convert.factors = FALSE )
