
library(dplyr)

library(feather)
# load('data_dictionary.rda') # loads 'data_dictionary'
# write_feather( data_dictionary, "data_dictionary.feather")
data_dictionary = read_feather("data_dictionary.feather") 

system.time(read_feather("data_dictionary.feather"))
system.time(load("data_dictionary.rda"))
system.time(read_feather("data_dictionary.feather") %>% filter(country %in% "Rwanda"))

# devtools::install_github("hannesmuehleisen/MonetDBLite")
library(MonetDBLite)

library(DBI)
dbdir <- paste0(getwd(), "/data_dictionary")
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)
dbWriteTable(con, "data_dictionary", data_dictionary, overwrite = TRUE)
dbGetQuery(con, "SELECT country, count(country) FROM data_dictionary group by country")

dbDisconnect(con, shutdown=TRUE)

ms <- MonetDB.R::src_monetdb(embedded = dbdir )
data_dictionary_mdb <- tbl(ms, "data_dictionary")
system.time(data_dictionary_mdb)
system.time( data_dictionary_mdb  %>% count(country) )

data_dictionary_mdb %>%  count(country)
data_dictionary %>%  count(country)





### monetdblite xample, test
library(DBI)
dbdir <- tempdir()
# dbdir <- paste0(getwd(), "/data_dictionary")
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

# directly copy a data.frame object to a table within the database
dbWriteTable(con, "mtcars", mtcars)

# calculate the average miles per gallon, grouped by number of cylinders
dbGetQuery(con, "SELECT cyl, AVG(mpg) FROM mtcars GROUP BY cyl" )

# calculate the number of records in the _mtcars_ table
dbGetQuery(con, "SELECT COUNT(*) FROM mtcars" )
dbDisconnect(con, shutdown=TRUE)

# embedded connection
ms <- MonetDB.R::src_monetdb(embedded = dbdir)
mt <- tbl(ms, "mtcars")
mt %>% filter(cyl == 8) %>% summarise(max(mpg))


