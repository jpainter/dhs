
library(dplyr)

library(feather)
# load('data_dictionary.rda') # loads 'data_dictionary'
# write_feather( data_dictionary, "data_dictionary.feather")
data_dictionary = read_feather("data_dictionary.feather") 
system.time(read_feather("data_dictionary.feather"))
system.time(read_feather("data_dictionary.feather") %>% filter(country %in% "Rwanda"))

library(MonetDBLite)

library(DBI)
con <- dbConnect(MonetDB.R::MonetDB(), embedded = getwd())
dbWriteTable(con, "data_dictionary", data_dictionary)

dbdir <- tempdir()
ms <- MonetDB.R::src_monetdb(embedded = dbdir )
data_dictionary_mdb <- tbl(ms, "data_dictionary")
system.time(data_dictionary_mdb)
system.time( data_dictionary_mdb %>% filter(country %in% "Rwanda") %>% count(country) )

data_dictionary_mdb %>%  count(country)
data_dictionary %>%  count(country)
