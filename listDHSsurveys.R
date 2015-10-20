# DHS datasets

library(XML)
theurl <- "http://www.dhsprogram.com/data/available-datasets.cfm"
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
n.rows  # number of tables
tables[1]

# compile tables into one data frame
surveys = data.frame(NA)
for (i in 1:length(n.rows)){
  if (i==1){surveys = as.data.frame(tables[i])} else
  {
    surveys = rbind( surveys, as.data.frame(tables[i]))
  }
}

# fix col names
library(dplyr)
surveys = surveys %>% rename(
  Survey = NULL.Survey,
  Type = NULL.Type,
  Phase = NULL.Phase,
  Recode = NULL.Recode,
  Survey.Datasets =  NULL.Survey.Datasets,
  GPS.Datasets = NULL.GPS.Datasets,
  HIV.Other.Biomarkers.Datasets = NULL.HIV.Other.Biomarkers.Datasets.,
  SPA.Datasets = NULL.SPA.Datasets
)

save(surveys, file = 'surveys.rda')

View(surveys)
