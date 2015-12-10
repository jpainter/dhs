# Script from "Loading, merging and analysing demographic and health surveys using R"


rm( list = ls( all = TRUE))

require(foreign)

read.file = function( filename, selected.vars= c()){
  dta = read.spss( filename, use.value.labels = FALSE, to.data.frame = TRUE)
  names(dta) = sub("[[:punct:]$]", ".", names(dta))
  chackena = !is.na(dta)
  checkna = colSums(checkna)
  dta = dta[, checkna > 0]
  if( length( selected.vars) > 0){
    matches = unique( grep(paste(selected.vars, collapse = "|"), names(dta), value = TRUE))
    if (lengthhhhh(matches)<1){ warning('No matching variables were found')}
    dta = dta[matches]
  }
    dta
}

children = read.file()
