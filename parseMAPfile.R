# convert .MAP document to table and parse out the name of variable, the label, 
# and the number and value of reponses


# test files
# mapFile = "Tanzania/Standard DHS 2010/Children's Recode.MAP"
# mapFile = "Benin/Standard DHS 1996/Couples' Recode.MAP"
# mapFile = "Benin/Standard DHS 2011-12/Household Member Recode.MAP"


parseMAPfile = function(mapFile){
  
  require(dplyr)
  require(readr)
  require(tidyr) # for fill function
  
  # lines_to_skip
  x = read_lines( mapFile, locale = locale(encoding = "ISO-8859-1" ) )

  # find first line with "Item Name"
  line_begins_item = which( grepl( "Item Name", x, fixed = TRUE))
  
  # remove those lines, plus one to get line of dashes
  if ( length(line_begins_item) == 0 )  return()
  
  y = x[-(1:(line_begins_item[1]-1 ))]
  
  # change header so that there are no spaces in col names
  # eg. "Item Name              Item Label                                     Start  Len Type Type  Occ  Dec Char Fill" to
  # "Item_Name              Item_Label                                     Start  Len Type Type  Occ  Dec Char Fill
  y[1] = gsub("Item ", "Item_", y[1], fixed = TRUE)
  
  y = y[-2]  # remove line with dashes under header
  
  #remove form feed (\f)
  y = gsub("\f", "-f", y)
  
  # remove (m) and (na) and (record type) from values
  y = gsub( "\\(m\\)|\\(na\\)", "", y ) 
  
  # remove line with  (record type) 
  y = y[ -which( grepl( "\\(record type\\)", y ) )]
  
  # find other lines with dashes--they will be paired headers, about 3 lines apart
  dash = which( grepl( "^-", y))
  
  # how many ocurences
  length(dash)
  

  # create matrix of paired lines with dashes; want to exclude all lines in between
  if ( !(length(dash) %% 2) == 0 ){ y = y[-dash[length(dash)]] } # remove single occurence of dash
 
    start = dash[ seq(1, length(dash), 2)]
    stop = dash[ seq(2, length(dash), 2)]
    npairs = length(start )
    pairs = cbind( start, stop )
  
  exclude = NA
    for (i in 1:nrow(pairs)){
      exclude = c(exclude, pairs[i, 'start']:pairs[i, 'stop'])
    }
  exclude = exclude[!is.na(exclude)]
  include = setdiff( 1:length(y), exclude)
  z = y[include]
  
  n = length(z)
  map = data_frame(
    Item_Name = character(n),
    Item_Label = character(n),
    
    value = character(n),
    label = character(n)
  ) 
  
  # set all to NA
  map[, ] = NA
  
  cat( " there are ", length(z), " rows to read and parse")
  
  for (row in 2:length(z)){
    
    if (row %% 100 == 0 ) { print( row ) }
    # Sys.sleep(1)
    
    # assume line has all columns
    writeLines(z[ c(1, row) ],"temp.map")

    m = read_table( "temp.map", 
                    locale = locale(encoding = "ISO-8859-1"), 
                    na = c("", "NA"), 
                    col_names = T )
    
    # test if all columns are empty, then skip
    empty = function(x) {
      is.na(x) | is.null(x) | trimws(x) == ""
    }
    
    if ( ncol(m) == sum( sapply( m, empty) ) ){ next() }
      
    
    # if first column is blank, then row is a value list
    if ( is.na(m$Item_Name) ){ 
      
      writeLines( z[row], "temp.map")
      m = read_table( "temp.map", 
                      locale = locale(encoding = "ISO-8859-1"), 
                      na = c("", "NA"), 
                      col_names = F )
      m$label = ""
      if (ncol(m) >1) m$label = paste( m[2:ncol(m)], collapse = " ")
      m$value = m$X1
      m = m[, c("value", "label")]
      
    } else {
      
      # if blanck columns before Start, then combine
      colnum.start = which(grepl("Start", colnames(m), fixed = TRUE))
      colnum.label = which(grepl("Item_Label", colnames(m), fixed = TRUE))  
      # be sure Start is character
      m$Start = as.character(m$Start)
      
      if ( colnum.start - colnum.label > 1 ){
        
      m$Item_Label = paste( m[colnum.label:(colnum.start-1)], collapse = " ")
      
    }
    
    # get rid of columns with no name
    keep_cols = c('Item_Name', 'Item_Label', 'value', 'label')
    m = m[ , colnames(m) %in% keep_cols]
    
    }
    
    map[row, colnames(map) %in% colnames(m)] = m

  }
  
  # keep important cols
  map = map %>% select( Item_Name, Item_Label, value, label) 
  
  # remove rows with all empty data
  map = map[ rowSums(is.na(map)) != ncol(map) , ]

  # fill in values from value above
  map = map %>% fill(Item_Name) 
  map = map %>% fill(Item_Label) 

  
  # Add row number to be able to re-sort in original order after adding in fixed up rows
  map = map %>% mutate( row = row_number())
  
  
  # remove rows with no value or num if there are other rows with same Name, that has a value or num
  map = map %>% group_by( Item_Name) %>%
    filter( ( n()>1 & !is.na(value) & !is.na(label) ) | ( n() == 1 ) ) 
  

  return(map)
}

# test:  map = parseMAPfile(mapFile); View(map); map %>% count(Name)


