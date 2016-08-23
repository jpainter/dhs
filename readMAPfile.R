# convert .MAP document to table and parse out the name of variable, the label, 
# and the number and value of reponses


# test file
# mapFile = "Tanzania/Standard DHS 2010/Children's Recode.MAP"
# mapFile = "Benin/Standard DHS 1996/Couples' Recode.MAP"
mapFile = "Benin/Standard DHS 2011-12/Household Member Recode.MAP"


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
  
  
  df = NA
  for (row in 2:length(z)){
    
    # assume line has all columns
    writeLines(z[ c(1, row) ],"temp.map")
    
    m = read_table( "temp.map", locale = locale(encoding = "ISO-8859-1"), col_names = T )
    
    # if first column is blank, then row is a value list
    if( m$Item_Name == "" ){ 
      writeLines(z[ c(row) ],"temp.map")
      m = read_table( "temp.map", locale = locale(encoding = "ISO-8859-1"), col_names = F )
      m$label = ""
      if (ncol(m) >1) m$label = paste( m[2:ncol(m)], collapse = " ")
      m$value = m$X1
      m = m[, c("value", "label")]
      
    } else {
      
      # if blanck columns before Start, then combine
      colnum.start = which(grepl("Start", colnames(m), fixed = TRUE))
      colnum.label = which(grepl("Item_Label", colnames(m), fixed = TRUE))  
      if ( colnum.start - colnum.label > 1 ){
        
        m$Item_Label = paste( m[colnum.label:(colnum.start-1)], collapse = " ")
      
    }
    
    # get rid of columns with no name
    m = m[ , !colnames(m) %in% ""]
    
    # be sure Start is character
    m$Start = as.character(m$Start);
    }

    if (row == 2){ df = m;  df$Start = as.character(df$Start); next()}

    df = bind_rows( df, m)
   
  }
  df = df %>% select( Item_Name, Item_Label, value, label)
  
  parseLine = function(z_row){
     
    z =c(z[1], z_row)
    
    writeLines(z,"temp.map")
    
    m = read_table( "temp.map", locale = locale(encoding = "ISO-8859-1"), col_names = T )
    
    # if first column is blank, then row is a value list
    if( m$Item_Name == "" ){ 
      writeLines( z_row,"temp.map")
      m = read_table( "temp.map", locale = locale(encoding = "ISO-8859-1"), col_names = F )
      m$label = ""
      if (ncol(m) >1) m$label = paste( m[2:ncol(m)], collapse = " ")
      m$value = m$X1
      m = m[, c("value", "label")]
      
    } else {
      
      # if blanck columns before Start, then combine
      colnum.start = which(grepl("Start", colnames(m), fixed = TRUE))
      colnum.label = which(grepl("Item_Label", colnames(m), fixed = TRUE))  
      if ( colnum.start - colnum.label > 1 ){
        
        m$Item_Label = paste( m[colnum.label:(colnum.start-1)], collapse = " ")
      
    }
    
    # get rid of columns with no name
    m = m[ , !colnames(m) %in% ""]
    
    # be sure Start is character
    m$Start = as.character(m$Start);
    }
    
    return(m)
  }
  
  df =  lapply(z[2:length(z)],  parseLine)
  df = bind_rows( df )
  df = df %>% select( Item_Name, Item_Label, value, label)
  
  
  # library(data.table)
  # zy = paste(y, collapse =  "\n")
  # writeLines(zy,"temp.map")
  # m = fread( "temp.map")
  
  map = read_table(  "temp.map", #mapFile,
                 locale = locale(encoding = "ISO-8859-1")
                 ,
                 col_names = c('Name', 'id', 'Label', 'Start', 'Len', 'dataType', 'itemType', 'Occ', 'Dec', 'decChar', 'zeroFill'),
                 col_types = cols(
                   Name = col_character() ,
                   id = col_skip() ,
                   Label = col_character() ,
                   Start = col_character(),
                   Len = col_skip() ,
                   dataType = col_skip() ,
                   itemType = col_skip() ,
                   Occ = col_skip() ,
                   Dec = col_skip() ,
                   decChar = col_character() ,
                   zeroFill = col_character()
                 )
                 ,
                 skip = 0 )
  
   map = read_table(  "temp.map", #mapFile,
                 locale = locale(encoding = "ISO-8859-1"),
                 col_names = TRUE,
                 skip = 0)
  
  # convert empty to NA, then fill in values from value above
  map$Name = ifelse( map$Name == "", NA, map$Name) 
  map = map %>% fill(Name) 
  
  # remove variable names in parenthes
  map = map[ !grepl("(", map$Name, fixed = TRUE ), ]
  
  # remove (m) and (na) from values
  map = map %>% mutate( Label = gsub( "\\(m\\)|\\(na\\)", "", Label ) )
  
  # Add row number to be able to re-sort in original order after adding in fixed up rows
  map = map %>% mutate( row = row_number())
  
  # find missing labels that were incorrectly parsed into last two columns 
  incorrect = with(map, which( (!is.na(decChar) & !(decChar %in% c('No', 'Yes')) ) | 
                              ( !is.na(decChar) & !(zeroFill %in% c('No', 'Yes')) ) 
                            ))
  
  # pull out values in wromg place and reorder in fixed df
  map_fix = map[ incorrect,]  %>%
    select( -Label) %>%
    mutate( Label = paste( decChar, zeroFill )  ) %>%
    select( -decChar, -zeroFill) 
  # View(map_fix)
  
  # in original df, set messed up values to NA
  map[incorrect, c("decChar", "zeroFill")] = NA
  
  # add fixed rows back 
  map2 = map %>% bind_rows( map_fix) %>% arrange( row )
  
  # create column for variable description
  map2 = map2 %>% mutate( description  = ifelse( !is.na(decChar), Label, NA) )
  map2 = map2 %>% fill(description)

 
  # View(map2)
  
  # parse label into number and value
  map2 = suppressWarnings(
    separate( map2, Label, sep = " ", into = c("num", "value"), extra = "merge", remove = FALSE) 
  )
  
  # if 'num' is not a number, revert to original label
  map2[ !grepl("^[0-9]", map2$num ) , "num"] = NA # if does not start with a number, set to NA
  map2[ is.na(map2$num ), "value"] = NA 
  
  # add in missing num field
  options(warn = -1) # turn off warnings
  map2 = map2 %>% mutate(
    num = ifelse(
      row == lag(row) & !is.numeric(lag(num)),
      as.integer(lag(num)) + 1,
      num 
    ) ,
    value = ifelse(
      row == lag(row) & !is.numeric(lag(num)),
      Label,
      value 
    ) 
  )
  options(warn = 0) # turn warnings back on 
  
  # remove rows with no value or num if there are other rows with same Name, that has a value or num
  map2 = map2 %>% group_by( Name) %>%
    filter( ( n()>1 & !is.na(num) & !is.na(value) ) | ( n() == 1 ) ) %>%
    select( Name, num, value, description, row)
  
  # remove rows with duplicate info in label and description
  # map2 = map2[!(map2$Label == map2$description), c( "Name", "num", "value", "description", "row")]
 
  # View( map2)
  
  
  return(map2)
}

# test:  map = parseMAPfile(mapFile); View(map); map %>% count(Name)


