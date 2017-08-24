# convert .MAP document to table and parse out the name of variable, the label, 
# and the number and value of reponses


# test files
# mapFile = "Tanzania/Standard DHS 2010/Children's Recode.MAP"
# mapFile = "Benin/Standard DHS 1996/Couples' Recode.MAP"
# mapFile = "Benin/Standard DHS 2011-12/Household Member Recode.MAP"


parseMAPfile = function(mapFile, NameThenLabel = TRUE){
  
  require(dplyr)
  require(readr)
  require(tidyr) # for fill function
  require(data.table)
  
  x = read_lines( mapFile, locale = locale(encoding = "ISO-8859-1" ) )
  # x = read_lines( mapFile, locale = locale(encoding = "UTF-8" ) )
  # x = read_lines( mapFile)
 
  # x = gsub("<..>", "", x, perl = T) # remove bad characters like <e9>
  
  # View(as.data.frame(x))
  
  # remove header lines ####
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
  # length(dash)
  

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
  #   ####
  
  z = y[include] # text, without header lines, to parse
  
  n = length(z)
 
  map = list()
  
  cat( " there are ", length(z), " rows to read and parse")
  
  # pp <- progress_estimated(length(z))

  # for (row in 2:length(z)){
    
    # if (row %% 100 == 0 ) { print( row ) }
    # Sys.sleep(1)
    # pp$pause(0.1)$tick()$print()
    
    # ATTN
    # need to remove bad characters like single backlash #####
    # from mali, childrens, row = 5370,
    # "Item_Name              Item_Label                                     Start  Len Type Type  Occ  Dec Char Fill"  
    # "S_110K                 Improved stove (\"Foyer amélioré \")              1397    1    N    I    1    0   No   No"
    # converted to "S 110K                 Improved stove   Foyer am lior                  1397    1    N    I    1    0   No   No"
    
    # zz = gsub("\"", " ", z[ c(1, row) ], fixed = TRUE) 
    
    zz = gsub("\"", " ", z[], fixed = TRUE) 
    
    zzz = gsub("\\(", "", zz) 
    zzzz = gsub("\\)", "", zzz) 
    
    writeLines( zzzz, "temp.map")  # all the text
    
    cols = c('Item_Name', 'Item_Label', 'Start', 'Len', 'Type', 'Type1' , 'Occ' , 'Dec', 'Char', 'Fill')
    # col_pos = sapply(cols, zzzz)
    
    # update oct 2016. #####
    # Problem: reading with white space as deliminater causes too many cols because labels have many words
    # Identify column positions and read as fixed with table
    
    writeLines( zzzz[1:2], "temp.map1") # write-read first lines to get column positions
    col_positions = fwf_empty("temp.map1", skip = 1)
    
    # Inspect text: for small number of surveys( Benin 2006, DRC 2007, MADA 2008, Zam 2007)
    # the order of document is reversed: Item_Label, Item_Name
    # For these surveys, need to alter the column positions...
    # View(zzzz[1:2])
    
    col_start = unlist( col_positions[1] )
    col_end = unlist( col_positions[2] )
    
    characters = unlist( strsplit(zzzz[2], "") )
    first.numeric = which( !is.na( suppressWarnings( as.numeric(characters)) ))[1] - 1
    
    if (NameThenLabel){ 
      
      # consolidate col positions between 1st column and 1st numeric column. 
      start.positions2 = which( col_start>=first.numeric )
      end.positions2 = c(start.positions2[1]-1, start.positions2 )
      
      col_start2 = c( col_start[1:2], col_start[ start.positions2 ] ) 
      col_start2 = unique(col_start2[ !is.na( col_start2)])
      col_end2 = c( col_end[1], col_end[ end.positions2 ] )
      col_end2 = unique( col_end2)
      col_end2[2] = col_start2[3] - 1  # use max space for Item_Label
      
      col_positions2 = fwf_positions(col_start2, col_end2, col_names = cols)
      
    } else {
      
      # consolidate col positions between 1st column and 1st numeric column. 
      start.positions2 = which( col_start >= first.numeric )
      start.positions2 = c(1,  min(start.positions2) - 1, start.positions2)
      end.positions2 = c( which( col_end == col_start[start.positions3[2]]-1 ),
                          which( col_end >= first.numeric - 1 ),
                          max(col_end)
      )
      
      col_start2 = unname( col_start[ start.positions2 ] ) 
      col_end2 = c( col_end[ end.positions2 ], col_start2[3] - 1 ) 
      col_end2 = unname( col_end2[ order(col_end2)] )
      
      col_positions2 = fwf_positions(col_start2, col_end2, col_names = cols)
      
    }
    # #####  
    m = suppressMessages(
      read_fwf("temp.map", 
               col_positions = col_positions2, 
               col_types = cols_only( 
                 Item_Name = col_character(),
                 Item_Label= col_character()
                 ) ,
               skip = 1 , 
               # locale = locale(encoding = "ISO-8859-1"), 
               na = c("", "NA")
      )
    )
                 
 
    # if first column is blank, then row is a value list
    if (NameThenLabel){
      mx =  suppressWarnings(
      separate(m, Item_Label, c("value", "label"), " ", 
                                remove = FALSE, extra = "merge")
      ) %>%
      filter(
        !( Item_Label %in% "Not applicable")
      ) %>%
      mutate(
        value = ifelse( is.na(Item_Name), value, NA),
        label = ifelse( is.na(Item_Name), label, NA),
        Item_Label = ifelse( is.na(Item_Name), NA, Item_Label ),
        row = row_number() # Add row number to be able to re-sort in original order after adding in fixed up rows
      ) 
    } else {
      mx =  suppressWarnings(
      separate(m, Item_Name, c("value", "label"), " ", 
                                remove = FALSE, extra = "merge")
      ) %>%
      filter(
        !( Item_Name %in% "NotAppl")
      ) %>%
      mutate(
        value = ifelse( is.na(Item_Label), value, NA),
        label = ifelse( is.na(Item_Label), label, NA),
        Item_Name = ifelse( is.na(Item_Label), NA, Item_Name ),
        row = row_number() # Add row number to be able to re-sort in original order after adding in fixed up rows
      )  %>%
        rename( item_label = Item_Name, item_name = Item_Label) %>%
        rename( Item_Label = item_label, Item_Name = item_name) %>%
        # clean up names with trailing bits
        rowwise() %>%
        mutate(
          Item_Name =  unlist( strsplit( Item_Name, " " ) )[1]
        )
    }
    
  

  # fill in values from value above
  mx = mx %>% fill(Item_Name) 
  mx = mx %>% fill(Item_Label) 

 
  # remove rows with no value or num if there are other rows with same Name, that has a value or num
  mx = mx %>% group_by( Item_Name) %>%
    filter( ( n()>1 & !is.na(value) & !is.na(label) ) | ( n() == 1 ) ) 
  

  return(mx)
}

# test:  map = parseMAPfile(mapFile); View(map); map %>% count(Name)


