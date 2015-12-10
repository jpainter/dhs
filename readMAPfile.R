# convert document to table

# library(dplyr)
# library(readr)
# library(tidyr) # for fill function

# mapFile = "C:\\Users\\bzp3\\Downloads\\ZWIR62FL\\ZWIR62FL.MAP"
# 
# mapFile = "sample/GNKR61FL.MAP"

# get encoding for ascii characters
# guess_encoding(mapFile)

# NB: open file in notepad; use font size 6 to put on full page; count lines to skip; count col widths


# COMPILE ABOVE INTO A FUNCTION
parseMAPfile = function(mapFile){
  
  require(dplyr)
  require(readr)
  require(tidyr) # for fill function
  
  map = read_fwf(mapFile, 
                  fwf_widths(c(10, 62, 5, 5, 5, 5,5,5,5,5),
                             col_names = c("var", "label", "Start", "Len", "Type", "Item Type", "Occ", "Dec", "Dec Char", "Zero Fill")
                  ),
                  locale = locale(encoding = "ISO-8859-1"),
                  skip = 18) %>%
    mutate(
      level = ifelse(is.na(var), "level", "name")
    ) %>% 
    fill(var) %>%
    separate(label, c("value", "label"), sep = " ", remove=TRUE, extra = "merge") %>%
    mutate(
      value = ifelse(level == "name", paste(value, ifelse(is.na(label), "", label)), value),
      label = ifelse(level == "name", NA, label)
    ) %>%
    select(var, value, label, Len, Type)
  
  return(map)
}

# test:  map = parseMAPfile(mapFile); View(map); map %>% count(Type)


# map = parseMAPfile("sample/GNKR61FL.MAP"); View(map); map %>% count(Type)

# maphr = parseMAPfile("sample/GNHR61FL.MAP"); View(maphr); maphr %>% count(Type)
# mapkr = parseMAPfile("sample/GNKR61FL.MAP"); View(mapkr); mapkr %>% count(Type)
# mappr = parseMAPfile("sample/GNPR61FL.MAP"); View(mappr); mappr %>% count(Type)
# 
# prefixhr = maphr %>% mutate( prefix = substring(var, 1, 2)) %>% count(prefix)
# prefixkr = mapkr %>% mutate( prefix = substring(var, 1, 2)) %>% count(prefix)
# prefixpr = mappr %>% mutate( prefix = substring(var, 1, 2)) %>% count(prefix)
# 
# bind_rows(prefixhr %>% mutate(map = 'hr'), prefixkr %>% mutate(map = 'kr'), prefixpr %>% mutate(map = 'pr')) %>%
#   spread(map, n) %>% as.data.frame()
