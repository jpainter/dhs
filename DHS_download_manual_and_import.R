# analyze survey data for free (http://asdfree.com) with the r language ####
# demographic and health surveys
# all available years
# all approved countries  


####################################################################################
# download every file from every year of the Demographic and Health Surveys with R #
# then save every file as an R data frame (.rda) so future analyses can be rapid   #
####################################################################################

# PREAMBLE #####

# if you have never used the r language before,
# watch this two minute video i made outlining
# how to run this script from start to finish
# http://www.screenr.com/Zpd8

# anthony joseph damico
# ajdamico@gmail.com

# if you use this script for a project, please send me a note
# it's always nice to hear about how people are using this stuff

# for further reading on cross-package comparisons, see:
# http://journal.r-project.org/archive/2009-2/RJournal_2009-2_Damico.pdf

# # # # # # # # # # # # # #
# important user warning! #
# # # # # # # # # # # # # #

# you *must* visit this dhsprogram.com website and explain your research
# before receiving a username and password.

# this is to protect both yourself and the respondents of the study.  register here:
# http://dhsprogram.com/data/Access-Instructions.cfm

# once you have registered, place your username, password, and the name of your project in the script below.
# this script will not run until valid values are included in the lines below.
# oh and don't forget to uncomment these lines by removing the `#`

# this massive ftp download automation script will not work without the above lines filled in.
# if the three lines above are not filled in with the details you provided at registration, 
# the script is going to break.  to repeat.  register to access dhs data.


# # # # # # # # # # # # # # # # # 
# end of important user warning #
# # # # # # # # # # # # # # # # #


# set your working directory.
# all DHS data files will be stored here
# after downloading.
# use forward slashes instead of back slashes

# uncomment this line by removing the `#` at the front..
# setwd( "C:/My Directory/DHS/" )
# ..in order to set your current working directory


# remove the # in order to run this install.packages line only once
# install.packages( c( "XML" , "httr" ) )

# no need to edit anything below this line #


# # # # # # # # #
# program start #
# # # # # # # # #


library(foreign) 	# load foreign package (converts data files into R)
library(httr)		# load httr package (downloads files from the web, with SSL and cookies)
library(XML)		# load XML (parses through html code to extract links)

# Setup libraries ####

library(tidyverse)
library(countrycode)
library(foreign)
library(rgdal)
library(stringr)

# find zip files and unzip
files = list.files("dhs_download")
is.zip.file = grepl(".zip", files)
zip_files = files[is.zip.file]

# SET option to redownload if dataset already exists (FALSE will only download new data) ####
redownload = FALSE  # TRUE will result in overwrite

## loop through each country folder #####
  
for ( j in seq_along( zip_files) ){
  
  # final folder to save it
  country_iso2 <- substr(zip_files[j],1,2)
  country = countrycode(country_iso2, "iso2c", "country.name")
  dir.create( paste0("dhs_download/", country) , showWarnings = FALSE )
  
  #create folder for survey within country 
  underscores = gregexpr( "_", zip_files[j])[[1]]
  survey = substr(zip_files[j], underscores[1]+1, underscores[3]-1 )
  survey_folder = paste0("dhs_download/", country, "/", survey)
  dir.create( survey_folder , showWarnings = FALSE )
  
  unzip( paste0("dhs_download/", zip_files[j] ), exdir = survey_folder)
  
  survey_file_folders = list.dirs( survey_folder, recursive = FALSE, full.names = TRUE)
  
  # get list of folders within survey
  for ( k in seq_along( survey_file_folders )){
    
    # 

   survey_files = list.files( survey_file_folders[k], recursive = FALSE, full.names = TRUE)
   
  # clear up RAM
  # gc()
      
        # if RDA file exists, go to next
        if ( exists( paste0( survey_file_folders[k] , ".rds" ) ) & redownload  ){ next } 
        
   
   # and now, if there's a stata file, import it!
        if ( any( st <- grepl( "\\.dta$" , tolower( survey_files ) ) ) ){
          
          # load the current stata file into working memory
          x <- read.dta( survey_files[ which( st ) ] , convert.factors = FALSE )
          
          # save the file on the local disk, within the appropriate country-survey filepath
          saveRDS( x , file = paste0( survey_file_folders[k] , ".rds" ) )
          
        }
        
        ####  import any MAP files (JP)
        if ( any( mp <- grepl( "\\.map$" , tolower( survey_files ) ) ) ){
    
          # load function to read MAP file
          source("parseMAPfile.R")
          
          parseMAP = FALSE
          
          for (zfile in survey_files[which(mp)]){
            
            mapfilename = tail( strsplit(zfile, "/")[[1]], 1 )
            
            # if more than one map file, then add letters to end (e.g. _A, _B)
            if ( length(survey_files[which(mp)])>1 ){ 
              fileletter = paste0("_", LETTERS[ which( zfile == survey_files[which(mp)] ) ]) 
            } else { fileletter = "" }
            
            file.copy( zfile, paste0( survey_file_folders[k] , fileletter, ".MAP" ), overwrite = redownload)
            
            # load the current stata file into working memory
            # NB: function throws lots of warning messages while parsing.  does not affect result.
            
            # process map file
            if ( parseMAP ){
              
              # TURN OFF WARNINGS
              oldw <- getOption("warn")
              options(warn = -1)
            
              map = parseMAPfile( paste0( survey_file_folders[k] , fileletter, ".MAP" )  )
            
              # TURN WARNINGS BACK ON
              options(warn = oldw)
              
              # save the file on the local disk, within the appropriate country-survey filepath
              saveRDS( map , file = paste0( survey_file_folders[k] , fileletter , ".map.rds" ) )
            }
            
          }
          
        }
   
      } 
  # Once done, , remove  all subdirectories except one with geo
  geo_folder = grepl( "ge", substr( survey_file_folders , nchar(survey_file_folders)-5, nchar(survey_file_folders)-4) )
  unlink( survey_file_folders[!geo_folder] , recursive=TRUE)
} 



# FINAL MESSAGES ####


# print a reminder: set the directory you just saved everything to as read-only!
message( paste0( "all done. you should set the folder " , getwd() , " read-only so you don't accidentally alter these tables." ) )

# for more details on how to work with data in r
# check out my two minute tutorial video site
# http://www.twotorials.com/

# dear everyone: please contribute your script.
# have you written syntax that precisely matches an official publication?
message( "if others might benefit, send your code to ajdamico@gmail.com" )
# http://asdfree.com needs more user contributions

# let's play the which one of these things doesn't belong game:
# "only you can prevent forest fires" -smokey bear
# "take a bite out of crime" -mcgruff the crime pooch
# "plz gimme your statistical programming" -anthony damico

# analyze survey data for free (http://asdfree.com) with the r language ####
# demographic and health surveys
# all available years
# all approved countries  


# dear everyone: please contribute your script.
# have you written syntax that precisely matches an official publication?
message( "if others might benefit, send your code to ajdamico@gmail.com" )
# http://asdfree.com needs more user contributions

# let's play the which one of these things doesn't belong game:
# "only you can prevent forest fires" -smokey bear
# "take a bite out of crime" -mcgruff the crime pooch
# "plz gimme your statistical programming" -anthony damico
