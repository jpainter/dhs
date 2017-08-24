# Malaria_territory_widget

suppressMessages( library(leaflet) )
suppressMessages( library(tidyverse) )
suppressMessages( library( scales ) )
suppressMessages( library(countrycode) )
suppressMessages( library(sp) )
suppressMessages( library(shiny) )
suppressMessages(  library(DT) )

# Datasets ####
gadm = "../malaria_atlas/gadm/"
load( paste0( gadm, "adm1.africa") )
load( paste0( gadm, "adm2.africa") )

# load survey_list, variable.doc, cluster, and adm1 created from malaria_atlas_datset_2
load( file = "malaria_atlas.rda" )

survey_list = mutate( survey_list, survey_year = paste(survey, year) )

# list of variables and variable categories
library(readxl)
variable.doc = read_excel("malaria_atlas_widget_definitions.xlsx")

variable_categories = filter(variable.doc, !is.na(Code) ) %>%
     .$Variable_category %>% unique

variables = filter(variable.doc, !is.na(Code) ) %>%
     filter( Display == 1) %>%
     .$Variable %>% unique

# UI functions ####

dhs_map_UI = function(id) {

   # Create a namespace function using the provided id
   ns <- NS(id)

   # shinyApp(

   fillCol(  height = "100%" , #  height = 600,
             flex = c(NA, 1),

             fluidPage(
                  fluidRow(

                       column(3,
                              selectInput( ns("var_cat"), "Category",
                                           # list of numeric variables
                                           choices = variable_categories,
                                           selected = 'Demographic' )
                       ),

                       column(3,
                              selectInput( ns("var"), "Variable/Indicator",
                                           # list of numeric variables
                                           choices = variables,
                                           selected = 'pct.RDT' )
                       ),

                       column(4,

                              HTML( paste( "<b>", ("Description"), "</b>" )  ),

                              textOutput( ns('var_def'))
                       ),

                       column(2,
                              actionButton( ns('maplink'), "Download Map")
                       )
                  )
             ),

             leafletOutput( ns('map'), height = "100%" )
   )
     # ))
}


dhs_dictionary_UI = function( id ){

     ns <- NS(id)

     fillCol(  height = "100%" , #  height = 600,
               flex = c(NA, 1),

          dataTableOutput( ns("variables" ) )
          )


}

# Server functions ####

dhs_map_Output = function(  input, output, session ,
                 dataset = NULL,
                 adm_map = NULL,
                 cluster_dataset = NULL,
                 country = "Tanzania",
                 survey_year = "Standard DHS 2011-12",
                 ncolors = 5,
                 title = "Household Surveys",
                 leaflet_style = "Stamen.TonerLite"
) {

     # update variable list after selecting variable category
     observe({

          variables = filter(variable.doc, !is.na(Code) ) %>%
               filter( Display == 1, Variable_category %in% input$var_cat ) %>%
               arrange( -order ) %>%
               .$Variable %>% unique

          updateSelectInput(session,
                                   inputId = "var",
                                   choices = variables,
                                   selected = variables[1]
          )
          })

     # update variable definition
     observe({

          def = filter(variable.doc, !is.na(Code) ) %>%
               filter( Display == 1,
                       Variable %in% input$var,
                       grepl( 'cluster', Applies_to )
                       ) %>%
               .$Description

          output$var_def = renderText(  paste( " " , def, sep = "") )

     })

   # polygon data
   dpoly <- reactive({


        # use reactive inputs
        .var = input$var
        .country = country()
        .survey_year = survey_year()
        .dataset = dataset()
        .adm_map = adm_map()

        .country_iso3 = countrycode( .country, "country.name", "iso3c" )

        if (!.country %in% "Africa"){

             .surveys = survey_list  %>% filter( iso3 %in% .country_iso3 )

        } else {

             .surveys = survey_list
        }


     # Get data ####
        if (length( .survey_year ) == 1) {

             # find surveys with selected survey(s)
             .surveys = .surveys %>%
                  filter(
                       # era %in% .survey_year |
                            survey_year %in% .survey_year )

             if ( nrow(.surveys) == 0  ) return( NULL )


             data = .dataset %>%
                  filter( !is.na( NAME_1) ) %>% # remove data without geocodes (!! TODO: add in data from survey region names)
                  semi_join( .surveys,
                             by = c("iso3", "survey", "year")
                             ) # filters to selected surveys

             if ( nrow( data ) == 0 ) return( NULL )



        } else if (length( .survey_year ) == 2) {

             # find surveys with selected survey(s)
             .surveys1 = .surveys %>% filter(
                  # era %in% .survey_year[1] |
                       survey_year %in% .survey_year[1] )
             .surveys2 = .surveys %>% filter(
                  # era %in% .survey_year[2] |
                       survey_year %in% .survey_year[2] )

             if ( nrow(.surveys1) == 0 | nrow(.surveys2) == 0  ) return( NULL )

             data1 = .dataset %>%
                  filter( !is.na( admin) ) %>% # remove data without geocodes (!! TODO: add in data from survey region names)
                  semi_join( .surveys1, by = c("iso3", "survey", "year") ) # filters to selected surveys

             data2 = .dataset %>%
                  filter( !is.na( admin) ) %>% # remove data without geocodes (!! TODO: add in data from survey region names)
                  semi_join( .surveys2, by = c("iso3", "survey", "year") ) # filters to selected surveys

             data1.matching2 = data1 %>% semi_join( data2[, c("NAME_0", "NAME_1" )], by = c("NAME_0", "NAME_1" ) )

             data2.matching1 = data2 %>% semi_join( data1[, c("NAME_0", "NAME_1" )], by = c("NAME_0", "NAME_1" )  )

             # data1.matching = data1$NAME_1 %in% data1.matching2
             # data1 = data1[ data1.matching , ] %>% arrange( NAME_0, NAME_1)
             #
             #
             # data2.matching = data2$NAME_1 %in% matching.adm1 & data2$NAME_0 %in% matching.adm0
             # data2 = data1[ data2.matching , ]  %>% arrange( NAME_0, NAME_1)

             # convert numeric columns to matrix for each dataset, and take difference
             numeric_cols = names(data1.matching2)[ sapply(data1.matching2, is.numeric) ]
             numeric_cols = numeric_cols[ !numeric_cols %in% c('x', 'y')]
             non_numeric_cols = names(data1)[ !sapply(data1, is.numeric) ]

             data1.matching2.matrix = as.matrix( data1.matching2[ , numeric_cols] )
             data2.matching1.matrix = as.matrix( data2.matching1[ , numeric_cols] )

             # if ( identical( dim(data1.matrix) , dim(data2.matrix) ) ){
             # matching.adm1 = semi_join(data1.matching2.matrix, data2.matching1.matrix,
             #                           by = c("NAME_0", "NAME_1" ) ) %>% .$NAME_1

             data.diff = data1.matching2.matrix - data2.matching1.matrix
             data.diff.df = as_data_frame( data.diff )
             colnames( data.diff.df ) = numeric_cols
             data = bind_cols( data1.matching2[ , c(non_numeric_cols, 'x', 'y')], data.diff.df )

        } else {
             return(NULL)
        }

     # join with map ####

        countries = unique( data$country )

        in.country = adm_map()@data$iso3 %in% countrycode( countries, "country.name", "iso3c")

        # map = adm1.africa[ in.country , ]
        map = adm_map()[ in.country , ]

        if ( nrow(map) == 0 ) return( NULL )

        name_levels = names(map@data)[ starts_with("NAME" , vars = names(map@data)) ]
        by_name = sapply( name_levels, FUN = function(x)( x )  )

        map@data = left_join( map@data,
                               data ,
                               by = setNames( by_name, by_name )
                               )

        # set variable 'z' to whichever variable is selected and extract as vector
        map@data$z = map@data %>% select_(.var) %>% unlist(use.names = FALSE)

      return(map)
   })

   # cluster data
   dcluster <- reactive({

        # use reactive inputs
        .var = input$var
        .country = country()
        .survey_year = survey_year()


        # find surveys with selected country(s)
        if (!.country %in% "Africa"){

             .country_iso3 = countrycode( .country, "country.name", "iso3c" )
             .surveys = survey_list  %>% filter( iso3 %in% .country_iso3)

        } else {

             .surveys = survey_list
        }

        # find surveys with selected survey(s)
        .surveys = .surveys %>% filter( era %in% .survey_year | survey_year %in% .survey_year )

        if ( nrow(.surveys) == 0  ) return( NULL )

        # Get data
        if (length( .survey_year ) %in% 1:2 ){

             data = cluster_dataset %>%
                  filter( !is.na( latitude ) ) %>% # remove data without geocodes (!! TODO: add in data from survey region names)
                  semi_join( .surveys, by = c("iso3", "survey", "year") ) # filters to selected surveys

             if ( nrow( data ) == 0 ) return( NULL )

        } else {

             return(NULL)
        }

      if ( nrow(data) == 0 ) return( NULL )

      # set variable 'z' to whichever variable is selected and extract as vector
      data$z  = data %>% select_(.var) %>% unlist(use.names = FALSE)

      return(data)
   })

   # color palettes
   pal <- reactive({

        if ( is.numeric( dpoly()@data$z ) ){

           if ( min( dpoly()@data$z, na.rm = T) < 0   ){

              # divergent +/- palette when comparing values from 2 surveys
              colorBin("RdYlGn", domain = dpoly()@data$z,
                       bins = ncolors, na.color = "#bdbdbd",
                       pretty = TRUE)

           } else {

               colorBin("YlGn", domain = dpoly()@data$z,
                    bins = ncolors, na.color = "#bdbdbd",
                    pretty = TRUE)
           }
        } else {  # palatte for character variables

             colorFactor("viridis", domain = dpoly()@data$z,
                         levels = NULL, ordered = FALSE, reverse = FALSE ,
                         na.color = "gray80", alpha = FALSE )

        }
   })

   cpal <- reactive({

        # levels = dplyr::count( dcluster(), input$var) %>% nrow
        # if (levels < 5 ) ncolors = levels

        if ( is.numeric( dcluster()$z ) ){

             if ( min( dcluster()$z, na.rm = T) < 0   ){

                  # divergent +/- palette when comparing values from 2 surveys
                  colorBin("RdYlGn", domain = dcluster()$z,
                           bins = ncolors, na.color = "#bdbdbd",
                           pretty = TRUE)

             } else {

                  colorBin("YlGn", domain = dcluster()$z,
                           bins = ncolors, na.color = "#bdbdbd",
                           pretty = TRUE)
             }
        } else {  # palatte for character variables

             colorFactor("viridis", domain = dcluster()$z,
                         levels = NULL, ordered = FALSE, reverse = FALSE ,
                         na.color = "#808080", alpha = FALSE )

        }

   })

   # popups
   popup_poly <- reactive({

      # use reactive inputs
      .var = input$var
      .val = dpoly()@data$z

      if ( is.null( .val ) ) return( )

      value =   # Conditionally format data:  if numeric--round
           if ( is.numeric( .val ) ){

                round( .val, digits = 3)

           } else { .val }

      paste("<strong>",
            dpoly()@data$NAME_0, ", ", dpoly()@data$NAME_1, ", ", dpoly()@data$NAME_2 , "<br>",
            dpoly()@data$survey, " " , dpoly()@data$year, "<br>",
            .var, " : ", value , "</strong>"
      )
   })

   popup_cluster <- reactive({

        # use reactive inputs
        .var = input$var
        .val = dcluster()$z

        if ( is.null( dcluster()$z ) ) return( NULL )

        value =   # Conditionally format data:  if numeric--round
             if ( is.numeric( .val ) ){

                  round( .val, digits = 3)

             } else { .val }

        paste("<strong>",
              dcluster()$country ,  "<br>",
              dcluster()$survey, " " , dcluster()$year, "<br>",
              "Cluster id:", dcluster()$hv001 ,
              "<br>",
              "Weight:", dcluster()$Survey_Weight,
              "<br>",
              "Households:" , dcluster()$households,
              "<br>",
              "Persons:" , dcluster()$persons,
              "<br>",
              "Children <6:" , dcluster()$children_u6,
              "<br>",
              "Pct. Urban:" , percent( dcluster()$pct.Urban ) ,
              "<br>",
              .var, " : ", value , "</strong>"
        )
   })

   map = reactive({

          req( dpoly() )
          if ( is.null( dpoly() ) ) return()

            leaflet() %>%

               # if ( background.map ) addProviderTiles( "Stamen.TerrainBackground" ) %>%
               # Base groups
               addProviderTiles( leaflet_style,
                                 options = providerTileOptions(opacity = 1),
                                 group = "Default"
               )  %>%
               addProviderTiles("OpenStreetMap.France", group = "OpenStreetMap") %>%
               addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
               # addProviderTiles( "Stamen.TerrainBackground", group = "Terrain" ) %>%
               addProviderTiles( "OpenMapSurfer.Roads", group = "Roads" ) %>%

               # Overlay groups
               addPolygons( data = dpoly() ,
                            fillColor = ~pal()(z),
                            fillOpacity = 0.7,
                            color = "#BDBDC3",
                            weight = 1,
                            popup = popup_poly(),
                            group = "Admin areas",
                            highlight = highlightOptions(
                                 weight = 3,
                                 color = "#666",
                                 dashArray = ""
                                 )
               ) %>%

               # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%

               addLabelOnlyMarkers( data = dpoly(), ~x, ~y,
                                   label =  ~as.character(

                                        # Conditionally format data:  if numeric--round
                                        if ( is.numeric( z ) ){

                                             round( z, digits = 3)

                                        } else { z }
                                        ),
                                   labelOptions = labelOptions(noHide = T,
                                                               offset = c(0,0),
                                                               textOnly = T),
                                   group = "Data Labels"
               )  %>%

               addLabelOnlyMarkers( data = dpoly()@data, ~x, ~y,
                                      label =  ~paste(NAME_1 ,
                                                      if("NAME_2" %in% colnames(dpoly()@data)){NAME_2}
                                                      ),
                                      labelOptions = labelOptions(noHide = T,
                                                                  offset = c(12,12), opacity = .75 ,
                                                                  textOnly = T),
                                      group = "Admin Labels"
                 )  %>%

               addCircles(
                  data = dcluster(),
                  popup = popup_cluster(),
                  lng = ~longitude, lat = ~latitude,
                  radius = ~Survey_Weight*6e3,
                  stroke = TRUE,
                  weight = 1,
                  color = ~cpal()(z),
                  opacity = .7 ,
                  fill = TRUE ,
                  fillColor =  ~cpal()(z),
                  fillOpacity = .7 ,
                  group = "Sampling clusters"
               )  %>%

               addCircles(
                  data = dcluster() %>% filter( round(pct.Urban) == 1 ),  # highlight urban clusters
                  lng = ~longitude, lat = ~latitude,
                  radius = ~Survey_Weight*6e3,
                  stroke = TRUE,
                  weight = 1,
                  color = "black",
                  opacity = 0.4 ,
                  fill = FALSE,
                  group = "Sampling clusters"
               )  %>%

               addCircles(
                      data = dcluster() %>% filter( round(pct.Urban) ==  0 ),  # highlight rural clusters
                      lng = ~longitude, lat = ~latitude,
                      radius = ~Survey_Weight*6e3,
                      stroke = TRUE,
                      weight = 1,
                      color = "brown",
                      opacity = 0.4 ,
                      fill = FALSE,
                      group = "Sampling clusters"
                 )  %>%

               # Layers control
               addLayersControl(
                  overlayGroups = c("Admin areas", "Sampling clusters", "Data Labels", "Admin Labels"),
                  # baseGroups = c("Default", "OpenStreetMap", "Toner Lite", "Roads"),
                  options = layersControlOptions(collapsed = FALSE),
                  position = "topleft"
               )  %>%

               # default layers to show
               hideGroup("Sampling clusters")


         })

   output$map = renderLeaflet({ map() })

   # trigger admin areas on refresh/change country
   observeEvent( dpoly() , {
        proxy <- leafletProxy("map") %>%
                         showGroup( "Admin areas")  %>%
                         addLegend( pal = pal(),
                                      values = dpoly()@data$z,
                                      opacity = 0.7,
                                      position = 'bottomright',
                                      title = paste(input$var)
        )


   })


   # trigger legend when group is displayed
   observeEvent( input$map_groups, {

        proxy <- leafletProxy("map") %>% clearControls()

        if ("Sampling clusters" %in% input$map_groups){
             proxy <- proxy %>% addLegend(pal = cpal(),
                                  values = dcluster()$z,
                                  opacity = .7,
                                  position = 'bottomleft',
                                  title = paste("Clusters:", input$var)
        )
        }

        if ("Admin areas" %in% input$map_groups){
             proxy <- proxy %>% addLegend( pal = pal(),
                        values = dpoly()@data$z,
                        opacity = 0.7,
                        position = 'bottomright',
                        title = paste("Regions:", input$var)
             )
        }
   }
   )


   # download map function ####

   ## install 'webshot' package
   # library(devtools)
   # install_github("wch/webshot")


      library(htmlwidgets)
      library(webshot)
      library(mapview)

      observeEvent( input$maplink ,{

           showModal(modalDialog(
                title = "Important message",
                "saving map as map.png",
                easyClose = TRUE,
                footer = NULL
                ))

           mapshot(map(), file = paste0(getwd(), "/map.png"))

           # content = function(file) {
                # saveWidget(map(), file, selfcontained = TRUE)
           # }
      })

      # TODO incorporate solution here
      # https://stackoverflow.com/questions/31336898/how-to-save-leaflet-in-r-map-as-png-or-jpg-file
      # with new package library(mapview)

}


dhs_dictionary_Output = function(  input, output, session ){

     variables = filter(variable.doc, !is.na(Code) ) %>%
          select( Variable_category, Variable, Description, Code ) %>%
          dplyr::rename( `Code (R)` = Code)

     output$variables = renderDataTable( variables ,
                                         options = list(
                                              scrollX = TRUE ,
                                              scrollY = TRUE
                                         )
                                         )

}
