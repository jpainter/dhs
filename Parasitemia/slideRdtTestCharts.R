 d_psu=  
    d %>%
    filter( (country %in% 
              
                                         "Burkina Faso") 
            ) %>%
    group_by(country, survey_year, region, psu ) %>%
    summarise(
      n = sum(n),
      rdt = 100 * sum(rdt_pos) / n ,
      slide = 100* sum(slide_pos) / n 
              ) %>%
    as.data.frame() %>%
   gather(test, perc_pos, rdt, slide) %>%
    mutate( id = row_number(),
      survey_name = paste(country, survey_year)) %>%
   arrange(country, survey_year, region, psu, test)
 
     d_psu %>% 
      ggvis(~region, ~perc_pos) %>%
      # ungroup() %>%
      # layer_points( fill = ~test, key := ~id) %>%
      layer_boxplots(fill = ~test) %>%
      # add_tooltip( survey_label, "hover") %>%
      add_axis("x", title = "Region") %>%
      add_axis("y", title = "% Positive") 

library(gridExtra)
     a = d_psu %>% 
       mutate(region = factor(region)) %>%
      ggplot( aes(region, perc_pos)) +
      geom_boxplot(aes(fill = test)) +
       facet_grid(survey_year ~ .)
    
     b = d_psu %>% 
       mutate(region = factor(region) )%>%
       group_by(survey_year, region, test) %>%
       summarise(
         std = sd(perc_pos)
       ) %>%
      ggplot( aes(factor(region), std)) +
      geom_point(aes(color = test)) +
      facet_grid(survey_year ~ .)

grid.arrange(a,b, ncol=1)    
     
     
mtcars %>% ggvis(~cyl, ~mpg) %>% layer_boxplots()
# Setting width=0.5 makes it 0.5 wide in the data space, which is 1/4 of the
# distance between data values in this particular case.
mtcars %>% ggvis(~cyl, ~mpg) %>% layer_boxplots(width = 0.5)


library(dplyr)
base <- mtcars %>% ggvis(~mpg, ~cyl) %>% layer_points()
base %>% group_by(cyl) %>% summarise(mpg = mean(mpg)) %>%
  layer_points(fill := "red", size := 100)

base %>% filter(mpg > 25) %>% layer_points(fill := "red")

base %>% mutate(cyl = jitter(cyl)) %>% layer_points(fill := "red")

## Not run: 
# Dynamically restrict range using filter
mtcars %>% ggvis(~disp, ~mpg) %>%
   filter(cyl > eval(input_slider(0, 10))) %>%
   layer_points()

# Dynamically compute box-cox transformation with mutate
bc <- function(x, lambda) {
  if (abs(lambda) < 1e-6) log(x) else (x ^ lambda - 1) / lambda
}
bc_slider <- input_slider(-2, 2, 1, step = 0.1)
mtcars %>%
 ggvis(~disp, ~mpg) %>%
 mutate(disp = bc(disp, eval(bc_slider))) %>%
 layer_points()
